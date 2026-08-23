% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Precautionary Legitimacy Test for Climate Mitigation Technologies (Bounded-Reversibility Reading)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   A reversibility-and-bounded-worst-case criterion operates as the
 *   admission test for climate-mitigation technologies across climate
 *   finance, taxonomy regulation, procurement, and advocacy: a technology
 *   counts toward mitigation goals if and only if its worst-case failure
 *   modes and legacy burdens can be contained and undone within roughly one
 *   generation. The arrangement is enforced continuously — lending covenants,
 *   sustainability classifications, campaign pressure — and it redistributes
 *   materially: capital, procurement preference, and public license flow
 *   toward short-legacy generation, while long-legacy and catastrophic-tail
 *   technologies are excluded wholesale rather than case-by-case. This file
 *   instantiates the precautionary_reading of the
 *   technology_legitimacy_kernel and authors only that reading, with one
 *   stable epsilon whose referent is the standing arrangement itself as the
 *   precautionary lights assess it — never the arrangement this reading would
 *   prefer. The claimed_type and the metrics below are independent authored
 *   facts: the claim states my structural judgment; the metrics describe
 *   observed operation; the engine computes per-seat types from the
 *   structural data, and divergence between claim and computed output is
 *   signal, not error.
 *
 * KEY AGENTS:
 *   - climate_taxonomy_regulators: Primary agenda-setter (institutional/constrained) — codify and administer the admission test; control the label that unlocks subsidized capital
 *   - environmental_advocacy_networks: Enforcer-beneficiary (organized/mobile) — authored the criterion and police its boundaries across jurisdictions
 *   - renewable_energy_industry: Primary beneficiary (powerful/arbitrage) — receives displaced capital, mandates, and the legitimacy premium
 *   - nuclear_industry_and_skilled_workforce: Primary target (organized/identity_locked) — bears categorical exclusion; skills and community identity are reactor-bound
 *   - electricity_ratepayers: Diffuse payer, incidental beneficiary (moderate/constrained) — absorb integration costs and falling marginal costs alike
 *   - future_generations: Declared protectee and residual-cost bearer (powerless/trapped) — hold no seat in any reviewing room
 *   - developing_country_energy_planners: Excluded seat (moderate/constrained) — bound by finance conditionality they did not author
 *   - intergovernmental_risk_assessment_bodies: Analytical observer (institutional/analytical) — compile the risk evidence all sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.48).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.62).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Legitimacy Test for Climate Mitigation Technologies (Bounded-Reversibility Reading)").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, 'e2597829-bb7d-4571-b5ef-02dbb1fd6ed0').
narrative_ontology:cs_kernel_codification('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', formalized).
narrative_ontology:cs_authority_grounding('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', distributed).
narrative_ontology:cs_reading_relation('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', foundational, irreversibility_disqualifies_legitimacy).
narrative_ontology:cs_axiom_status(irreversibility_disqualifies_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', irreversibility_disqualifies_legitimacy, deontological).
narrative_ontology:cs_axiom('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', secondary, reversibility_preference_under_uncertainty).
narrative_ontology:cs_axiom_status(reversibility_preference_under_uncertainty, holdable).
narrative_ontology:cs_axiom_grounding('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', reversibility_preference_under_uncertainty, instrumental).
narrative_ontology:cs_reference_frame('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', worst_case_bounded_within_generation).
narrative_ontology:cs_drift_state('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', contemporary_taxonomy_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2597829-bb7d-4571-b5ef-02dbb1fd6ed0', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_energy_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, environmental_advocacy_networks).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_and_skilled_workforce).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, electricity_ratepayers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and maintain the official classification of which energy technologies count as sustainable for finance and procurement purposes, applying the reversibility test to candidate technologies and handling appeal procedures. They operate under treaty commitments, legislative mandates, and member-state pressure; changing a classification requires formal review steps they cannot skip. Their leverage comes from controlling the label that unlocks subsidized capital, and they bear the diplomatic cost whenever the label's boundaries are contested internationally.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, climate_taxonomy_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Originated and popularized the reversibility test through campaigns, divestment pressure, and consultation submissions, and monitor governments and lenders for departures from it, mobilizing members when the boundary is challenged. Membership growth, media salience, and grant income track the prominence of the issue they defined. They can move staff, framing, and campaigns across borders and topics at will; nothing pins them to this specific criterion except strategic choice.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, environmental_advocacy_networks, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, environmental_advocacy_networks, agenda_setter).

% Manufactures and develops wind, solar, storage, and grid equipment. Receives procurement mandates, concessional finance, and a legitimacy premium that competitors lacking the credential cannot access; books orders and shifts investment across jurisdictions wherever the credential travels. Decommissioning obligations exist on paper and increasingly in regulation, but the sector's balance-sheet exposure to them trails its deployment volume.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_energy_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Operates existing reactors, proposes new builds, and staffs a specialized labor pool — reactor physicists, licensed operators, waste engineers — whose skills have almost no market outside the technology. Plant-host towns depend on payroll and property tax; closure ends careers and hollows local civic life. The industry retains lobbying capacity and deep technical credibility, but re-entry into standing requires reversing the credential itself, not relocating, and the workforce cannot become something else without dissolving the professional identity it trained into.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_and_skilled_workforce, payer,
    organized, generational, identity_locked, global).

% Pay retail tariffs shaped by the generation mix the credential permits. Where firm low-carbon plants retire early they absorb integration, backup, and grid-expansion costs; where wind and solar dominate they capture falling marginal costs. They cannot opt out of grid dependence, lack a concentrated voice in classification reviews, and are represented episodically by consumer institutions rather than continuously.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, electricity_ratepayers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, electricity_ratepayers, beneficiary).

% Will inherit whatever wastes, contamination, committed warming, and locked-in infrastructure today's portfolio choices leave behind. They hold no vote, lawsuit, or contract in any current proceeding. The test is administered in their name: it shields them from newly manufactured irreversibilities, while they simultaneously bear the residual legacies that persist regardless of it and the opportunity costs of any mitigation the test slows. Nothing they do can alter the arrangement; everything the arrangement does alters their inheritance.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, future_generations, beneficiary).

% Plan electrification and industrialization under climate-finance conditionality shaped by donor-side classifications they did not write. Access to concessional capital depends on aligning project lists with the reversibility credential; deviation invites scrutiny, repricing, or withdrawal of finance. Their ministries sit outside the standard-setting bodies and participate in the forums where the credential is debated mainly as petitioners.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, developing_country_energy_planners, excluded,
    moderate, generational, constrained, national).

% Compile lifecycle-emissions, accident-frequency, and waste-stewardship evidence that every faction cites; publish scenario libraries and special reports on technology portfolios. They endorse no admission test, control no capital, and neither gain nor lose from which criterion prevails; their assessments feed all sides.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, intergovernmental_risk_assessment_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__precautionary_reading, renewable_energy_industry).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__precautionary_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives dispersed funders, regulators, insurers, and voters a shared admission rule for climate-relevant technologies, so that no single actor can commit the commons to a technology whose worst-case failure or legacy burden exceeds what one generation can contain; it converts an unmanageable portfolio problem — which bets may be imposed on everyone — into a checkable predicate.
% TRANSFER_FUNCTION: Moves investable capital, procurement preference, and public license from technologies carrying long-lived or catastrophic-tail legacies toward technologies whose failure modes and decommissioning close within roughly a generation; correspondingly moves the present costs of forgone firm capacity onto electricity consumers and the residual-carbon burden onto the global atmospheric commons.
% ABSENT_VOICES: Developing-country planners face the test as finance conditionality drafted elsewhere and sit outside the standard-setting rooms; geoengineering researchers are excluded categorically before evidence exists to evaluate; future generations — the test's nominal principals — appear nowhere in any reviewing room; reliability-first grid engineers participate mainly as petitioners against the prevailing classification. Each would contest either the criterion's content or its jurisdiction if seated.
% DISAPPEARANCE_RATIONALE: If the reversibility credential vanished overnight, taxonomy screens and lending covenants would drop, capital would return to nuclear life-extension and new-build proposals, advocacy campaigns would lose their organizing rule, and technology portfolios would re-sort around cost and speed; the specific pattern of winners and losers this arrangement produces is maintained by the credential's continuous application, so its absence would visibly reorganize climate finance and generation planning.
% FOUNDING_PROBLEM: Mid-century technological commitments produced waste streams and contamination whose remediation outlasted the institutions that created them, and climate mitigation raised the stakes: solving one civilization-scale problem must not manufacture new ones whose cleanup exceeds a generation's capacity. The arrangement was built to keep mitigation from becoming a new source of unrecoverable commitment.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: insurance markets continue pricing certain tail risks as uninsurable; national waste agencies publish stewardship obligations running tens of thousands of years; remediation ledgers at legacy-contamination sites document multi-decade, high-cost cleanups; and nuclear-safety regulators' own probabilistic analyses corroborate the tail-risk structure the test responds to — even where those same sources dispute the categorical, technology-level form the test takes. No attesting source denies that the underlying problem exists.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at mid-range (0.48 at interval end) because much of the transfer tracks genuine, externally corroborated risk differentiation — insurance markets price some tails as uninsurable and waste agencies publish multi-millennium stewardship obligations — while a real residue does not: categorical exclusion concentrates gains on the credentialed sector and concentrates harms on an identity-locked workforce and diffuse bill-payers beyond any risk differential. Suppression is moderately high (0.62) and unscaled-by-construction: the arrangement maintains itself through finance denial, classification rules, and boundary-policing discourse rather than physical coercion, and it resists rival decision rules, not merely rival technologies. Theater is low-moderate (0.30): the risk-assessment work is real, but reversibility is increasingly asserted ahead of demonstrated decommissioning and recycling capacity, so a growing share of activity performs boundedness rather than verifying it. Accessibility_collapse is low (0.35) because accepting the criterion collapses rival criteria only partially — the sibling readings remain live institutional positions. Resistance is substantial (0.60): an organized counter-coalition of industry, grid engineers, and developing-country blocs contests the criterion continuously. All three measurement series run on one shared seven-point grid (t=0..30); the trajectories show enforcement hardening (post-accident classification tightening), extraction accumulation as the credentialed sector scaled into the displaced capital, and theater growth as reversibility claims outpaced material capability. Base_properties reflect end-state values (t=30).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because exit differs sharply. From the regulator seat the criterion is prudential portfolio governance; from the credentialed industry seat it is a moat worth defending; from the identity-locked nuclear seat it is categorical exclusion of a vocation and a way of town life, with no relocation that preserves the identity; from the ratepayer seat it is a diffuse line item; from the future-generations seat it is a promise made on their behalf by parties who also collect from administering it. Same predicate, different experienced types — the engine derives this divergence from power, exit, and role declarations rather than from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (renewable industry, advocacy networks) place those seats near d=0: the arrangement subsidizes them and their exit is arbitrage-grade or mobile. Victim declarations place nuclear industry and workforce near the full-target pole, amplified by identity-lock and generational time horizons; ratepayers land between poles, damped by their secondary beneficiary position (falling marginal costs, avoided local accident risk). Future generations are declared victims — they inherit residual irreversibilities and any delay costs — but they are also the arrangement's intended protectees, a dual position the naive derivation (victim + trapped + powerless => near-full target) misses; a directionality override on the powerless atom (d=0.6) encodes the moderation. No other override is needed: every remaining seat's derived d follows from its declared role and exit.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading this arrangement as pure coordination would erase the concentrated, identity-locked payers whose exclusion is categorical rather than risk-proportioned; reading it as pure extraction would erase the tail-risk coordination that parties outside the beneficiary set — insurers, waste agencies, safety regulators — corroborate as real and unresolved. The tangled_rope claim holds both halves in view and locates the open question where it belongs: in application uniformity and demonstrated reversibility (see omegas), not in the presence or absence of a coordination function. Mandatrophy is not resolved: the founding problem — containing legacies that outlast the institutions creating them — remains live per corroborating sources outside the beneficiary set, so the classification does not rest on vestigial maintenance or theatrical persistence; the theater that exists is symptom-level, attached to specific reversibility claims rather than to the arrangement's core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_property_disagreement,
    'Which property does the underlying technology-legitimacy kernel actually turn on — bounded generation-scale reversibility (this reading), dispatchable baseload capability, or deployment velocity within the remaining carbon budget — and is the disagreement empirical, conceptual, or preference-based?',
    'Adjudication records: taxonomy revision outcomes, integrated-assessment-model portfolio comparisons under each criterion held to the same carbon budget, and litigation over classification exclusions.',
    'Resolution toward velocity restructures the beneficiary set (firm low-carbon technologies enter, some renewables'' intermittency becomes disqualifying); resolution toward reliability admits nuclear; persistence of the contest leaves the three readings as parallel constraints with divergent victim sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_property_disagreement, conceptual, 'Committer-frame omega: this constraint is one reading of technology_legitimacy_kernel; the located disagreement is which single property the legitimacy biconditional keys on.').

omega_variable(
    criterion_application_uniformity,
    'Is the reversibility predicate applied uniformly across all candidate technologies, or selectively — with some technologies carrying unbounded atmospheric legacies admitted while others are excluded categorically?',
    'Comparative audit: score every technology decision in major taxonomies and finance screens against the formal predicate (worst-case boundedness, legacy reversibility within a generation), independent of the deciding institution''s stated rationale.',
    'Selective application indicates rent-seeking riding on the criterion, raising effective extraction substantially and pushing the classification toward pure extraction with identifiable capturers; uniform application confirms the asymmetric transfer tracks genuine risk differentiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(criterion_application_uniformity, empirical, 'Whether the standing arrangement enforces the criterion it declares, or enforces a selective version of it.').

omega_variable(
    reversibility_demonstration_gap,
    'Are the legacy costs of technologies passing the test actually reversible within a generation — panel, battery, and blade material flows closed at scale, sites restorable — or is reversibility asserted faster than decommissioning capacity is built?',
    'Material-flow accounting: end-of-life collection and recycling rates versus deployment volumes, decommissioning bond adequacy, and demonstrated full-site restoration cases.',
    'If irreversibility is being migrated rather than eliminated, the criterion functions partly as risk-shuffling, base extractiveness rises, and the beneficiary composition of the arrangement requires revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_demonstration_gap, empirical, 'Demonstrated-versus-asserted reversibility for technologies the test admits.').

omega_variable(
    forgone_firm_capacity_cost,
    'What does categorical exclusion of firm low-carbon generation cost in cumulative emissions and system expense, compared with case-by-case licensing under the same reversibility scrutiny applied plant-by-plant?',
    'Integrated-assessment ensembles holding the carbon budget constant across portfolios differing only in firm-capacity availability, plus grid-operator cost studies in jurisdictions that reversed early closures.',
    'A sizable penalty loads additional extraction onto the future-generations seat and sharpens the payer-side reading; a negligible penalty strengthens the protective framing and lowers the measured asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forgone_firm_capacity_cost, empirical, 'Counterfactual climate and ratepayer cost of the categorical form of the exclusion.').

omega_variable(
    suppression_mechanism_structure,
    'Is the suppressed legitimacy of excluded technologies structural (lending covenants, classification rules, licensing posture) or discursive-internalized (risk perception that persists after evidence updates and would survive removal of the structural bars)?',
    'Natural experiment: jurisdictions where structural bars were lifted (licensing restarted, finance re-admitted) tracked for whether capital and public acceptance actually returned, versus jurisdictions where bars persist.',
    'If suppression is substantially internalized-discursive, it outlives enforcement removal and exit stays closed regardless of rule change, raising the durable suppression estimate; if structural, classification reform is sufficient to reopen the option set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structure, empirical, 'Structural versus internalized component of the enforcement suppressing the excluded alternative.').

omega_variable(
    future_generations_representation,
    'Who authorizes and verifies this criterion on behalf of its nominal principals, given that no proxy institution with binding voice represents future generations in any reviewing room?',
    'Institutional observation: whether ombudsperson offices, youth-delegate mechanisms, or future-generations commissions acquire standing with decision rights in taxonomy and finance-screen reviews, and whether their interventions alter outcomes.',
    'Absent binding representation, the victim declaration rests on third-party assertion and the protective intent of the arrangement is unverifiable by the party it names; acquired representation would convert the arrangement''s central claim into a testable accountability relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_representation, conceptual, 'Representation deficit of the seat the criterion exists to protect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(tech_tr_t30, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(tech_be_t30, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 5, 0.49).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(tech_su_t30, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, identity_coordination).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% Family decomposition per the epsilon-invariance principle: the colloquial label 'legitimate climate technology' conflates three structurally distinct admission tests — bounded generational reversibility (this file), dispatchable baseload capability (reliability_primacy_reading), and deployment speed within the carbon-budget timeline (velocity_primacy_reading). Each reading yields its own epsilon, beneficiary set, and victim set — this reading admits renewables and excludes long-legacy firm generation; the reliability reading reverses the nuclear position; the velocity reading re-ranks everything by deployability. They are linked as one kernel family: whichever reading governs a taxonomy or finance screen structurally conditions the resource environment of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
