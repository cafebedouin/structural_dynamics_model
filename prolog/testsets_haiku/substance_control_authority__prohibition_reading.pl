% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Substance Prohibition Authority (Third-Party Protection Framing)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading frames state criminalization of drug use and
 *   possession as a valid exercise of authority to protect third
 *   parties—neighborhoods, families, and non-users—from spillover harms of
 *   drug markets and drug-related crime. This is ONE reading of a contested
 *   kernel (substance_control_authority). The reading asserts that
 *   criminalization deters use, reduces public disorder, and justifies high
 *   enforcement costs. The rival readings (harm_reduction_reading,
 *   legalization_reading) dispute this: they reframe drug use as a public
 *   health issue, argue criminalization increases harm and cost, and advocate
 *   treatment/regulated-access models instead. This story instantiates the
 *   prohibition frame only—not the alternative frames. Its ε=0.82 and
 *   suppression=0.91 reflect the reading's structural reality:
 *   criminalization is highly extractive from drug users and minorities (who
 *   bear criminal penalty disproportionately) and highly suppressive
 *   (criminalization forecloses alternatives and identity-locks marginalized
 *   populations). The authorized beneficiaries are neighborhoods and the
 *   crime-prevention constituency; the victims are criminalized users and
 *   racially disproportionately-enforced populations. The measurement series
 *   show extraction and suppression rising over 40 years and plateauing, with
 *   theater rising as enforcement becomes more procedurally elaborate while
 *   maintaining deterrent effects.
 *
 * KEY AGENTS:
 *   - State authority apparatus (institutional beneficiary/agenda-setter): sets policy, administers enforcement, collects authority and budgetary control
 *   - Drug users criminalized (powerless victims): bear criminal records, incarceration, health harm, employment loss
 *   - Racial minorities disproportionately enforced (moderate power, identity-locked victims): experience 3-10x enforcement rates, de facto racialized criminalization
 *   - Neighborhoods protected from disorder (organized beneficiary): receive deterrent effect concentrated in affluent areas
 *   - Crime-prevention constituency (organized beneficiary): derives political position and symbolic confirmation from criminalization
 *   - Criminal enforcement infrastructure (institutional agenda-setter/beneficiary): derives funding, importance, career structures from enforcement
 *   - Public health alternatives excluded (moderate power, trapped, excluded): treatment and harm-reduction voices structurally excluded from authority
 *   - Observers/research communities (analytical): document constraint's actual effects and comparison data from alternative jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.82).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.91).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Substance Prohibition Authority (Third-Party Protection Framing)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '996a1d1a-8b38-4e76-a0da-2ceac38a61c5').
narrative_ontology:cs_kernel_codification('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', formalized).
narrative_ontology:cs_authority_grounding('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', extraction).
narrative_ontology:cs_interpretation_layer_present('996a1d1a-8b38-4e76-a0da-2ceac38a61c5').
narrative_ontology:cs_reading_relation('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', foundational, deterrence_centrality).
narrative_ontology:cs_axiom_status(deterrence_centrality, holdable).
narrative_ontology:cs_axiom_grounding('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', deterrence_centrality, empirically_contingent).
narrative_ontology:cs_axiom('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', secondary, third_party_protection_justification).
narrative_ontology:cs_axiom_status(third_party_protection_justification, holdable).
narrative_ontology:cs_axiom_grounding('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', third_party_protection_justification, deontological).
narrative_ontology:cs_reference_frame('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', deterrence_based_social_order).
narrative_ontology:cs_drift_state('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', contemporary_research_era_2000_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('996a1d1a-8b38-4e76-a0da-2ceac38a61c5', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, neighborhoods_protected_from_disorder).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, crime_prevention_constituency).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users_criminalized).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, racial_minorities_disproportionately_enforced).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, criminal_enforcement_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces criminalization policy; administers detection, prosecution, incarceration infrastructure. Justifies the constraint as protecting third parties (neighborhoods, non-users) from drug-related crime, public disorder, and health spillovers. The apparatus collects political authority, budgetary control, and operational discretion from enforcement.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_authority_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the primary cost of criminalization: criminal records, incarceration, health harm from criminalized supply, loss of employment and housing eligibility, family separation. Their choice set collapses to abstinence (enforced by criminal penalty) or criminality. Exit from the jurisdiction does not resolve the constraint because criminalization follows across state and international borders (extradition, deportation).
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users_criminalized, payer,
    powerless, biographical, trapped, national).

% Experience enforcement rates 3-10x higher than demographically similar non-minority populations for equivalent conduct. The constraint's stated logic (third-party protection) is applied selectively: enforcement patterns concentrate in neighborhoods with high minority population density, creating de facto racialized criminalization. Identity-locked because disproportionate enforcement traces to racial classification, not conduct choice.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, racial_minorities_disproportionately_enforced, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, racial_minorities_disproportionately_enforced, excluded).

% Receive deterrent effect from criminalization: visible open-air drug markets are reduced in affluent/politically organized neighborhoods through enforcement concentration. They benefit from the constraint's deterrent function applied selectively to poor and minority neighborhoods, while their own areas remain safer.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, neighborhoods_protected_from_disorder, beneficiary,
    organized, generational, constrained, regional).

% Comprises political coalitions (law-and-order advocacy, some public-safety groups, insurance and business associations) that frame drug criminalization as necessary for social order. They benefit from the constraint's symbolic confirmation of state capacity and their political position; their material benefit is diffuse but politically salient.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, crime_prevention_constituency, beneficiary,
    organized, generational, mobile, national).

% Police, prosecutors, and incarceration apparatus derive operational funding, institutional importance, and career structures from enforcement volume. The constraint justifies budget allocation, staffing, and authority expansion. Enforcement infrastructure has structural incentive to sustain and amplify the constraint regardless of deterrent efficacy.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, criminal_enforcement_infrastructure, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, criminal_enforcement_infrastructure, beneficiary).

% Treatment providers, harm-reduction advocates, and public-health researchers arguing for decriminalization or regulated access are structurally excluded from authoritative policy-setting; their evidence and recommendations are treated as politically illegitimate within the prohibition framework. Their exclusion is maintained by the state authority's monopoly on policy legitimacy.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_alternatives_excluded, excluded,
    moderate, biographical, trapped, national).

% International research communities (epidemiology, criminology, public health) document the constraint's actual effects: incarceration outcomes, overdose rates, recidivism, racial disparities, and comparison data from jurisdictions with harm-reduction or legalization frameworks. They take no position within the constraint but provide data external parties use to contest the prohibition frame.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, observers_public_health_research, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, state_authority_apparatus).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Deters drug use by attaching criminal penalty to possession and use, thereby reducing drug availability in regulated (affluent/organized) neighborhoods and signaling state commitment to social order.
% TRANSFER_FUNCTION: Moves agency, resources, and freedom from drug-using populations (especially racial minorities) to the criminal enforcement apparatus and to neighborhoods politically organized to demand enforcement. Transfers incarceration risk onto marginalized populations as the price of neighborhood order.
% ABSENT_VOICES: Public health practitioners, people with lived experience of addiction, harm-reduction advocates, and jurisdictions that have decriminalized are structurally excluded from authoritative voice in the policy formation. Their testimony and evidence are treated as illegitimate within the prohibition frame.
% DISAPPEARANCE_RATIONALE: If criminalization vanished, the entire enforcement apparatus (police narcotics units, drug courts, incarceration infrastructure) would reorganize; drug-related behavior would shift from criminal pathway to public-health pathway; incarcerated populations would be released; neighborhoods would experience either increased visible drug use (in the short term) or stabilization at lower levels if treatment access expanded (medium term); state budgets and law-enforcement authority would contract.
% FOUNDING_PROBLEM: Drug use produces spillover harms to non-users: drug-related crime, disrupted neighborhoods, disease transmission, and children exposed to drug markets. Criminalization is the state's response to protect third parties from these harms.
% FOUNDING_PROBLEM_CORROBORATION: The state authority and crime-prevention constituencies attest the founding problem remains acute and criminalization is necessary. Public health researchers and decriminalization advocates dispute this: they present data showing criminalization does not reduce drug use, increases overdose mortality (by criminalizing the supply), and produces worse health outcomes than treatment-focused alternatives. Jurisdictions that decriminalized (Portugal, Switzerland, parts of Canada) published outcome data showing drug-use prevalence unchanged or reduced while harm measures (overdose, incarceration, health costs) improved — this external corroboration comes from outside the prohibition-reading beneficiary set.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.68 to 0.82 over 40 years because enforcement infrastructure expands and incarceration becomes the primary mechanism, layering more costs onto users without proportional reduction in drug use. The constraint is classified as tangled_rope (not snare) because genuine coordination benefit exists (third-party protection through deterrence) alongside asymmetric extraction (users bear criminal penalty). Suppression is extremely high (0.91) because criminalization operates via legal force backed by incarceration capacity—resistance is punished directly. Theater rises from 0.32 to 0.48 as enforcement becomes increasingly procedurally elaborate (drug courts, treatment mandates, community supervision) while maintaining core extractive function. The coercion grid reveals level-dependent dynamics: individual-level accessibility collapse (0.88-0.91) is near-maximal because criminalization forecloses drug use entirely; organizational-level collapse (0.72-0.75) is lower because harm-reduction organizations and some jurisdictions maintain alternative framings; class-level resistance (0.71-0.75 at t40) shows increasing organized pushback from decriminalization movements; structural-level suppression (0.85-0.88) reflects state capacity to enforce across jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   The state authority/crime-prevention seat experiences this as protective coordination—reducing visible drug markets in their neighborhoods, confirming state effectiveness. The criminalized-user and minority-enforcement seats experience the same structure as asymmetric punishment: their conduct triggers legal force with incarceration consequences, while beneficiary neighborhoods benefit from enforcement concentration. The engine computes different directionality (d) for each seat: state apparatus and organized constituencies d ≈ 0.1-0.2 (beneficiaries); criminalized users d ≈ 0.85-0.95 (targets); minorities d ≈ 0.8-0.9 (targets, higher because disproportionality amplifies target status). These divergences arise from the structural data: who collects enforcement discretion (agenda_setter), who bears criminal penalty (payer), who has exit (trapped vs. mobile), and whether enforcement is selective by race (identity_locked).
 *
 * DIRECTIONALITY LOGIC:
 *   Drug users bear the constraint's direct cost (criminalization, incarceration, health harm from criminalized supply) with no exit except abstinence (enforcement-backed) or jurisdiction-flight (ineffective because criminalization is national). Racial minorities experience the same constraint applied at 3-10x rates for equivalent conduct: their encounter rate with enforcement is higher because enforcement concentrates in their neighborhoods. This race-specific disproportionality operates at the level of identity—race cannot be exited—making their directionality d ≈ 0.9 (full-target end). Beneficiary neighborhoods receive deterrent benefit concentrated in affluent areas; they have exit via mobility (moving to safer neighborhoods does not require leaving the constraint, since they are the beneficiary seat) and arbitrage (they capture the benefit without bearing costs). The state apparatus has arbitrage exit: it administers the constraint rather than bears it. Organized crime-prevention constituencies have mobile exit: they can shift political allegiance if the constraint fails or gets remedied. These asymmetries compound: powerless + trapped + identity-locked users sit at d≈0.90; powerful + mobile beneficiary neighborhoods sit at d≈0.15; institutional state apparatus sits at d≈0.05 (full beneficiary end).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (drug-related spillover harms to third parties) and the disappearance verdict (world_rearranges) align on 'live and important.' The tangled_rope classification prevents misreading this as pure snare: there IS genuine coordination benefit (neighborhoods do experience reduced visible disorder under sustained criminalization). But the constraint meets all tangled_rope structural requirements: (1) coordination function (deterrence) AND (2) asymmetric extraction (users pay criminal penalty; neighborhoods benefit). The mandatrophy risk lies in the contested founding_problem_status. External observers (research communities, decriminalized jurisdictions) attest the founding problem status is 'dead'—drug use prevalence unchanged or reduced under treatment-based alternatives, while overdose, health, and incarceration outcomes worsen under criminalization. If the founding problem is dead but the arrangement persists, the constraint becomes zombie-functional (Goodhart drift: enforcement infrastructure maintains the constraint for its own institutional survival, not because it solves the founding problem). The theater_ratio rising from 0.32 to 0.48 signals increasing proxy-goal replacement: drug courts and treatment mandates are performed procedurally but serve incarceration infrastructure, not genuine drug-use reduction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_uncertainty,
    'Does criminalization actually deter drug use at the population level, or does it primarily redistribute use patterns while increasing harm from criminalized supply?',
    'Large-scale natural experiments (jurisdictions that decriminalized vs. maintained criminalization) with 5-10 year outcome tracking: drug use prevalence, overdose mortality, incarceration, health outcomes, recidivism.',
    'If deterrence is real and substantial: criminalization''s coordination benefit justifies the tangled_rope classification and high extraction metrics. If deterrence is minimal or offset by increased harm: the constraint reclassifies as snare (extraction with cover story); theater_ratio rises toward 0.7+; the ''third-party protection'' benefit becomes theater (procedural without function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_uncertainty, empirical, 'Whether criminal penalty produces genuine behavioral deterrence or merely redistributes conduct while increasing harm.').

omega_variable(
    racialized_enforcement_structural_vs_discretionary,
    'Are racial disparities in criminalization enforcement structural (law written to target minorities) or discretionary (neutral law applied selectively)?',
    'Statistical analysis of enforcement rates controlling for conduct (drug use prevalence surveys vs. arrest rates by race, neighborhood); audit of prosecutor and police charging practices; legislative analysis of sentencing rules.',
    'If structural (law targets minorities explicitly): the constraint''s racial disparity is a design feature, not a side effect; victim set is formally racialized; the ''third-party protection'' reading applies selectively by race (falsifying the universality claim). If discretionary (neutral law, selective enforcement): the constraint''s design is not racialized but implementation is; the remedy is enforcement equity rather than decriminalization, though equity is structurally difficult to achieve under criminalization incentives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racialized_enforcement_structural_vs_discretionary, empirical, 'Whether racial disparities in criminalization are structural features or discretionary application of neutral law.').

omega_variable(
    third_party_benefit_localization,
    'Is third-party protection (reduced visible disorder in neighborhoods) distributed equally across neighborhoods, or concentrated in affluent/politically organized areas?',
    'Mapping of enforcement intensity by neighborhood, controlling for drug use prevalence (survey data); comparison of enforcement ratios in high-use wealthy areas vs. high-use poor areas; analysis of neighborhood-level advocacy and political power.',
    'If distributed equally: third-party protection is genuine benefit applied universally; tangled_rope remains appropriate. If concentrated in wealthy areas: the beneficiary seat is not ''neighborhoods'' generically but wealthy/organized neighborhoods specifically; poor neighborhoods bear enforcement costs without proportional benefit (neighborhood becomes secondary victim rather than beneficiary); the constraint reclassifies toward snare for the poor-neighborhood seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(third_party_benefit_localization, empirical, 'Whether enforcement-driven third-party protection is universally distributed or concentrated by wealth/political power.').

omega_variable(
    kernel_reading_foreclosure_risk,
    'Could the harm-reduction or legalization readings logically foreclose the prohibition reading within a single authoritative framework, or do they genuinely coexist as distinct institutional choice?',
    'Doctrinal analysis of whether the readings'' foundational axioms (deterrence_centrality vs. criminalization_harm_amplification vs. market_regulation_optimality) can be held simultaneously within a coherent framework, or whether adopting one axiom commits a policy actor to rejecting the others.',
    'If foreclosure exists (one reading logically eliminates another): the relation changes from coexists_with to forecloses, and the kernel exhibits true logical conflict rather than institutional pluralism. If no foreclosure (all three can be held by different coherent policies): the readings genuinely coexist, and policy divergence reflects institutional choice, not truth-value resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_risk, conceptual, 'Whether the three substance-control readings logically foreclose each other or can coexist as distinct institutional commitments.').

omega_variable(
    suppression_internalization_interpersonal_level,
    'Is the high suppression (0.91) measured at the individual level (criminalized users accepting prohibition as inevitable) structural (legal/police force) or internalized (users have accepted the criminal classification as legitimate)?',
    'Post-decriminalization trajectory: if users criminalized under prohibition show rapid behavioral shift toward public-health engagement (treatment uptake, stigma reduction) after decriminalization, suppression was primarily structural; if stigma and behavioral inhibition persist, suppression is partially internalized.',
    'If structural: decriminalization + treatment infrastructure would rapidly reduce suppression and resistance. If internalized: the constraint''s suppressive effect would persist even after formal criminalization ends; long-term de-stigmatization and alternative socialization would be required alongside decriminalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_interpersonal_level, empirical, 'Whether measured suppression of drug users reflects legal/police force or internalized acceptance of criminal classification.').

omega_variable(
    enforcement_budget_dependency_path_lock,
    'Does the enforcement apparatus'' institutional dependence on criminalization create irreversible path-lock, or can law-enforcement organizations successfully transition to alternative public-health partnerships?',
    'Case studies of jurisdictions that decriminalized or reduced enforcement: did police organizations transition to collaborative public-health roles, or did they resist decriminalization or defect from harm-reduction partnerships?',
    'If path-locked: enforcement apparatus will resist decriminalization and undermine alternatives structurally; remedy requires parallel investment in competing public-health infrastructure rather than mere criminalization removal. If transition-capable: decriminalization + retraining can redirect enforcement resources toward collaborative safety roles compatible with harm reduction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_budget_dependency_path_lock, empirical, 'Whether criminal enforcement infrastructure is structurally locked into criminalization or can transition to alternative public-safety models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t5, substance_control_authority__prohibition_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__prohibition_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_authority__prohibition_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__prohibition_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_authority__prohibition_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(subs_tr_t25, observed).
narrative_ontology:measurement(subs_tr_t30, substance_control_authority__prohibition_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(subs_tr_t30, observed).
narrative_ontology:measurement(subs_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(subs_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t5, substance_control_authority__prohibition_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(subs_be_t5, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__prohibition_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_authority__prohibition_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__prohibition_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_authority__prohibition_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(subs_be_t25, observed).
narrative_ontology:measurement(subs_be_t30, substance_control_authority__prohibition_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(subs_be_t30, observed).
narrative_ontology:measurement(subs_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(subs_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t5, substance_control_authority__prohibition_reading, suppression_requirement, 5, 0.87).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__prohibition_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_authority__prohibition_reading, suppression_requirement, 15, 0.89).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__prohibition_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_authority__prohibition_reading, suppression_requirement, 25, 0.91).
narrative_ontology:measurement_basis(subs_su_t25, observed).
narrative_ontology:measurement(subs_su_t30, substance_control_authority__prohibition_reading, suppression_requirement, 30, 0.91).
narrative_ontology:measurement_basis(subs_su_t30, observed).
narrative_ontology:measurement(subs_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.91).
narrative_ontology:measurement_basis(subs_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(subs_grid_01, substance_control_authority__prohibition_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(subs_grid_02, substance_control_authority__prohibition_reading, accessibility_collapse(class), 40, 0.71).
narrative_ontology:measurement(subs_grid_03, substance_control_authority__prohibition_reading, accessibility_collapse(individual), 0, 0.88).
narrative_ontology:measurement(subs_grid_04, substance_control_authority__prohibition_reading, accessibility_collapse(individual), 40, 0.91).
narrative_ontology:measurement(subs_grid_05, substance_control_authority__prohibition_reading, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(subs_grid_06, substance_control_authority__prohibition_reading, accessibility_collapse(organizational), 40, 0.75).
narrative_ontology:measurement(subs_grid_07, substance_control_authority__prohibition_reading, accessibility_collapse(structural), 0, 0.82).
narrative_ontology:measurement(subs_grid_08, substance_control_authority__prohibition_reading, accessibility_collapse(structural), 40, 0.85).
narrative_ontology:measurement(subs_grid_09, substance_control_authority__prohibition_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(subs_grid_10, substance_control_authority__prohibition_reading, resistance(class), 40, 0.75).
narrative_ontology:measurement(subs_grid_11, substance_control_authority__prohibition_reading, resistance(individual), 0, 0.45).
narrative_ontology:measurement(subs_grid_12, substance_control_authority__prohibition_reading, resistance(individual), 40, 0.48).
narrative_ontology:measurement(subs_grid_13, substance_control_authority__prohibition_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(subs_grid_14, substance_control_authority__prohibition_reading, resistance(organizational), 40, 0.68).
narrative_ontology:measurement(subs_grid_15, substance_control_authority__prohibition_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(subs_grid_16, substance_control_authority__prohibition_reading, resistance(structural), 40, 0.63).
narrative_ontology:measurement(subs_grid_17, substance_control_authority__prohibition_reading, stakes_inflation(class), 0, 0.71).
narrative_ontology:measurement(subs_grid_18, substance_control_authority__prohibition_reading, stakes_inflation(class), 40, 0.74).
narrative_ontology:measurement(subs_grid_19, substance_control_authority__prohibition_reading, stakes_inflation(individual), 0, 0.84).
narrative_ontology:measurement(subs_grid_20, substance_control_authority__prohibition_reading, stakes_inflation(individual), 40, 0.87).
narrative_ontology:measurement(subs_grid_21, substance_control_authority__prohibition_reading, stakes_inflation(organizational), 0, 0.65).
narrative_ontology:measurement(subs_grid_22, substance_control_authority__prohibition_reading, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(subs_grid_23, substance_control_authority__prohibition_reading, stakes_inflation(structural), 0, 0.79).
narrative_ontology:measurement(subs_grid_24, substance_control_authority__prohibition_reading, stakes_inflation(structural), 40, 0.81).
narrative_ontology:measurement(subs_grid_25, substance_control_authority__prohibition_reading, suppression(class), 0, 0.78).
narrative_ontology:measurement(subs_grid_26, substance_control_authority__prohibition_reading, suppression(class), 40, 0.81).
narrative_ontology:measurement(subs_grid_27, substance_control_authority__prohibition_reading, suppression(individual), 0, 0.89).
narrative_ontology:measurement(subs_grid_28, substance_control_authority__prohibition_reading, suppression(individual), 40, 0.92).
narrative_ontology:measurement(subs_grid_29, substance_control_authority__prohibition_reading, suppression(organizational), 0, 0.81).
narrative_ontology:measurement(subs_grid_30, substance_control_authority__prohibition_reading, suppression(organizational), 40, 0.83).
narrative_ontology:measurement(subs_grid_31, substance_control_authority__prohibition_reading, suppression(structural), 0, 0.85).
narrative_ontology:measurement(subs_grid_32, substance_control_authority__prohibition_reading, suppression(structural), 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__prohibition_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested substance_control_authority kernel. The harm_reduction and legalization readings are sibling constraints instantiating the same kernel under different policy frames. All three stories share the same referent (state response to drug use) but author different ε values because the readings differ in what constitutes extraction: prohibition reading counts criminalization as justified third-party protection (lower ε factoring in coordination benefit); harm-reduction reading counts criminalization itself as extraction (higher ε). The three stories are linked via network.affects_constraints to enable comparison of how the same policy domain produces different constraints depending on reading frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, powerless, 0.92).
constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, moderate, 0.88).
constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, organized, 0.18).
constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
