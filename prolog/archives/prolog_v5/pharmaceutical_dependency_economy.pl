% ============================================================================
% CONSTRAINT STORY: pharmaceutical_dependency_economy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_dependency_economy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pharmaceutical_dependency_economy
 *   human_readable: Pharmaceutical Dependency Economy
 *   domain: healthcare/pharmaceutical_policy/economic_extraction
 *
 * SUMMARY:
 *   The pharmaceutical dependency economy creates a structural coupling
 *   between biological need (chronic disease management requiring continuous
 *   medication), institutional control (patent law, regulatory approval,
 *   manufacturing complexity), and economic extraction (monopoly pricing,
 *   evergreening strategies, supply chain bottlenecks). This constraint
 *   exhibits Tangled Rope characteristics: genuine coordination function
 *   (bringing drugs from development to patient) coexists with asymmetric
 *   extraction (manufacturers capture rents from patients who have no exit
 *   options). The constraint's extractiveness has increased over the
 *   measurement interval (0.42 → 0.58) as patent evergreening strategies have
 *   extended monopoly periods and as manufacturing complexity creates
 *   barriers to generic entry. Theater ratio (0.55) reflects that regulatory
 *   approval processes maintain significant performative content — complexity
 *   that serves barrier-to-entry functions more than safety validation for
 *   mature drug classes. The analytical observer risks naturalizing
 *   contingent policy choices (patent length, exclusivity periods, regulatory
 *   design) as immutable requirements of pharmaceutical innovation. The
 *   constraint is simultaneously solving a real problem (funding drug R&D,
 *   ensuring safety, coordinating complex manufacturing) and extracting rents
 *   from trapped agents (chronic patients with no alternatives).
 *
 * KEY AGENTS:
 *   - Chronic Disease Patients: Primary victims (powerless/trapped) — biologically dependent on continuous medication with no exit options; bear full cost of monopoly pricing
 *   - Low-Income Populations: Secondary victims (powerless/constrained to trapped) — medication adherence reduces with cost barriers; face health consequences from treatment abandonment
 *   - Healthcare Systems: Mixed position (moderate/constrained) — coordinate drug access while experiencing extraction through price inflation; constrained by patent law and regulatory requirements
 *   - Pharmaceutical Manufacturers: Primary beneficiaries (institutional/arbitrage) — experience constraint as beneficial coordination enabling R&D recovery and innovation funding; have arbitrage options (therapeutic area shifts, market selection)
 *   - Generic Drug Manufacturers: Secondary position (powerful/constrained) — enabled by regulatory bioequivalence pathways but excluded by patent enforcement during exclusivity periods
 *   - Patent Reform Coalition: Organized agents (organized/constrained) — patient advocacy groups, international health organizations, some governments perceiving pharmaceutical dependency as temporary arrangement with sunset clause
 *   - Regulatory Agencies (FDA/EMA): Institutional actors (institutional/arbitrage) — approve drugs and set standards; maintain performative approval complexity that protects incumbents
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy as immutable natural law of pharmaceutical economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_dependency_economy, 0.58).
domain_priors:suppression_score(pharmaceutical_dependency_economy, 0.62).
domain_priors:theater_ratio(pharmaceutical_dependency_economy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_dependency_economy, extractiveness, 0.58).
narrative_ontology:constraint_metric(pharmaceutical_dependency_economy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(pharmaceutical_dependency_economy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_dependency_economy, tangled_rope).
narrative_ontology:human_readable(pharmaceutical_dependency_economy, "Pharmaceutical Dependency Economy").
narrative_ontology:topic_domain(pharmaceutical_dependency_economy, "healthcare/pharmaceutical_policy/economic_extraction").

domain_priors:requires_active_enforcement(pharmaceutical_dependency_economy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_dependency_economy, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_dependency_economy, patent_holders).
narrative_ontology:constraint_beneficiary(pharmaceutical_dependency_economy, pharmacy_benefit_managers).
narrative_ontology:constraint_beneficiary(pharmaceutical_dependency_economy, institutional_investors).
narrative_ontology:constraint_victim(pharmaceutical_dependency_economy, chronic_disease_patients).
narrative_ontology:constraint_victim(pharmaceutical_dependency_economy, low_income_populations).
narrative_ontology:constraint_victim(pharmaceutical_dependency_economy, healthcare_systems).
narrative_ontology:constraint_victim(pharmaceutical_dependency_economy, generic_drug_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHRONIC PATIENT (SNARE) — Biologically dependent on continuous medication; faces structural entrapment through patent monopolies, lack of affordable alternatives, and medical necessity. Cannot exit the constraint without risking health deterioration. Maximum experienced extraction — the patient bears full cost of monopoly pricing with no genuine alternatives.
constraint_indexing:constraint_classification(pharmaceutical_dependency_economy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HEALTHCARE SYSTEM (TANGLED ROPE) — Coordinates drug access and patient care through pharmaceutical supply chains while experiencing significant extraction via price inflation. Constrained by patent law, regulatory requirements, and dependency on branded drugs during patent periods. Mixed experience: genuine coordination function (ensuring drug availability) alongside asymmetric extraction (manufacturers control prices).
constraint_indexing:constraint_classification(pharmaceutical_dependency_economy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Experiences the constraint as beneficial coordination: patent protection enables recovery of R&D costs and funds future drug development. Net beneficiary with arbitrage options (can shift to other therapeutic areas, negotiate pricing). Experiences constraint as coordination mechanism that solves the innovation funding problem.
constraint_indexing:constraint_classification(pharmaceutical_dependency_economy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GENERIC DRUG MANUFACTURER (TANGLED ROPE) — Coordinated through regulatory approval pathways for bioequivalence testing, enabling affordable alternatives. Simultaneously extracted from: patent law excludes them during exclusivity periods, regulatory complexity creates barriers to market entry. Mixed extraction and coordination — some agents benefit from enabling access, others are locked out by patent enforcement.
constraint_indexing:constraint_classification(pharmaceutical_dependency_economy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PATENT REFORM COALITION (SCAFFOLD) — Organized agents (patient advocacy groups, international health organizations, some governments) perceive the pharmaceutical dependency economy as a temporary institutional arrangement with a sunset clause. Patent extensions (evergreening), regulatory exclusivity periods, and bilateral trade agreements all have defined time horizons. Coalition strategy: accelerate generic uptake, renegotiate TRIPS provisions, implement compulsory licensing frameworks. Low effective extraction because the coalition has agency and sees an exit path through policy reform.
constraint_indexing:constraint_classification(pharmaceutical_dependency_economy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DRUG APPROVAL RITUAL (PITON) — FDA/EMA approval processes maintain significant performative content: regulatory burdens create barriers to entry that protect incumbent firms more than they ensure safety. The approval theater persists through institutional inertia despite lower functional benefit in mature drug classes. Expensive clinical trials (often performed on populations different from end-users) delay generic alternatives. Theater ratio reflects that much approval complexity functions to maintain extraction rather than ensure public health — a degraded institutional mechanism maintained by structural momentum.
constraint_indexing:constraint_classification(pharmaceutical_dependency_economy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some pharmaceutical cost is inherent: drug development requires significant capital investment, clinical trial safety requirements are biologically necessary, and manufacturing complexities cannot be eliminated. This perspective risks naturalizing contingent policy arrangements (patent lengths, exclusivity periods, regulatory design) as immutable requirements of pharmaceutical economics. The engine's false summit detector will identify this as rationalization of institutional arrangements rather than natural law.
constraint_indexing:constraint_classification(pharmaceutical_dependency_economy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_dependency_economy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_dependency_economy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_dependency_economy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_dependency_economy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_dependency_economy, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_dependency_economy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The pharmaceutical dependency economy exhibits substantial extraction through monopoly pricing, patent evergreening, and supply chain control. However, extraction is not maximal because: (a) genuine R&D costs justify some margin (estimated 10-30% of pricing), (b) regulatory complexity creates real safety/quality assurance (though with performative component), (c) manufacturing scale economies require significant capital investment. The increasing trajectory (0.42 → 0.58 over 15 years) reflects rising rates of evergreening, increasing regulatory complexity, and consolidation reducing competition among manufacturers. Suppression (0.62): High. Patients experience strong suppression through: biological necessity of medication (cannot simply choose not to take treatment), legal enforceability of patents (no ability to produce generics), regulatory barriers to generic entry (expensive bioequivalence testing), geographic barriers (some regions lack generic supply chains), economic barriers (cost prevents purchase for low-income groups). Suppression is not total because: some generics eventually reach market (patent expiration), some countries invoke compulsory licensing, some patients find cost-control workarounds (medication splitting, reduced adherence, international purchasing). Theater ratio (0.55): Moderate. Approval processes maintain significant performative content: FDA/EMA review complexity, clinical trial designs often conducted on populations different from end-users, post-market surveillance rituals. However, theater is not dominant because genuine safety validation exists (manufacturing control, adverse event tracking) and is functionally valuable. Theater has increased over the interval as regulatory complexity has grown faster than genuine safety requirements (0.35 → 0.55).
 *
 * PERSPECTIVAL GAP:
 *   The pharmaceutical dependency economy demonstrates maximal perspectival divergence. Chronic patients experience pure extraction (Snare) — no coordination benefit, no exit option, full cost. Manufacturers experience pure coordination (Rope) — patent protection solves the innovation funding problem, enables risk-taking, benefits their operations. Healthcare systems experience mixed tangled rope — genuine coordination (drugs reach patients) alongside extraction (prices exceed production costs plus R&D). Generic manufacturers experience constrained tangled rope — regulatory pathways coordinate bioequivalence testing but patent law extracts by excluding them from markets. Patent reform coalition sees temporary extraction (Scaffold) — sunset clauses inherent in patent law (expiration dates, exclusivity periods), policy reform (compulsory licensing, TRIPS renegotiation) providing exits. The approval ritual appears as degraded piton (institutional inertia, complexity no longer justified by safety gains). The civilizational observer risks seeing mountain (innovation requires investment, safety requires testing, complexity is inevitable) but the structural data reveals this as false summit — many contingencies (patent length choices, evergreening strategies, regulatory complexity) are policy decisions, not natural laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position: (1) Chronic patients: trapped exit + victim status → high d (0.93) → high f(d) (1.38) → experience χ as high extraction. (2) Healthcare systems: constrained exit + mixed victim/beneficiary → moderate d (0.60) → moderate f(d) (0.78) → experience mixed tangled rope. (3) Manufacturers: arbitrage exit + beneficiary status → low d (0.18) → low f(d) (0.00) → experience χ as coordination. (4) Generic manufacturers: constrained exit + mixed position → moderate-high d (0.65) → f(d) (1.00) → experience as asymmetric tangled rope. (5) Reform coalition: constrained exit + organized power → lower d (0.52) → f(d) (0.65) → see extraction as surmountable through policy. The beneficiary/victim declarations (manufacturers benefit, patients/systems are victims) drive the directionality pipeline and explain why the same constraint produces snare from powerless perspective, rope from beneficiary perspective, and tangled rope from systemic perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The pharmaceutical dependency economy resolves mandatrophy by acknowledging that ALL perspectives have structural validity. The constraint simultaneously solves real problems (funding drug development, ensuring safety, coordinating complex supply chains) AND extracts rents from trapped agents (chronic patients, low-income populations). This is exactly what tangled rope classification captures: genuine coordination function + asymmetric extraction + active enforcement. The mandatrophy does not arise from ambiguity about whether the constraint is good or bad — it arises from the genuine structural coexistence of coordination and extraction. The analytical observer's temptation to classify as Mountain (innovation has unavoidable costs) is a false summit: yes, some costs are necessary (R&D, safety testing, manufacturing complexity), but the actual constraint value (0.58 extractiveness, 0.62 suppression, rising theater ratio) reflects extraction beyond these necessities. The constraint is not resolved by choosing one perspective — it is resolved by recognizing the legitimacy of each perspective while rejecting the naturalization that converts contingent policy choices into inevitable laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_extraction_tradeoff,
    'How much of pharmaceutical profit margin is necessary for R&D funding versus extractive rent-seeking?',
    'Comparative analysis of R&D spending, clinical trial costs, and marketing budgets across firms; correlation between profit margins and drug innovation rates; pharmaceutical economics empirical studies on optimal patent periods',
    'If R&D requirement accounts for <30% of margin: pharmaceutical dependency economy is primarily extractive (Snare). If >50%: coordination function is substantial (Rope/Tangled Rope justified). Current estimates range 10-30% R&D, 20-40% marketing, remainder profit/shareholder extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_extraction_tradeoff, empirical, 'Proportion of pharmaceutical pricing necessary for innovation versus extraction').

omega_variable(
    generic_equivalent_availability,
    'What percentage of patients with chronic conditions have access to generic equivalents at any given time, and how does this vary by disease type and geography?',
    'Global pharmaceutical supply data; patent expiration tracking; analysis of patent evergreening practices; generic availability studies by therapeutic area and region',
    'If >70% have generic access: snare classification weakens (trapped exit is less total). If <30%: snare classification strengthens (trapped agents have minimal alternatives). Current estimates: 40-60% have generics available, but 20-30% of chronic medications still under extended patent/exclusivity periods.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generic_equivalent_availability, empirical, 'Availability of generic alternatives for chronic medications').

omega_variable(
    price_elasticity_patient_behavior,
    'What proportion of patients reduce adherence or abandon treatment when faced with cost barriers, and what are the health outcome consequences?',
    'Medication adherence studies controlling for cost; health outcome data for patients with insurance gaps; comparative analysis of adherence rates across cost tiers',
    'If high elasticity + severe outcomes: suppression classification justified (costs prevent exit). If patients have workarounds: suppression lower (constraining rather than trapping).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_elasticity_patient_behavior, empirical, 'Medication adherence reduction due to cost barriers').

omega_variable(
    patent_evergreening_prevalence,
    'How systematically do manufacturers extend patent/exclusivity periods through minor formulation changes, new indications, or combination drugs rather than genuine innovation?',
    'Patent analysis of drug class extensions; comparison of original patent terms to extended protection periods; FDA approval data for incremental vs breakthrough indications',
    'If evergreening is systematic (>70% of extended exclusivity): rent-seeking mechanism confirmed (extraction). If rare: patent system functions as intended (coordination). Current data suggests evergreening in >80% of top-grossing drugs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patent_evergreening_prevalence, empirical, 'Systematic use of patent evergreening to extend monopoly periods').

omega_variable(
    regulatory_capture_versus_public_health,
    'To what extent does FDA/EMA regulatory design reflect optimal public health requirements versus pharmaceutical industry preferences?',
    'Comparative analysis of approval timelines and standards across jurisdictions; pharmaceutical industry influence on regulatory policy; analysis of approval standards for generic versus branded drugs',
    'If capture substantial: approval ritual (Piton) confirmed. If regulatory design is genuinely optimal: approval complexity justified (coordination). Current evidence suggests moderate capture in approval prioritization and complexity thresholds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_versus_public_health, empirical, 'Extent of regulatory capture in pharmaceutical approval processes').

omega_variable(
    compulsory_licensing_effectiveness,
    'When governments invoke compulsory licensing for essential medicines, how effective are alternative supply chains at reducing prices and meeting demand?',
    'Analysis of countries using compulsory licensing (India, Brazil, Thailand); price changes after licensing invoked; supply chain stability; comparison to branded monopoly supply',
    'If effective (>50% price reduction, stable supply): scaffold/exit mechanisms are real (policy reform works). If ineffective: victims remain trapped despite policy tools. Current evidence mixed by geography.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_licensing_effectiveness, empirical, 'Effectiveness of compulsory licensing as escape mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_dependency_economy, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharmdep_tr_t0, pharmaceutical_dependency_economy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pharmdep_tr_t5, pharmaceutical_dependency_economy, theater_ratio, 5, 0.42).
narrative_ontology:measurement(pharmdep_tr_t10, pharmaceutical_dependency_economy, theater_ratio, 10, 0.52).
narrative_ontology:measurement(pharmdep_tr_t15, pharmaceutical_dependency_economy, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(pharmdep_be_t0, pharmaceutical_dependency_economy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pharmdep_be_t5, pharmaceutical_dependency_economy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(pharmdep_be_t10, pharmaceutical_dependency_economy, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(pharmdep_be_t15, pharmaceutical_dependency_economy, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_dependency_economy, resource_allocation).
narrative_ontology:affects_constraint(pharmaceutical_dependency_economy, drug_access_inequality).
narrative_ontology:affects_constraint(pharmaceutical_dependency_economy, antimicrobial_resistance_incentive_misalignment).
narrative_ontology:affects_constraint(pharmaceutical_dependency_economy, rare_disease_orphan_drug_pricing).

% DUAL FORMULATION NOTE:
% The pharmaceutical dependency economy decomposes into multiple structurally distinct constraints: (1) drug access inequality (distribution mechanisms, geographic barriers, insurance gaps) has different ε from pricing extraction; (2) antimicrobial resistance incentive misalignment (R&D incentives poorly aligned with public health needs) is downstream of the dependency economy; (3) rare disease orphan drug pricing (small markets, no generics possible) represents a variant where suppression approaches total but extractiveness can be lower (fewer agents trapped). These are linked constraints sharing a common institutional foundation (patent law, regulatory design) but with distinct ε values and beneficiary/victim relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_dependency_economy, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
