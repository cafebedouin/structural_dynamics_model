% ============================================================================
% CONSTRAINT STORY: federal_regulatory_reach
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_regulatory_reach, []).

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
 *   constraint_id: federal_regulatory_reach
 *   human_readable: Federal Regulatory Reach and Interstate Coordination
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   Federal regulatory reach in the United States represents a hybrid
 *   coordination-extraction constraint that has evolved substantially since
 *   the 1960s expansion of federal environmental, labor, and consumer
 *   protection authority. The constraint coordinates interstate competition,
 *   prevents race-to-the-bottom dynamics in environmental and labor
 *   standards, and creates uniform market conditions. Simultaneously, it
 *   extracts through unequal compliance cost distribution, loss of state
 *   policy autonomy, and concentration of enforcement discretion in federal
 *   agencies with limited actual enforcement capacity. The constraint
 *   exhibits all eight perspectives (six canonical plus two institutional
 *   variations), demonstrating how the same regulatory system is experienced
 *   differently depending on structural position. Theater ratio has increased
 *   over the interval as regulatory frameworks have grown more complex while
 *   enforcement capacity has stagnated — agencies now manage compliance
 *   through negotiation, guidance, and selective enforcement rather than
 *   systematic monitoring. Base extractiveness has increased as the scope of
 *   federal authority has expanded (climate, data privacy, labor organizing
 *   rights) and compliance costs have accumulated across overlapping
 *   frameworks.
 *
 * KEY AGENTS:
 *   - Federal Agencies: Primary beneficiary (institutional/arbitrage) — captures enforcement authority, rule-making discretion, and budget justification through regulatory scope
 *   - State Governments: Primary victim (powerless/trapped) — must implement federal mandates without proportional funding or policy discretion; exit via non-compliance invokes federal penalties and funding loss
 *   - Small Businesses: Victim (powerless/constrained) — face per-capita compliance costs higher than large corporations; exit options exist but entail substantial costs
 *   - Large Corporations: Secondary beneficiary (institutional/arbitrage) — benefit from uniform market conditions and can offset compliance costs; use regulatory access to create barriers to entry for smaller competitors
 *   - Environmental Constituency: Mixed (moderate/constrained) — benefits from pollution control and ecosystem protection; bears costs through energy prices and job loss in extractive sectors
 *   - State Governments (Interstate Coalition): Organized actor (organized/mobile) — developing alternative coordination mechanisms that could sunset federal reach over generational timescale
 *   - Local Communities: Secondary victim (organized/constrained) — benefit from environmental protection and labor standards; lose policy autonomy and face concentrated costs
 *   - Analytical Observer: Civilizational (analytical/analytical) — risks naturalizing federal regulatory authority as necessary to large federal systems rather than recognizing it as contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_regulatory_reach, 0.52).
domain_priors:suppression_score(federal_regulatory_reach, 0.65).
domain_priors:theater_ratio(federal_regulatory_reach, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_regulatory_reach, extractiveness, 0.52).
narrative_ontology:constraint_metric(federal_regulatory_reach, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(federal_regulatory_reach, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_regulatory_reach, tangled_rope).
narrative_ontology:human_readable(federal_regulatory_reach, "Federal Regulatory Reach and Interstate Coordination").
narrative_ontology:topic_domain(federal_regulatory_reach, "political_economy/federalism").

domain_priors:requires_active_enforcement(federal_regulatory_reach).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_regulatory_reach, federal_agencies).
narrative_ontology:constraint_beneficiary(federal_regulatory_reach, large_corporations).
narrative_ontology:constraint_victim(federal_regulatory_reach, state_governments).
narrative_ontology:constraint_victim(federal_regulatory_reach, small_businesses).
narrative_ontology:constraint_victim(federal_regulatory_reach, local_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE GOVERNMENT (SNARE) — States face federal regulatory mandates with minimal exit capacity. They cannot opt out of federal frameworks without losing federal funding (Medicaid, highway funding, etc.) or facing legal penalties. The regulatory reach extracts compliance costs and limits policy autonomy. States bear the implementation burden while federal agencies capture the policy credit and enforcement authority. No exit pathway except sovereign action, which invokes severe external costs.
constraint_indexing:constraint_classification(federal_regulatory_reach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS (SNARE) — Small firms face federal regulatory compliance costs that are more burdensome per-capita than for large corporations. They cannot negotiate exemptions or obtain regulatory relief at the same rates as large firms with dedicated compliance staff and lobbying capacity. Exit options exist (relocate to less-regulated jurisdiction, exit regulated market) but entail substantial costs — loss of market access, capital sunk in current jurisdiction, customer base disruption. High suppression from unequal cost distribution.
constraint_indexing:constraint_classification(federal_regulatory_reach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENVIRONMENTAL CONSTITUENCY (TANGLED ROPE) — Environmental regulation coordinates collective action (preventing tragedy-of-commons pollution) while extracting compliance costs. Communities benefit from reduced local pollution and ecosystem protection (coordination function) but also bear costs through higher energy prices, constrained industrial activity, and reduced local employment in extractive/manufacturing sectors. Mixed experience: genuine coordination need, but asymmetric distribution of benefits (diffuse public health gains) and costs (concentrated in particular regions and worker populations).
constraint_indexing:constraint_classification(federal_regulatory_reach, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE CORPORATION (ROPE) — Large firms with compliance capacity and regulatory access experience federal regulation primarily as coordination. Federal standards create uniform market conditions, preventing state-level regulatory arbitrage and race-to-the-bottom dynamics. Large firms can anticipate, influence, and comply with federal frameworks; they benefit from regulatory predictability and market consolidation (small competitors face higher compliance costs). Net beneficiary — the constraint coordinates markets while benefiting those with institutional capacity to navigate it.
constraint_indexing:constraint_classification(federal_regulatory_reach, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERSTATE COMMERCE COALITION (SCAFFOLD) — Organizations promoting interstate coordination (National Governors Association, Council of State Governments) view federal regulatory reach as a temporary coordination problem with a sunset. These actors see the constraint as necessary during transition (harmonizing divergent state rules) but expect eventual devolution of regulatory authority back to states as capacity and interstate agreements mature. This perspective sees active sunset mechanics: regulatory frameworks designed with transition timelines, periodic reauthorization requiring justification, and built-in state-option provisions. Sunset horizon: 20-40 years as interstate coordination mechanisms strengthen.
constraint_indexing:constraint_classification(federal_regulatory_reach, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY AGENCY (PITON) — Federal agencies view their regulatory reach as performatively justified but functionally degraded. Agencies maintain oversight authority through legal mandate and institutional inertia, but actual capacity to monitor compliance, adapt rules to local conditions, or enforce uniformly across 50 states is limited. The regulatory theater persists (agencies publish rules, hold hearings, issue compliance reports) but enforcement is sparse, rules are riddled with exemptions and waivers, and actual behavioral change is often achieved through negotiation rather than mandate. Agency sees its own authority as increasingly nominal — maintained because alternatives haven't matured, not because centralized regulation works.
constraint_indexing:constraint_classification(federal_regulatory_reach, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: LOCAL COMMUNITY (TANGLED ROPE) — Communities benefit from federal environmental and labor standards that prevent race-to-the-bottom conditions and provide minimum protections. However, they also experience extraction through loss of local policy control, federal rules imposed without community input, and asymmetric cost distribution (pollution control costs may close local employers without equivalent benefit flow to community). Genuine coordination function (preventing environmental externalities, labor exploitation) coupled with extraction (autonomy loss, imposed costs). Moderate power and constrained exit options.
constraint_indexing:constraint_classification(federal_regulatory_reach, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of regulatory centralization is inherent to large federal systems: coordination across jurisdictions requires some central authority; preventing jurisdictional arbitrage requires supra-state rules; public goods like environmental protection require enforceable common standards. This perspective sees federal regulatory reach as a necessary structural feature of any federal system above a certain scale. However, this perspective risks naturalizing what is actually a contingent institutional choice — the degree of federal reach, the mechanisms of enforcement, and the distribution of costs are all policy decisions, not laws of nature.
constraint_indexing:constraint_classification(federal_regulatory_reach, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_regulatory_reach_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_regulatory_reach, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_regulatory_reach, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_regulatory_reach, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_regulatory_reach, TR),
    TR >= 0.70.

:- end_tests(federal_regulatory_reach_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Federal regulatory reach extracts through multiple mechanisms: unequal compliance cost distribution, concentration of rule-making authority without proportional state input, and capture dynamics that benefit incumbents. The baseline (0.38 at T=0) reflects the genuine coordination function of early environmental regulation (1960s-1970s). The increase to 0.52 reflects the expansion of federal scope without corresponding increase in enforcement capacity or cost-sharing mechanisms. Suppression (0.65): High. Substantial barriers exist to exiting federal regulatory frameworks: funding dependence (Medicaid, highway funds); legal liability for non-compliance; inability of states to coordinate alternative frameworks unilaterally; lack of political viability for federal authority retreat. Theater ratio (0.58): Moderate-high. Federal agencies maintain substantial performative activity (rule-making, compliance audits, stakeholder engagement) while actual enforcement is sparse and negotiation-driven. The theater has increased as regulatory complexity has outpaced enforcement capacity. However, the constraint is not primarily theatrical — genuine extraction and coordination both occur.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Federal agencies and large corporations see primarily Rope (coordination mechanism enabling uniform markets). State governments see primarily Snare (trapped extraction). Small businesses see Snare (unequal cost burden with constrained exit). Environmental constituencies see Tangled Rope (genuine coordination function coupled with asymmetric costs). Interstate coalitions see Scaffold (temporary coordination problem with sunset mechanisms). Regulatory agencies themselves see Piton (performative authority maintained by inertia). Local communities see Tangled Rope (benefits from protection coupled with autonomy loss). The analytical observer risks seeing Mountain (federal regulatory reach as inherent to large federal systems) but structural analysis reveals this as a false summit — the specific mechanisms, cost distribution, and degree of federal authority are all policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments derive high d from trapped exit status and victim status — they cannot exit federal frameworks without severe costs. Small businesses derive high d from constrained exit and victim status but lower than powerless governments due to theoretical (if costly) exit options. Large corporations derive low d from arbitrage exit options and beneficiary status — they can adjust strategies and benefit from coordination. Federal agencies derive low d from institutional power and arbitrage exit — they maintain regulatory authority and extract policy discretion. Environmental constituency derives moderate d from mixed beneficiary/victim status and constrained exit — they benefit from pollution control but cannot easily exit the cost incidence. Interstate coalition derives lower d from organized power and mobile exit options — they can coordinate alternative mechanisms. Local communities derive moderate d from mixed status and constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: This constraint resolves the mandatrophy through perspectival multiplicity and institutional analysis. The mandatrophy emerges from colloquial usage: 'federal regulation' appears to be a single phenomenon but encompasses multiple structurally distinct claims. The coordination function (preventing race-to-the-bottom, creating uniform market conditions) is real and justified — this is Rope. The extraction function (unequal compliance cost distribution, autonomy loss, regulatory capture) is also real — this is Snare. The temporary problem being solved through alternative mechanisms (interstate coordination, state capacity building) is real — this is Scaffold. The performative authority maintained by institutional inertia is real — this is Piton. No single type is 'the' correct classification — the constraint is genuinely hybrid. The mandatrophy resolves by recognizing that federal regulatory reach is a tangled bundle of coordination and extraction, experienced differently depending on structural position. The 'is it really coordination or extraction?' question has the same status as the barrel of flour question: it depends on the viewpoint and decomposition granularity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_regulatory_efficacy,
    'Does centralized federal regulation actually achieve stated policy goals (pollution reduction, worker safety, etc.) more effectively than devolved or market-based mechanisms?',
    'Empirical comparison of outcomes in federally regulated vs. state-regulated sectors; cost-benefit analysis of federal rules vs. alternative coordination mechanisms',
    'If federal regulation is highly efficacious: the coordination function dominates, and the constraint classifies as Rope from more perspectives. If efficacy is marginal: the extraction function dominates, and more perspectives see Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_regulatory_efficacy, empirical, 'Whether federal regulation achieves policy goals more effectively than alternatives').

omega_variable(
    compliance_cost_distribution,
    'Is the unequal cost distribution between large corporations and small businesses a necessary feature of federal regulation or a correctable implementation detail?',
    'Regulatory impact analysis by firm size; comparison of compliance costs as percentage of revenue across firm size classes; analysis of whether differential compliance rates are designed or emergent',
    'If necessary feature: small business perspective remains Snare indefinitely. If correctable: regulatory reform could shift small business perspective to Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_cost_distribution, empirical, 'Whether unequal compliance costs are necessary or correctable').

omega_variable(
    state_capacity_and_race_to_bottom,
    'Given improved state technical capacity and interstate coordination mechanisms (CSG, NGA), is federal regulatory reach still necessary to prevent jurisdictional arbitrage and regulatory races to the bottom?',
    'Historical analysis of state-level coordination successes (California emissions standards adopted by other states); measurement of actual regulatory divergence when federal rules have sunset provisions or state flexibility options',
    'If states can coordinate effectively: scaffold perspective confirmed, sunset is real. If states still compete to attract businesses through deregulation: federal reach remains necessary coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_and_race_to_bottom, empirical, 'Whether improved state capacity enables effective interstate coordination without federal mandate').

omega_variable(
    regulatory_capture_at_federal_level,
    'Is federal regulatory reach itself captured by regulated industries, creating a Snare that benefits incumbents through regulatory barriers to entry?',
    'Analysis of lobbying expenditures, regulatory exemptions by firm size and sector, comparison of regulatory costs as barriers to entry, longitudinal tracking of firm consolidation in heavily regulated sectors',
    'If significant capture: federal regulation benefits incumbents at expense of entrants and small competitors; large corporation perspective shifts toward Snare. If minimal capture: large corporation perspective remains Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_at_federal_level, empirical, 'Degree of regulatory capture by incumbents in federal framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_regulatory_reach, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_reg_tr_t0, federal_regulatory_reach, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fed_reg_tr_t20, federal_regulatory_reach, theater_ratio, 20, 0.52).
narrative_ontology:measurement(fed_reg_tr_t40, federal_regulatory_reach, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(fed_reg_be_t0, federal_regulatory_reach, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fed_reg_be_t20, federal_regulatory_reach, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(fed_reg_be_t40, federal_regulatory_reach, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_regulatory_reach, enforcement_mechanism).
narrative_ontology:affects_constraint(federal_regulatory_reach, state_preemption_doctrine).
narrative_ontology:affects_constraint(federal_regulatory_reach, regulatory_arbitrage_dynamics).
narrative_ontology:affects_constraint(federal_regulatory_reach, interstate_commerce_barriers).

% DUAL FORMULATION NOTE:
% Federal regulatory reach decomposes into distinct constraint families: (1) environmental coordination — genuine coordination function with Rope baseline; (2) compliance cost distribution — extraction mechanism with Snare baseline; (3) regulatory capture dynamics — incumbent advantage with institutional concentration. These are linked but structurally distinct. The network map above tracks downstream constraints affected by federal reach changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_regulatory_reach, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
