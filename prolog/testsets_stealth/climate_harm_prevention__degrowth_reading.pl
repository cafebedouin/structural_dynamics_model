% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Degrowth Reading of Climate Harm Prevention — Growth-Framework Climate Response as Coordinated Extraction
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth_reading of the
 *   climate_harm_prevention kernel. The standing arrangement under contest is
 *   the growth-framework climate response regime: decarbonization pursued
 *   through technological transition, green investment, and market mechanisms
 *   inside economies that must keep growing. As the degrowth reading assesses
 *   that arrangement, it coordinates real economic activity and delivers real
 *   but insufficient decarbonization while extracting carbon-budget space
 *   from the Global South and future generations to preserve Global North
 *   present consumption — and its own accounting (GDP) renders the transfer
 *   invisible. The reading's claim: growth-framework mitigation is physically
 *   and politically impossible, so legitimate climate response requires
 *   planned Northern contraction. Per the kernel-reading referent rule,
 *   epsilon is authored for the STANDING growth-framework arrangement (not
 *   for the endorsed contraction alternative), assessed by this reading's own
 *   lights. Claim and metrics are independent authored facts: claimed_type
 *   records this seat's structural verdict (tangled_rope — genuine
 *   coordination, asymmetric extraction through the same structure); the
 *   metrics describe the arrangement's operation as this reading sees it.
 *   Sibling readings are separate files, not part of this one.
 *
 * KEY AGENTS:
 *   - global_north_consumers: Primary beneficiary (organized/constrained) — consumption capacity subsidized by displaced climate costs; terminal receipt seat
 *   - fossil_capital_owners: Beneficiary and enforcement funder (institutional/constrained) — asset values depend on continued fossil throughput
 *   - northern_financial_institutions: Agenda-setter/enforcer (institutional/arbitrage) — credit allocation compels growth-oriented policy across debtor states
 *   - growth_dependent_states: Agenda-setter (institutional/identity_locked) — administer climate policy within growth mandates; legitimacy fused with GDP
 *   - global_south_populations: Primary target (powerless/trapped) — bears displaced climate harms with least historical contribution
 *   - future_generations: Primary target (powerless/trapped) — bears committed warming; absent from every decision forum
 *   - northern_carbon_workers: Dual-positioned beneficiary/payer (organized/constrained) — standing-arrangement employment, prescription-side transition costs
 *   - northern_climate_vulnerable_communities: Payer within the beneficiary bloc (powerless/trapped) — bears impacts now, risks bearing transition costs too
 *   - southern_export_elites: Secondary beneficiary (organized/arbitrage) — captures growth-led development gains while populations bear harms
 *   - climate_justice_movement: Excluded advocate (organized/constrained) — contraction-with-justice proposals ruled out of agenda bounds by the growth frame
 *   - ipcc_climate_science_community: Analytical observer (institutional/analytical) — documents gaps and scenario asymmetries; holds no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.78).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.65).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Degrowth Reading of Climate Harm Prevention — Growth-Framework Climate Response as Coordinated Extraction").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '0c06f0c5-19a6-4b7a-a15a-60172ba99cc8').
narrative_ontology:cs_kernel_codification('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', distributed).
narrative_ontology:cs_authority_grounding('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', distributed).
narrative_ontology:cs_reading_relation('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', climate_harm_prevention__adaptation_priority, forecloses).
narrative_ontology:cs_axiom('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', foundational, growth_framework_mitigation_impossible).
narrative_ontology:cs_axiom_status(growth_framework_mitigation_impossible, holdable).
narrative_ontology:cs_axiom_grounding('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', growth_framework_mitigation_impossible, empirically_contingent).
narrative_ontology:cs_axiom('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', foundational, north_consumption_contraction_owed).
narrative_ontology:cs_axiom_status(north_consumption_contraction_owed, holdable).
narrative_ontology:cs_axiom_grounding('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', north_consumption_contraction_owed, deontological).
narrative_ontology:cs_reference_frame('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', fair_share_contraction_framework).
narrative_ontology:cs_drift_state('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', contemporary_policy_implementation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0c06f0c5-19a6-4b7a-a15a-60172ba99cc8', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, fossil_capital_owners).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, northern_financial_institutions).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, growth_dependent_states).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, northern_carbon_workers).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, southern_export_elites).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, northern_climate_vulnerable_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, northern_carbon_workers).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, degrowth_necessity_thesis).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, absolute_decoupling_insufficiency).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, intergenerational_carbon_inequity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% High-consumption lifestyles in wealthy economies are enabled by the arrangement: energy- and import-intensive consumption at prices that do not carry climate costs, with the displaced harms borne elsewhere. Electoral weight makes consumption contraction the third rail of Northern politics. Exit would mean voluntary downshift against infrastructure, pricing, and status incentives that all point toward more consumption.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consumers, beneficiary,
    organized, biographical, constrained, global).

% Asset values and cash flows depend on continued fossil throughput; reserves, pipelines, and refineries are specific to the arrangement. They fund political resistance to contraction and the 'politically impossible' framing of alternatives, and some diversify into renewables, but their core position and political influence require the standing arrangement to persist. Stranding costs make full exit unattractive.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, fossil_capital_owners, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, fossil_capital_owners, agenda_setter).

% Allocate credit against growth expectations and enforce debt service that compels growth-oriented policy in debtor states; collect interest spreads on growth-dependent lending. Their portfolio mobility lets them restructure faster than states can change policy — the exit mobility that makes their enforcement credible. Contraction anywhere threatens loan books calibrated to expansion.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, northern_financial_institutions, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, northern_financial_institutions, beneficiary).

% Northern states administer climate policy inside GDP-growth mandates: fiscal revenue, employment, and electoral legitimacy are indexed to expansion, so contraction reads as state failure. The growth frame is fused with the state's self-conception — GDP is what governing success means — making exit from the frame unthinkable without an identity rupture no governing coalition will attempt.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, growth_dependent_states, agenda_setter,
    institutional, biographical, identity_locked, national).

% Employment and wages in carbon-intensive sectors depend on continued throughput; the standing arrangement preserves their jobs while climate impacts and any disorderly transition threaten them. Under the contraction prescription they bear immediate transition costs on a paycheck timescale, which organizes their politics against contraction despite their exposure to climate harm. Retraining promises arrive slower than layoffs.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, northern_carbon_workers, beneficiary,
    organized, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, northern_carbon_workers, payer).

% Commodity-export and manufacturing elites capture growth-led development gains: export revenue, state contracts, and capital appreciation under the expanding framework. They benefit from the growth machinery while their populations bear climate harms, and mobile capital lets them exit national harms without exiting the arrangement. Contraction framed as Northern self-limitation leaves their gains formally intact; contraction framed as universal restraint threatens them.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, southern_export_elites, beneficiary,
    organized, biographical, arbitrage, continental).

% Bear disproportionate climate impacts — heat, flood, crop failure, storm intensity — having contributed least to cumulative emissions. Adaptation finance arrives late and small relative to assessed need; loss-and-damage claims are acknowledged in principle and underfunded in practice. Migration options narrow as Northern borders tighten. There is no exit from the climate system itself.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, payer,
    powerless, biographical, trapped, global).

% Will inherit committed warming and locked-in infrastructure from decisions made entirely in their absence. The arrangement's discounting renders their claims weightless in present cost-benefit accounting, and no forum exists where they could object. No exit is possible from a deteriorated climate; their only representation is proxy advocacy with no enforcement power.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, payer,
    powerless, generational, trapped, global).

% Low-income Northern communities in flood, fire, and heat zones bear climate impacts now while the consumption benefits of the arrangement accrue elsewhere in their own societies. Under contraction they risk bearing energy-price and transition costs as well unless deliberately protected — the seat where the reading's justice claims are most tested inside the North, and the natural bridge constituency between North and South payer coalitions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, northern_climate_vulnerable_communities, payer,
    powerless, immediate, trapped, regional).

% Degrowth scholars, climate justice organizers, and Southern negotiating blocs argue for contraction-with-justice: fair-share carbon entitlements, debt cancellation, and managed downshift of Northern consumption. They are structurally outside the seats where 'politically possible' is defined — finance ministries, central banks, mainstream negotiating agendas — and their proposals are ruled out of bounds as non-serious by the growth frame itself. Their exclusion is maintained by the arrangement they critique.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_justice_movement, excluded,
    organized, generational, constrained, global).

% Assesses emission gaps and scenario feasibility; documents that demand-side and lifestyle-change scenarios receive a small fraction of modeling attention relative to technology scenarios, and that pledge-delivery gaps widen with each assessment cycle. Holds no enforcement power and treats value questions — which future is legitimate, who should contract — as outside its remit, which leaves the feasibility dispute this reading turns on formally unadjudicated.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, ipcc_climate_science_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, global_north_consumers).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The standing growth-framework arrangement coordinates the global economy: it organizes production, trade, employment, and state finance around expanding output, and channels climate response through that machinery — green investment, technology transition, market mechanisms — so that decarbonization proceeds without halting growth. It also coordinates the climate regime itself: a single framework of pledges, accounting, and finance through which all parties act.
% TRANSFER_FUNCTION: Moves carbon-budget space and atmospheric sink capacity to present Global North consumption; moves climate harms, adaptation burdens, and loss-and-damage costs to Global South populations and future generations; moves debt-service flows from debtor states (largely Southern) to Northern creditor institutions; moves political attention toward technology futures and away from present consumption restraint.
% ABSENT_VOICES: Future generations are absent from every forum and can object only through proxy institutions with no enforcement power. Global South populations are formally seated in the climate regime but structurally under-resourced and outvoted on finance and ambition. Degrowth and climate justice advocates are excluded from the seats where 'politically possible' is defined — finance ministries, central banks, mainstream negotiating agendas — their proposals ruled out of bounds as non-serious by the growth frame itself. The global poor outside formal negotiating coalitions have no seat at all.
% DISAPPEARANCE_RATIONALE: The growth framework is the operating system of the global economy: credit is created against growth expectations, employment and state revenue are indexed to expansion, and development expectations worldwide presuppose it. Overnight disappearance would collapse finance and employment in both North and South before any climate benefit could materialize — the world would rearrange around the collapse, not around climate stabilization.
% FOUNDING_PROBLEM: The arrangement was built to solve two stacked problems: the post-war problem of organizing industrial expansion, employment, and development (the growth framework itself), and the later problem of decarbonizing without triggering economic collapse or political backlash — climate response grafted onto the growth framework rather than replacing it.
% FOUNDING_PROBLEM_CORROBORATION: No source outside the benefiting parties attests that the founding problem (decarbonize within growth) remains solvable as posed — that attestation comes only from growth-dependent institutions themselves, which is itself signal. The dead-as-posed status is corroborated from outside the beneficiary set: IPCC and UNEP assessments document widening pledge-delivery gaps, and the ecological economics literature corroborates the decoupling-insufficiency claim. Southern negotiating blocs corroborate the harm-displacement reading of the arrangement's operation.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78 at interval end) because the arrangement concentrates carbon-budget consumption in Northern present consumption while displacing harms to seats with no exit and no voice; the transfer is rendered invisible by the framework's own accounting, which counts the consumption but not the displaced harm. It is not authored at 1.0 because real decarbonization and real development gains occur through the same structure. Suppression (0.65) is a raw structural property, unscaled by power or scope: it reflects marginalization of alternatives through growth-dependence (debt service, employment, fiscal revenue) rather than overt coercion, with 'politically impossible' operating as a self-fulfilling frame — below snare-typical suppression because the alternative remains articulable. Theater (0.48) reflects the documented pledge-delivery gap: offsets, net-zero pledges without delivery plans, and accounting maneuvers constitute nearly half of observed climate-response activity while real deployment continues. Accessibility_collapse (0.42): the degrowth alternative is articulable and partially institutionalized (IPCC demand-side scenarios, wellbeing-economy initiatives) but structurally blocked from agenda access. Resistance (0.60): climate justice mobilization, Southern bloc demands, and youth movements are sustained but so far ineffective. The three measurement series share one six-point grid (1992, 2000, 2008, 2016, 2021, 2026). Suppression_requirement is authored because this story specifically tracks enforcement-capacity change: the growth-dependence machinery deepened across the interval (post-2008 financialization, expanding sovereign debt, credit-allocation enforcement), requiring progressively more active marginalization as degrowth critique gained scholarly visibility.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats the arrangement is the only serious climate policy: contraction is unthinkable from inside growth-dependent institutions, so the structure appears as coordination toward decarbonization. From the payer seats the same structure operates as enforced extraction — the coordination is real, but its proceeds and costs are asymmetrically distributed. The degrowth reading's central claim is that the first perspective is itself a product of the arrangement: growth-dependence makes the arrangement's alternative invisible from the seats that run it. Two institutional agenda-setter seats diverge on exit despite equal nominal power: growth_dependent_states are identity_locked (the growth frame is fused with state legitimacy — GDP as the measure of governing success), while northern_financial_institutions hold arbitrage-grade exit (portfolio mobility), which is what makes their enforcement of growth-dependence credible while states' compliance is compelled. The payer seats' coalition potential — Southern negotiating blocs, climate justice movements, and Northern climate-vulnerable communities acting together — is the structural threat to the arrangement; its current ineffectiveness (resistance without agenda access) is what keeps the extraction stable. The engine computes per-seat classifications from this structural data; the authored claim does not adjudicate between seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats (global_north_consumers, fossil_capital_owners, northern_financial_institutions, growth_dependent_states, southern_export_elites, and northern_carbon_workers in their standing-arrangement position) derive low directionality — damped or inverted effective extraction. Victim seats (global_south_populations, future_generations, northern_climate_vulnerable_communities) derive high directionality, with trapped exit pushing each toward the full-target end. The arrangement's global scope amplifies effective extraction for targets (pledge verification across jurisdictions is hard; the engine owns the modifier). Receipt: the extraction's proceeds terminate in Northern consumption capacity — fossil profits and creditor interest are intermediate captures that recycle into the same consumption complex — so gain_flow names global_north_consumers rather than diffuse: multiple seats capture shares, but the terminal accrual seat is identifiable. northern_carbon_workers' prescription-side costs are a coalition problem (see the intra_north_cost_incidence omega), not a directionality correction: directionality is defined against the standing arrangement, which is epsilon's referent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — decarbonize without halting growth — is contested across the kernel's readings: mitigation_priority attests it live, this reading attests it dead-as-posed, adaptation_priority attests mitigation infeasible outright. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no zombie flag fires, because the dispute is over whether the mandate can ever be fulfilled, not over a dead mandate being theatrically maintained — the arrangement's activity is substantially functional (it runs the world economy and delivers real decarbonization) even as this reading judges its climate function failing. The mandatrophy discipline cuts against this reading too: declaring the growth framework's mandate dead is the reading's own contested move, and the classification prevents that declaration from being misread as proof of pure extraction — the arrangement retains genuine coordination function, which is why tangled_rope is claimed from this seat rather than snare. If the decoupling-sufficiency omega resolved decisively in this reading's favor and enforcement of alternatives intensified further, the trajectory would run toward snare; the measurement series shows the accumulation pattern that T17-style investigation would flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (degrowth_reading) of the climate_harm_prevention kernel; what structurally changes if a sibling reading (mitigation_priority, adaptation_priority) is instantiated instead?',
    'Comparative classification across the sibling constraint files: each sibling authors its own epsilon, beneficiary/victim structure, and claimed_type for the same kernel, and the corpus compares the three.',
    'mitigation_priority relocates costs onto transition-incumbents and concentrates beneficiaries in future generations via growth-enabled decarbonization; adaptation_priority shifts benefits to present near-term populations and reassigns South/future harm as accepted background. Victim set, epsilon, and type all move; the shared upstream physical constraint (atmospheric carbon budget finiteness) does not.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which reading of the kernel this constraint instantiates and what sibling readings would change.').

omega_variable(
    decoupling_sufficiency_dispute,
    'Is the degrowth reading''s foundational feasibility claim empirically correct — can absolute decoupling of territorial and consumption-based emissions from GDP proceed fast enough within growth frameworks to meet climate targets, or is growth-framework mitigation physically impossible as claimed?',
    'Decomposition analysis comparing observed absolute decoupling rates (best national cases, consumption-based accounting) against required rates implied by fair-share carbon budgets; ex-post audit of green-growth scenario projections against delivered outcomes.',
    'If required-rate decoupling is demonstrated, the impossibility axiom fails, the reading collapses toward mitigation_priority, and the declared foreclosure of that sibling is empirically refuted; if not, the axiom is corroborated and mitigation_priority is foreclosed on the evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_sufficiency_dispute, empirical, 'The empirical core of the impossibility axiom; routes the engine''s foreclosure computation.').

omega_variable(
    political_feasibility_of_contraction,
    'Is planned contraction politically implementable, or does the degrowth reading replace one impossibility claim with another?',
    'Assessment of post-growth policy experiments (wellbeing-economy initiatives, work-time reduction trials, explicit post-growth legislative proposals) and historical precedents for deliberate output reorganization (war mobilization, planned recessions); comparative political feasibility analysis against the growth framework''s own mitigation delivery record.',
    'If contraction is as politically infeasible as the growth framework''s sufficient mitigation, the reading''s claim to be the legitimate response weakens and the live contest shifts to comparative feasibility with adaptation_priority; if contraction is implementable where growth-framework mitigation is not, the reading''s legitimacy claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_contraction, empirical, 'Political feasibility of the reading''s prescribed response.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternatives structural (growth-dependence: debt service, employment dependence, fiscal revenue) or internalized (growth as common sense — ''there is no alternative'' as cognitive default), and in what proportion?',
    'Post-crisis trajectory analysis: in periods and places where structural growth-dependence loosens (austerity eras, degrowth-adjacent recessions, post-growth local experiments), does contraction become thinkable, or does growth-normative commitment reassert and foreclose it anyway?',
    'If a large share is internalized, suppression persists after structural loosening — accessibility_collapse stays elevated without ideological change and policy experiments fail for reasons the structural data cannot show; if mostly structural, removing growth-dependence (debt relief, employment guarantees) collapses the suppression directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized mechanism of alternative-suppression.').

omega_variable(
    intra_north_cost_incidence,
    'Who within the Global North bears the contraction costs — is the Northern payer seat uniform, or do Northern working classes and carbon-intensive regions bear transition costs while professional classes bear consumption restraint?',
    'Distributional incidence analysis of specific contraction instruments (carbon rationing, work-time reduction, wealth-based contraction, luxury-emission targeting) across income and regional strata.',
    'If incidence is regressive, northern_carbon_workers and northern_climate_vulnerable_communities become victims of the prescription as well as of the standing arrangement, compromising the reading''s justice claim and its coalition prospects; if incidence is progressive, the intra-North coalition the reading needs becomes structurally available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intra_north_cost_incidence, empirical, 'Intra-North cost incidence of the contraction prescription.').

omega_variable(
    emission_attribution_framing,
    'The reading''s epsilon and victim structure depend on consumption-based emissions attribution (Northern demand drives Southern production emissions); a production-based attribution frame would locate more extraction in Southern export sectors. Which attribution frame does this constraint''s identity require?',
    'Comparative classification of the same arrangement under consumption-based and production-based attribution; the degrowth reading''s own texts (fair-share allocation, consumption accounting) fix the frame internally, and the corpus records the sensitivity.',
    'Under production-based framing, southern_export_elites shift toward the payer side and Northern consumers'' directionality falls; the tangled_rope structure persists but the extraction geography changes, and the reading''s ''Northern consumption bears costs'' structural delta weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emission_attribution_framing, conceptual, 'Attribution-frame sensitivity of the extraction geography.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_harm_prevention__degrowth_reading, theater_ratio, 1992, 0.25).
narrative_ontology:measurement_basis(clim_tr_t1992, observed).
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__degrowth_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(clim_tr_t2000, observed).
narrative_ontology:measurement(clim_tr_t2008, climate_harm_prevention__degrowth_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement_basis(clim_tr_t2008, observed).
narrative_ontology:measurement(clim_tr_t2016, climate_harm_prevention__degrowth_reading, theater_ratio, 2016, 0.42).
narrative_ontology:measurement_basis(clim_tr_t2016, observed).
narrative_ontology:measurement(clim_tr_t2021, climate_harm_prevention__degrowth_reading, theater_ratio, 2021, 0.46).
narrative_ontology:measurement_basis(clim_tr_t2021, observed).
narrative_ontology:measurement(clim_tr_t2026, climate_harm_prevention__degrowth_reading, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(clim_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_harm_prevention__degrowth_reading, base_extractiveness, 1992, 0.58).
narrative_ontology:measurement_basis(clim_be_t1992, observed).
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__degrowth_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(clim_be_t2000, observed).
narrative_ontology:measurement(clim_be_t2008, climate_harm_prevention__degrowth_reading, base_extractiveness, 2008, 0.66).
narrative_ontology:measurement_basis(clim_be_t2008, observed).
narrative_ontology:measurement(clim_be_t2016, climate_harm_prevention__degrowth_reading, base_extractiveness, 2016, 0.7).
narrative_ontology:measurement_basis(clim_be_t2016, observed).
narrative_ontology:measurement(clim_be_t2021, climate_harm_prevention__degrowth_reading, base_extractiveness, 2021, 0.74).
narrative_ontology:measurement_basis(clim_be_t2021, observed).
narrative_ontology:measurement(clim_be_t2026, climate_harm_prevention__degrowth_reading, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(clim_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_harm_prevention__degrowth_reading, suppression_requirement, 1992, 0.45).
narrative_ontology:measurement_basis(clim_su_t1992, observed).
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__degrowth_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement_basis(clim_su_t2000, observed).
narrative_ontology:measurement(clim_su_t2008, climate_harm_prevention__degrowth_reading, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement_basis(clim_su_t2008, observed).
narrative_ontology:measurement(clim_su_t2016, climate_harm_prevention__degrowth_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement_basis(clim_su_t2016, observed).
narrative_ontology:measurement(clim_su_t2021, climate_harm_prevention__degrowth_reading, suppression_requirement, 2021, 0.63).
narrative_ontology:measurement_basis(clim_su_t2021, observed).
narrative_ontology:measurement(clim_su_t2026, climate_harm_prevention__degrowth_reading, suppression_requirement, 2026, 0.65).
narrative_ontology:measurement_basis(clim_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, atmospheric_carbon_budget_finiteness).

% DUAL FORMULATION NOTE:
% The climate_harm_prevention kernel decomposes into three readings with different epsilon referents and victim structures: this degrowth_reading (contraction required; growth-framework mitigation impossible), mitigation_priority (technological transition within growth suffices), and adaptation_priority (resilience priority; accepts higher warming). All three share the upstream physical constraint atmospheric_carbon_budget_finiteness, which none of them contest — the readings contest the response, not the budget. The upstream claim (the budget is finite and depleting) is cited as evidence within this reading's impossibility axiom, which is why the edge runs from this reading to the upstream mountain as well as to both siblings. Each file in the family links the others; no member is an orphan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
