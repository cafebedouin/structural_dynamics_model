% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__systemic_transformation_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: CAA Section 111(d) Systemic Transformation Reading — Grid-Wide Generation-Shifting Mandate
 *   domain: administrative law / environmental regulation / constitutional interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act directs EPA to regulate existing
 *   sources' 'best system of emission reduction.' This story instantiates ONE
 *   reading of that contested kernel — the systemic_transformation_reading,
 *   on which the best system reaches beyond the facility fenceline to
 *   grid-wide, generation-shifting measures: renewable substitution, dispatch
 *   changes, and early coal retirement. Under this reading, EPA's 2015 Clean
 *   Power Plan set state-level decarbonization targets; states implemented
 *   through plans with trading and substitution flexibility; and the
 *   arrangement operated from proposal (2014) through finalization (2015),
 *   stay (2016), repeal (2019), and Supreme Court foreclosure (West Virginia
 *   v. EPA, 2022). Per the epsilon-invariance principle this story authors
 *   ONLY the systemic reading's arrangement: its epsilon, beneficiaries, and
 *   victims are those of the generation-shifting architecture, not of the
 *   facility-level sibling
 *   (caa_section_111d_delegation__facility_constraint_reading), which is a
 *   separate constraint story. The referent of epsilon is this standing
 *   arrangement as this reading operates it — never the facility-level
 *   alternative the reading's opponents endorse.
 *
 * KEY AGENTS:
 *   - epa_rulemaking_authority: agenda setter (institutional/constrained) — writes the best-system determination and state targets; collects regulatory reach
 *   - coal_plant_operators: primary target (powerful/constrained) — bears retirement mandates and stranded assets
 *   - coal_mining_communities: concentrated target (powerless/trapped) — bears job loss and regional decline with no agenda seat
 *   - fossil_locked_state_governments: institutional target (institutional/constrained) — bears compliance and fiscal costs; resists via litigation coalition
 *   - renewable_energy_producers: primary beneficiary (organized/mobile) — collects compliance-driven demand and credit revenue
 *   - natural_gas_producers: secondary beneficiary (powerful/mobile) — collects bridge-fuel substitution share
 *   - environmental_advocacy_organizations: mission beneficiary (organized/mobile) — collects precedent and emission reductions, bears only litigation costs
 *   - residential_ratepayers: dual-positioned seat (moderate/constrained) — pays rate pass-through, receives efficiency and health benefits
 *   - federal_courts: analytical observer — adjudicates the kernel contest between readings
 *   - future_generations: excluded voice (non-agent entry) — largest claimed beneficiary with no procedural seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.55).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.58).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA Section 111(d) Systemic Transformation Reading — Grid-Wide Generation-Shifting Mandate").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative law / environmental regulation / constitutional interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a').
narrative_ontology:cs_kernel_codification('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', fixed_text).
narrative_ontology:cs_authority_grounding('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', lineage).
narrative_ontology:cs_interpretation_layer_present('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a').
narrative_ontology:cs_reading_relation('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', foundational, best_system_reaches_beyond_fenceline).
narrative_ontology:cs_axiom_status(best_system_reaches_beyond_fenceline, overridden).
narrative_ontology:cs_axiom_grounding('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', best_system_reaches_beyond_fenceline, conventional).
narrative_ontology:cs_axiom('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', secondary, climate_necessity_justifies_capacious_reading).
narrative_ontology:cs_axiom_status(climate_necessity_justifies_capacious_reading, holdable).
narrative_ontology:cs_axiom_grounding('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', climate_necessity_justifies_capacious_reading, instrumental).
narrative_ontology:cs_reference_frame('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', gridwide_best_system_authority).
narrative_ontology:cs_drift_state('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', post_west_virginia_v_epa, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('9ba0a3db-5d1d-49bc-aae3-82bdc7a5241a', '2026-08-05T00:00:00Z').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_producers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_organizations).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, epa_rulemaking_authority).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_plant_operators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, residential_ratepayers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, residential_ratepayers).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, cooperative_federalism_implementation).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, technology_forcing_statutory_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines the best system of emission reduction, converts it into state-level emission targets, reviews and approves state implementation plans, and maintains a federal plan as a backstop for non-submitting states. Gains regulatory reach over the grid's generation mix and precedent for a capacious statutory reading; bears the litigation and political cost of defending that reach. Cannot abandon the program without replacement rulemaking and judicial review.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa_rulemaking_authority, agenda_setter,
    institutional, generational, constrained, national).

% Own and operate plants whose output the state targets assume will decline; face accelerated retirement dates, impaired capital, and lost market share. Can litigate, lobby, diversify into gas and renewables, or seek timeline relief, but cannot move sunk plants; most costs land on asset holders and remaining-plant margins.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_plant_operators, payer,
    powerful, biographical, constrained, national).

% Live in regions whose employment and tax base depend on mines serving the retiring fleet; bear job losses, population outflow, and eroding local revenue as retirement schedules advance. Had formal comment access in rulemaking but little agenda-setting power; exit means relocating households or retraining into thin local labor markets.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_communities, payer,
    powerless, biographical, trapped, regional).

% State governments of coal-dependent states face steep compliance costs, falling severance and property tax revenue, and intense constituent pressure. Can resist through multi-state litigation, refuse to submit implementation plans (risking the federal plan backstop), and petition Congress, but cannot exit the federal framework; their fiscal exposure persists across budget cycles.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_governments, payer,
    institutional, generational, constrained, national).

% Sell into demand the compliance architecture creates: renewable substitution credits, long-term power purchase contracts, and state plan build-out requirements. Deploy capital wherever credit markets price their output favorably and carry little of the arrangement's burden; their revenue projections under the plan drove the largest compliance-investment flows.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers, beneficiary,
    organized, biographical, mobile, national).

% Gain dispatch share as coal units retire earlier than they otherwise would, positioning gas as the substitution fuel in most state plans. Benefit through volume and basis pricing; carry modest exposure since deeper decarbonization targets could eventually reach gas generation too.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_producers, beneficiary,
    powerful, biographical, mobile, continental).

% Litigate to defend the arrangement's scope and publicize its health and climate benefits; gain precedent for climate regulation under existing authority and mission-advancing emission reductions. Bear litigation costs and political backlash but no compliance burden.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Pay compliance costs passed through retail rates and see some of those costs returned as efficiency-program savings and avoided health damages; long-run climate benefits accrue to them diffusely. Exit is limited to relocating across state lines or investing in self-generation; most households cannot do either.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, residential_ratepayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, residential_ratepayers, beneficiary).

% Adjudicate whether the statutory text reaches generation-shifting measures: the D.C. Circuit upheld the reading in 2019 over dissent, and the Supreme Court rejected it in West Virginia v. EPA in 2022. Their holdings determine which reading of the kernel is operative law; they collect nothing and pay nothing under the arrangement.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% The arrangement's claimed largest beneficiaries — avoided climate damages and reduced pollution — have no procedural seat in rulemaking or litigation; they are represented only indirectly by advocacy organizations and appear in the record only as asserted interests.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(caa_section_111d_delegation__systemic_transformation_reading, future_generations).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: power-sector carbon emissions are unpriced and no single state or generator captures the benefits of reducing them, so a national framework sets state-level targets derived from a best-system determination and lets trading, renewable substitution, and efficiency compliance locate the cheapest reductions across the interconnected grid.
% TRANSFER_FUNCTION: Moves generation, capital, and regulatory discretion: generation and investment shift from coal units to renewable and gas resources; compliance spending flows from coal-dependent operators and their ratepayers toward credit-generating renewables; target-setting authority moves from states and plant operators to EPA, while avoided health and climate damages flow to the public.
% ABSENT_VOICES: Coal mining communities had comment access but no agenda-setting seat despite bearing the most concentrated costs; future generations — the arrangement's largest claimed beneficiaries — have no seat at all; consumer advocates were outnumbered in the comment record by industry and advocacy commenters; plant workers were addressed only through general transition-assistance proposals.
% DISAPPEARANCE_RATIONALE: The record shows the rearrangement directly: when the arrangement was stayed in 2016, state plan development halted mid-stream; when repealed in 2019, the compliance architecture (trading-ready state plans, credit markets, renewable procurement pipelines built to the targets) was dismantled and replaced by the facility-level ACE rule; when the Supreme Court foreclosed the reading in 2022, EPA's successor rulemaking had to be rebuilt around fenceline measures and dozens of states' litigation postures reorganized. The arrangement's disappearance rearranged the regulatory world each time it occurred.
% FOUNDING_PROBLEM: After comprehensive climate legislation failed in 2009-2010, power plants remained the largest stationary source of U.S. carbon emissions with no operative federal limit; the founding problem was closing that gap using existing statutory authority — specifically whether Section 111(d)'s 'best system of emission reduction' could carry a grid-wide decarbonization mandate.
% FOUNDING_PROBLEM_CORROBORATION: The problem's liveness is corroborated from outside the benefiting parties by the U.S. National Climate Assessment and IPCC assessment cycles, and by utility integrated-resource plans that retired coal for economic reasons independent of the rule. The vehicle claim — that Section 111(d) properly carries a grid-wide mandate — is corroborated by the D.C. Circuit's 2019 majority (a source outside EPA and the environmental advocacy set) and contested by the Supreme Court's 2022 majority; no adjudicator outside the contest altogether exists, and the arrangement's own beneficiaries are the only sources asserting the vehicle claim without reservation.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55 and suppression 0.58 describe the arrangement at its operational peak (finalization through the early stay period): binding state targets with real retirement implications, enforced through plan approval and a federal backstop — substantial, but bounded by the genuine coordination function and by state pathway flexibility. Theater 0.30: the rulemaking and plan apparatus did real work, but a growing share of activity became litigation-defensive. Accessibility collapse 0.45: pathway choice persisted inside the reading and the facility-level sibling reading persisted as a live legal alternative, so alternatives never fully collapsed. Resistance 0.80: unprecedented multi-state litigation, congressional disapproval votes, a presidential veto fight, and a Supreme Court reversal. The temporal series runs on one shared grid (t0-t8 mapping 2014-2022: proposal, finalization, stay, repeal proposal, ACE finalization run-up, West Virginia v. EPA) so every tracked metric is authored at every examined point; extractiveness decays after 2016 as the arrangement loses operative force while theater rises as functional activity gives way to litigation posture. Suppression_requirement is authored because this story specifically tracks enforcement-capacity change: the machinery built to hold the arrangement (backstop plans, legal defense) matured to a 2016 peak and then decayed with the arrangement's authority.
 *
 * PERSPECTIVAL GAP:
 *   From EPA's seat the arrangement is cooperative federalism it designed and defended; from coal operator and coal community seats it is an uncompensated mandate with existential stakes; from renewable and advocacy seats it is demand creation and precedent; from the courts it is a question about the word 'system.' The payer seats attempted coalition first through Congress (veto-blocked) and finally through litigation to the Supreme Court — the resistance metric's 0.80 is that coalition's footprint. Per-seat classification is computed from power, exit, and role; the divergence between the agenda setter's seat and the trapped payer seats is the measurement, not a defect.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (renewable producers, gas producers, advocacy organizations, EPA) sit at the beneficiary end of d; declared victims (coal operators, coal communities, fossil-locked states) sit at the target end, with d amplified by constrained and trapped exits — sunk plants, immobile households, and states that cannot leave the framework. Residential ratepayers are dual-positioned and near-symmetric, which role-plus-exit derivation alone under-weights; a single override sets the moderate power atom (the ratepayer seat is the story's only moderate agent) to d=0.52. Federal courts are analytical and collect nothing; future generations are a non-agent entry excluded from the arithmetic. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live — the emissions gap the arrangement addressed did not close — so nothing here is mandate decay; the arrangement ended by jurisdictional repudiation, not obsolescence, and the mismatch check (status contested x verdict world_rearranges) raises no zombie flag. The tangled_rope classification prevents two misreadings: reading it as pure extraction erases the genuine coordination function that motivated utility compliance investment and state plan participation; reading it as pure coordination erases the concentrated, largely uncompensated costs that fell on coal communities whom the trading architecture did not reach. It also keeps the sibling facility reading structurally distinct: under that reading the coal-sector cost profile changes shape entirely, which is why the two are separate constraints rather than one constraint measured two ways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of kernel caa_section_111d_delegation — what structural facts would differ if the facility_constraint_reading were instantiated instead?',
    'Author the sibling story and compare victim sets, epsilon, and enforcement structure side by side; the disagreement is located in the semantic scope of ''system'' in ''best system of emission reduction.''',
    'If the facility reading is the operative law, the coal sector ceases to be a victim of this arrangement, EPA''s reach contracts to the fenceline, and this story''s tangled_rope structure dissolves into the sibling''s distinct profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: this story is one reading of the Section 111(d) kernel; the sibling reading would change the entire beneficiary/victim structure.').

omega_variable(
    statutory_scope_resolution,
    'Will the systemic reading''s textual claim be revived (Court composition change, new statutory text, or new rulemaking on remand), or does the repudiation hold?',
    'Track post-2022 existing-source rulemaking, certiorari grants, and congressional amendment attempts.',
    'Revival reactivates the full extraction/coordination profile and the temporal series becomes dormancy rather than death; permanent foreclosure fixes the arc as terminal decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_scope_resolution, empirical, 'Whether the reading''s repudiation is terminal or dormant.').

omega_variable(
    coal_transition_compensation_adequacy,
    'Does the coordination benefit justify the concentrated costs borne by coal mining communities, given the compensation actually delivered?',
    'Evaluate just-transition program funding (POWER Initiative, IRA-era community grants) against documented regional losses across the arrangement''s window.',
    'Adequate compensation moves the arrangement rope-ward (coordination with transitional costs); inadequate compensation moves it snare-ward (uncompensated concentrated sacrifice for diffuse gains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_transition_compensation_adequacy, preference, 'Whether the burden on coal communities is proportionate coordination cost or disproportionate sacrifice.').

omega_variable(
    compliance_cost_incidence,
    'Who actually bore the arrangement''s costs — ratepayers via pass-through, shareholders via stranded assets, or workers via job loss — and in what proportions?',
    'Utility rate case data, asset impairment filings, and regional employment statistics across the 2014-2022 window.',
    'If shareholders absorbed most costs, the victim structure narrows to asset holders; if ratepayers and workers bore more, the victim set widens and the ratepayer seat''s directionality rises above symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_incidence, empirical, 'Actual incidence of the arrangement''s costs across payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa111d_sys_tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(caa111d_sys_tr_t0, observed).
narrative_ontology:measurement(caa111d_sys_tr_t1, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 1, 0.18).
narrative_ontology:measurement_basis(caa111d_sys_tr_t1, observed).
narrative_ontology:measurement(caa111d_sys_tr_t2, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2, 0.26).
narrative_ontology:measurement_basis(caa111d_sys_tr_t2, observed).
narrative_ontology:measurement(caa111d_sys_tr_t4, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement_basis(caa111d_sys_tr_t4, observed).
narrative_ontology:measurement(caa111d_sys_tr_t6, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 6, 0.44).
narrative_ontology:measurement_basis(caa111d_sys_tr_t6, observed).
narrative_ontology:measurement(caa111d_sys_tr_t8, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement_basis(caa111d_sys_tr_t8, observed).

% Extraction over time
narrative_ontology:measurement(caa111d_sys_be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(caa111d_sys_be_t0, observed).
narrative_ontology:measurement(caa111d_sys_be_t1, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 1, 0.55).
narrative_ontology:measurement_basis(caa111d_sys_be_t1, observed).
narrative_ontology:measurement(caa111d_sys_be_t2, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement_basis(caa111d_sys_be_t2, observed).
narrative_ontology:measurement(caa111d_sys_be_t4, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(caa111d_sys_be_t4, observed).
narrative_ontology:measurement(caa111d_sys_be_t6, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement_basis(caa111d_sys_be_t6, observed).
narrative_ontology:measurement(caa111d_sys_be_t8, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement_basis(caa111d_sys_be_t8, observed).

% Suppression requirement over time
narrative_ontology:measurement(caa111d_sys_su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(caa111d_sys_su_t0, observed).
narrative_ontology:measurement(caa111d_sys_su_t1, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 1, 0.62).
narrative_ontology:measurement_basis(caa111d_sys_su_t1, observed).
narrative_ontology:measurement(caa111d_sys_su_t2, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement_basis(caa111d_sys_su_t2, observed).
narrative_ontology:measurement(caa111d_sys_su_t4, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 4, 0.66).
narrative_ontology:measurement_basis(caa111d_sys_su_t4, observed).
narrative_ontology:measurement(caa111d_sys_su_t6, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement_basis(caa111d_sys_su_t6, observed).
narrative_ontology:measurement(caa111d_sys_su_t8, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement_basis(caa111d_sys_su_t8, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, resource_allocation).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'the Section 111(d) best system' is a contested kernel that decomposes into two structurally distinct constraints. This story authors the systemic_transformation_reading (grid-wide generation-shifting authority; coal sector as concentrated victim; renewables as compliance-demand recipients; epsilon ~0.55 at operational peak). The sibling facility_constraint_reading authors the fenceline-limited reading, with a different victim set, different enforcement surface, and its own epsilon. The upstream/downstream pressure runs through litigation: each reading's judicial fortunes change the other's legitimacy conditions. Per the epsilon-invariance principle the two are separate stories linked by this edge, not one constraint measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
