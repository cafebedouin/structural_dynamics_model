% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__real_options_technologist, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: valuation_legitimacy__real_options_technologist
 *   human_readable: SpaceX Valuation Legitimacy via Real Options Portfolio
 *   domain: economic/technological/governance
 *
 * SUMMARY:
 *   This constraint story represents the real_options_technologist reading of
 *   the valuation_legitimacy kernel. It asserts that SpaceX's ~$1.75T
 *   valuation (2024 secondary market) is legitimate because it prices a
 *   portfolio of real options: Starlink (proven cash flows, $7.2B EBITDA),
 *   Starship (high-variance enabler for all downstream options), orbital
 *   compute (addresses genuine 62 GW U.S. power gap), lunar economy
 *   (first-mover advantage), and Mars (civilizational hedge). Vertical
 *   integration across launch, satellites, user terminals, and ground
 *   stations creates compounding optionality — success in any segment
 *   increases probability of success in others. Investors voluntarily
 *   participate understanding the risk/reward profile; the victim set is
 *   minimal. Humanity is the ultimate beneficiary if multiplanetary
 *   civilization succeeds. This reading coexists with three sibling readings
 *   that contest the valuation basis: dcf_fundamentalist (cash flows only),
 *   musk_cult_believer (founder track record), and governance_skeptic
 *   (control structure as extraction).
 *
 * KEY AGENTS:
 *   - elon_musk: Primary agenda setter (institutional/arbitrage) — controls 82.4% voting with 42% equity, sets development priorities
 *   - spacex_investors: Primary beneficiaries/payers (organized/constrained) — institutional and accredited investors in secondary markets, understand option structure
 *   - minority_shareholders: Potential payers/excluded (powerless/trapped) — employee shareholders, smaller investors with no governance voice
 *   - spacex_employees: Beneficiaries (moderate/constrained) — equity compensation tied to option realization, mission-aligned
 *   - humanity_future: Ultimate beneficiary (excluded/analytical) — gains if multiplanetary civilization materializes, no voice in current decisions
 *   - dcf_analysts: Observers (analytical/analytical) — apply traditional valuation, contest option framework
 *   - governance_advocates: Observers (institutional/analytical) — contest control structure as extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.35).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.15).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.35).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "SpaceX Valuation Legitimacy via Real Options Portfolio").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "economic/technological/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, 'ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9').
narrative_ontology:cs_kernel_codification('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', formalized).
narrative_ontology:cs_authority_grounding('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', expertise).
narrative_ontology:cs_interpretation_layer_present('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9').
narrative_ontology:cs_reading_relation('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_reading_relation('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_axiom('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', foundational, real_options_valuation_legitimate).
narrative_ontology:cs_axiom_status(real_options_valuation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', real_options_valuation_legitimate, empirically_contingent).
narrative_ontology:cs_axiom('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', foundational, vertical_integration_compounds_optionality).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_optionality, holdable).
narrative_ontology:cs_axiom_grounding('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', vertical_integration_compounds_optionality, empirically_contingent).
narrative_ontology:cs_reference_frame('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', real_options_valuation_framework).
narrative_ontology:cs_drift_state('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', contemporary_musk_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ec7e82a1-ffd5-4293-a3ea-b622c78bf6b9', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_employees).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, humanity_future).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, elon_musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_institutional_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_employee_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, spacex_institutional_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, spacex_employee_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, minority_shareholders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, real_options_valuation_framework).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_compounds_optionality).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, multiplanetary_civilization_feasibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls 82.4% voting power with 42% equity through dual-class shares and trust structure. Sets technical roadmap, capital allocation, and hiring for all SpaceX ventures (launch, Starlink, Starship, orbital compute). Bears founder-level downside via concentrated equity and personal guarantees on early debt. Can exit via secondary sales or share pledges but chooses to compound.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, elon_musk, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__real_options_technologist, elon_musk, beneficiary).

% Institutional funds (Baillie Gifford, Fidelity, Sequoia, etc.) and accredited angels in secondary markets. Enter voluntarily at valuations implying ~6% probability of $28.5T TAM. Understand option structure: Starlink DCF floor (~$180B) plus Starship/orbital compute/lunar/Mars call options. Exit constrained by secondary market windows, lockups, and information asymmetry. No governance rights (non-voting shares typical).
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_institutional_investors, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__real_options_technologist, spacex_institutional_investors, payer).

% Employees receive RSUs/options as compensation, vesting over 4 years. Equity value tied to same option portfolio. Mission-aligned (many joined for Mars goal). Exit constrained by vesting, lockups, trading windows, and career capital invested in SpaceX-specific skills. No governance voice.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_employee_shareholders, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__real_options_technologist, spacex_employee_shareholders, payer).

% Small investors, former employees with vested equity, SPAC/secondary buyers without institutional terms. Same downside as institutions, no governance voice, minimal liquidity, no information rights. This reading does not posit them as a distinct victim class (terms disclosed, voluntary entry), but governance_skeptic reading does. Included here as excluded seat to capture the structural possibility.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, minority_shareholders, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__real_options_technologist, minority_shareholders, excluded).

% Ultimate beneficiary if multiplanetary civilization succeeds: species survival hedge, resource expansion, scientific knowledge. Bears zero current cost, has zero voice, cannot exit the species-level outcome. Non-agent entity — included for narrative completeness of the option portfolio's terminal payoff.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, humanity_future, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__real_options_technologist, humanity_future).

% Equity researchers and fundamental investors who apply DCF/models to SpaceX. Contest option framework: argue unproven technologies are options not assets, Starlink EBITDA doesn't justify $1.75T, probability-weighting is subjective. Provide counter-narrative that constrains narrative premium but doesn't affect SpaceX operations directly.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, dcf_analysts, observer,
    analytical, immediate, analytical, global).

% Institutional investor stewardship teams, SEC/CFTC watchdogs, academic governance scholars. Argue 82.4% voting control with 42% equity is extraction mechanism: Musk can dilute, self-deal, block exits, control information flow. Their pressure creates regulatory/legislative risk but hasn't altered SpaceX's private-company governance.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, governance_advocates, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, elon_musk).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools patient capital for capital-intensive, high-variance technological development with compounding optionality: reusable heavy lift (Starship) enables satellite constellation (Starlink) which funds orbital compute/lunar/Mars; each success de-risks the next. Solves the 'valley of death' for space infrastructure where no single revenue stream justifies the full stack.
% TRANSFER_FUNCTION: Moves capital from institutional/employee investors to SpaceX for option development (engineering, testing, manufacturing, deployment). Transfers potential future value: Starlink cash flows to investors (if realized), civilizational option value to humanity (if multiplanetary succeeds), control rents to Musk (if governance_skeptic is right).
% ABSENT_VOICES: Retail investors excluded from secondary markets (accredited-only) who might misprice optionality. Future generations who bear civilizational risk (Mars contamination, orbital debris, resource conflict) with no representation. Competitors (Blue Origin, ULA, Rocket Lab) who face asymmetric capital access but are not direct parties to SpaceX's constraint.
% DISAPPEARANCE_RATIONALE: If the real-options valuation framework vanished overnight, SpaceX's capital structure would revert to DCF-only: Starlink valued at ~$180B (25x EBITDA), Starship/orbital compute/lunar/Mars valued near zero. Musk would need to fund development from Starlink cash flows alone (~$7.2B/year), slowing Starship by 5-10x. Investor base would shift from growth/option funds to yield funds. The multiplanetary timeline would extend from 2030s to 2050s+.
% FOUNDING_PROBLEM: Government-only space funding (NASA, DoD) was bottlenecked by political cycles, cost-plus contracting, and single-mission architectures. A private entity needed to vertically integrate launch, spacecraft, and operations to achieve full reusability and compound learning — which requires patient capital that prices optionality, not just near-term cash flows.
% FOUNDING_PROBLEM_CORROBORATION: NASA's commercial crew/cargo success (SpaceX, Northrop Grumman) corroborates private capital + fixed-price + vertical integration model. Academic real options literature (Trigeorgis, Dixit-Pindyck, McGrath) corroborates option valuation for staged irreversible investments. Independent analysts (Casey Research, Morgan Stanley Space Team) corroborate Starlink DCF floor + option portfolio structure. No corroboration from governance_skeptic camp — they contest the control structure, not the founding problem.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__real_options_technologist_tests).
:- end_tests(valuation_legitimacy__real_options_technologist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.35 reflects that the valuation transfers capital from investors to SpaceX for option development, but investors enter voluntarily with disclosed risk. The 6% implied probability of $28.5T TAM is within plausible ranges for a portfolio with one proven leg (Starlink) and four high-variance legs. Suppression at 0.15 reflects minimal coercion — investors can exit secondary markets (though liquidity varies), no forced participation. Theater ratio at 0.10 reflects that the option development is genuine engineering work, not performative; Starship flight tests, Starlink deployment, and Raptor engine iteration are observable milestones. Accessibility collapse at 0.40 reflects that the specific option space (reusable heavy lift + satellite constellation + planetary infrastructure) has no close substitute, but investors have alternative asset classes. Resistance at 0.20 reflects minor pushback from governance advocates and DCF purists, not systemic opposition.
 *
 * PERSPECTIVAL GAP:
 *   The elon_musk seat (agenda_setter, institutional, arbitrage exit) experiences this as genuine coordination — he controls the option portfolio and bears founder risk. The spacex_investors seat (beneficiary/payer, organized, constrained exit) experiences it as asymmetric risk/reward they voluntarily accept. The minority_shareholders seat (if they exist as a distinct class) would experience higher effective extraction due to governance powerlessness — but this reading asserts they are not a distinct victim class because equity terms are disclosed. The humanity_future seat (excluded) experiences pure upside optionality with zero current cost. The engine computes per-seat χ from these structural positions; the divergence between agenda_setter (subsidy) and payer (extraction) seats is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   elon_musk is structural beneficiary (d ~ 0.1): controls the constraint, collects upside, bears downside via equity concentration, has arbitrage-grade exit (can sell secondary, pledged shares). spacex_institutional_investors are near-symmetric (d ~ 0.5): pay for option portfolio, receive pro-rata upside, constrained exit (secondary market liquidity windows). spacex_employees are beneficiaries (d ~ 0.2): equity upside with mission alignment, constrained exit (vesting schedules, lockups). minority_shareholders (if distinct) would be targets (d ~ 0.7): same downside, no governance voice, trapped exit — but this reading does not posit them as a distinct victim class. humanity_future is ultimate beneficiary (d ~ 0.0): civilizational option value at zero current cost. The derivation chain uses beneficiary declarations + exit options; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — capital-intensive space development without exclusive government funding — remains live (Starship, orbital compute, lunar/Mars infrastructure still require massive capital). The arrangement has not outlived its function; if anything, the option portfolio has expanded. No mandatrophy resolution needed. The governance_skeptic reading would argue mandatrophy occurred when Musk's control structure persisted beyond the coordination necessity, but this reading rejects that frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_real_options_technologist,
    'This constraint is one reading (real_options_technologist) of the contested kernel valuation_legitimacy. Sibling readings: dcf_fundamentalist, musk_cult_believer, governance_skeptic. What structural elements do readings disagree on?',
    'Compare each reading''s beneficiary/victim structure, claimed_type, and extractiveness referent. The disagreement is located in: (1) what counts as legitimate valuation basis (cash flows vs option space vs founder track record vs governance), (2) whether Musk''s control structure is coordination or extraction, (3) whether unproven technologies are assets or options.',
    'If governance_skeptic''s extraction claim holds, this constraint reclassifies from rope to tangled_rope or snare with minority_shareholders as victims. If dcf_fundamentalist''s referent holds, extractiveness rises as option value collapses to near-zero. If musk_cult_believer holds, extractiveness falls further as track record validates option pricing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_real_options_technologist, conceptual, 'Commitment kernel valuation_legitimacy with four contested readings; this story instantiates real_options_technologist only.').

omega_variable(
    option_value_referent_ambiguity,
    'Is the $1.75T valuation''s ~6% probability of $28.5T TAM a genuine market-implied probability or a narrative-driven premium?',
    'Decompose valuation into Starlink DCF ($7.2B EBITDA at ~25x = ~$180B) plus option value of Starship/orbital compute/lunar/Mars. If residual option value exceeds what Black-Scholes-type models justify given technical milestones, narrative premium exists.',
    'If narrative premium > 30% of valuation, extractiveness underestimates transfer from later investors to earlier insiders; constraint drifts toward tangled_rope. If option value is model-justified, rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(option_value_referent_ambiguity, empirical, 'Whether the option-value component of valuation is structurally justified or narrative-inflated.').

omega_variable(
    vertical_integration_coordination_vs_capture,
    'Does vertical integration (Starlink launch, Starship, satellites, user terminals, ground stations) create genuine compounding optionality, or does it create captive markets that extract from dependent segments?',
    'Compare internal transfer pricing vs market benchmarks for each vertical segment. If internal prices consistently favor the integrating entity (SpaceX launch vs external launch providers), coordination masks extraction.',
    'If captive-market extraction detected, beneficiary set narrows to Musk/insiders, victim set expands to Starlink subscribers/terminal buyers; constraint reclassifies to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vertical_integration_coordination_vs_capture, empirical, 'Whether vertical integration''s optionality is genuine coordination or structural capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valuation_legitimacy_real_options_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.08).
narrative_ontology:measurement(valuation_legitimacy_real_options_tr_t5, valuation_legitimacy__real_options_technologist, theater_ratio, 5, 0.09).
narrative_ontology:measurement(valuation_legitimacy_real_options_tr_t10, valuation_legitimacy__real_options_technologist, theater_ratio, 10, 0.09).
narrative_ontology:measurement(valuation_legitimacy_real_options_tr_t15, valuation_legitimacy__real_options_technologist, theater_ratio, 15, 0.1).
narrative_ontology:measurement(valuation_legitimacy_real_options_tr_t20, valuation_legitimacy__real_options_technologist, theater_ratio, 20, 0.1).
narrative_ontology:measurement(valuation_legitimacy_real_options_tr_t25, valuation_legitimacy__real_options_technologist, theater_ratio, 25, 0.1).

% Extraction over time
narrative_ontology:measurement(valuation_legitimacy_real_options_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(valuation_legitimacy_real_options_be_t5, valuation_legitimacy__real_options_technologist, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(valuation_legitimacy_real_options_be_t10, valuation_legitimacy__real_options_technologist, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(valuation_legitimacy_real_options_be_t15, valuation_legitimacy__real_options_technologist, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(valuation_legitimacy_real_options_be_t20, valuation_legitimacy__real_options_technologist, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(valuation_legitimacy_real_options_be_t25, valuation_legitimacy__real_options_technologist, base_extractiveness, 25, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(valuation_legitimacy__real_options_technologist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__real_options_technologist, 0.15).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, spacex_governance_structure).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, starlink_spectrum_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, starship_development_funding).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, orbital_compute_power_market).

% DUAL FORMULATION NOTE:
% Part of valuation_legitimacy kernel family with dcf_fundamentalist, musk_cult_believer, governance_skeptic. This reading prices option space; dcf_fundamentalist prices cash flows; governance_skeptic prices control; musk_cult_believer prices founder optionality. All four constrain the same equity but with different ε and different victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
