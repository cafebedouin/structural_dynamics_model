% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Real Options Valuation Legitimacy (Technology Portfolio Frame)
 *   domain: corporate_finance/technology_governance
 *
 * SUMMARY:
 *   SpaceX's ~$1.75T valuation under the real-options reading is framed as
 *   legitimate by present value of technological option space: Starlink
 *   (proven, $7.2B EBITDA), Starship (high-variance, enables downstream
 *   options), orbital compute (addresses 62 GW U.S. power gap), lunar economy
 *   (first-mover advantage), Mars (civilizational hedge). Vertical
 *   integration means success in any segment increases probability of others.
 *   The constraint legitimizes investing in optionality rather than near-term
 *   cash flow. The real-options reading competes with three sibling readings:
 *   DCF fundamentalist (only proven cash flows count), governance skeptic
 *   (Musk's control is extractive, not value-creating), and Musk cult
 *   believer (Musk's track record justifies faith in impossible goals). This
 *   JSON instantiates ONLY the real-options reading as a clean constraint—the
 *   competing readings are other constraint stories in the kernel family,
 *   linked by network.affects_constraints. The claim/metric gap is
 *   intentional: this reading claims the constraint is rope (genuine
 *   coordination of long-term capital formation), while the metrics show
 *   moderate extractiveness (0.42, rising to stated endpoint) and non-trivial
 *   suppression (governance concentration), indicating tension between the
 *   coordination function and the exercise of Musk's governing power.
 *
 * KEY AGENTS:
 *   - real_options_investor_cohort — Organized beneficiary (understand and accept the real-options frame; exit is mobile)
 *   - musk_as_agenda_setter — Institutional agenda-setter (controls frame-setting and capital allocation; identity-locked to SpaceX mission)
 *   - dcf_fundamentalist_investor_cohort — Excluded (reject optionality framing; would demand lower valuation or governance constraints)
 *   - governance_skeptic_minority_shareholder — Excluded (demand shareholder protection; object to option-overweighting under Musk's control)
 *   - technology_ecosystem_downstream — Analytical beneficiary (benefits if multiplanetary civilization succeeds; no seat at valuation table)
 *   - capital_market_mechanism — Observer (observes constraint through price discovery; tests option values against outcomes)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.42).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.28).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.42).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real Options Valuation Legitimacy (Technology Portfolio Frame)").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "corporate_finance/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, 'cd96528a-be3a-4e58-b107-c171519f2dd8').
narrative_ontology:cs_kernel_codification('cd96528a-be3a-4e58-b107-c171519f2dd8', distributed).
narrative_ontology:cs_authority_grounding('cd96528a-be3a-4e58-b107-c171519f2dd8', expertise).
narrative_ontology:cs_interpretation_layer_present('cd96528a-be3a-4e58-b107-c171519f2dd8').
narrative_ontology:cs_reading_relation('cd96528a-be3a-4e58-b107-c171519f2dd8', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('cd96528a-be3a-4e58-b107-c171519f2dd8', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_reading_relation('cd96528a-be3a-4e58-b107-c171519f2dd8', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_axiom('cd96528a-be3a-4e58-b107-c171519f2dd8', foundational, optionality_is_fundamental_value).
narrative_ontology:cs_axiom_status(optionality_is_fundamental_value, holdable).
narrative_ontology:cs_axiom_grounding('cd96528a-be3a-4e58-b107-c171519f2dd8', optionality_is_fundamental_value, empirically_contingent).
narrative_ontology:cs_axiom('cd96528a-be3a-4e58-b107-c171519f2dd8', foundational, vertical_integration_compounds_option_value).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_option_value, holdable).
narrative_ontology:cs_axiom_grounding('cd96528a-be3a-4e58-b107-c171519f2dd8', vertical_integration_compounds_option_value, empirically_contingent).
narrative_ontology:cs_reference_frame('cd96528a-be3a-4e58-b107-c171519f2dd8', real_options_portfolio_legitimacy).
narrative_ontology:cs_drift_state('cd96528a-be3a-4e58-b107-c171519f2dd8', contemporary_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cd96528a-be3a-4e58-b107-c171519f2dd8', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, humanity_if_multiplanetary).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, long_term_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, technology_ecosystem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, real_options_investor_cohort).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, technology_ecosystem_downstream).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Investors who understand and endorse the real-options framing: they model the constraint as legitimate valuation—that SpaceX's worth derives from the portfolio of technological options (Starlink proven revenue, Starship enabling downstream markets, orbital compute addressing power gaps, lunar economy first-mover advantage, Mars civilizational hedge). They exit freely by selling equity or shifting portfolio weight; their participation is voluntary. They understand the constraint as pricing in ~6% probability of $28.5T TAM achievement.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, real_options_investor_cohort, beneficiary,
    organized, generational, mobile, global).

% The broader technology and space ecosystem benefits if multiplanetary civilization becomes real: access to orbital compute (addressing 62 GW U.S. power gap), lunar resources, Mars as civilizational hedge. The constraint legitimizes investment in the option space that produces these public goods. Their benefit is prospective and depends on technical success; they have no seat at the valuation table but are the ultimate beneficiaries if the portfolio succeeds.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, technology_ecosystem_downstream, beneficiary,
    analytical, civilizational, analytical, global).

% Investors who reject the real-options framing and hold only to proven cash flows (Starlink $7.2B EBITDA, mission revenue). They argue unproven technologies should not inflate valuation and that the ~$1.75T valuation is divorced from present value of contracted business. They exclude themselves or are excluded from the discourse about legitimate valuation by framing incompatibility—they would demand lower valuation or governance constraints on option-priced assets.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, dcf_fundamentalist_investor_cohort, excluded,
    organized, biographical, mobile, global).

% Minority shareholders and governance advocates who argue that Musk's 82.4% voting control with only 42% economic interest violates standard shareholder protection norms. They would object to any valuation framework that legitimizes unrestricted option-priced investment under dominant control (no board veto, no option-spending caps). They are structurally excluded from governance that would constrain the constraint itself.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, governance_skeptic_minority_shareholder, excluded,
    moderate, biographical, constrained, global).

% Musk sets the valuation frame and enforces it through control: he declares the options legitimate, allocates capital to maximize option value (Starship R&D, orbital compute exploration, Mars planning), and manages investor communication around the options portfolio. His identity is fused with SpaceX's mission and technological ambition; exit is impossible. He controls enough votes to override shareholder objection to option-spending or governance reform.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, musk_as_agenda_setter, agenda_setter,
    institutional, generational, identity_locked, global).

% The mechanism by which valuation is set: stock price discovery, institutional investor positioning, analyst models. The capital market observes the constraint and tests it—if option values consistently fail to materialize, prices compress and the constraint weakens. If options succeed, valuations are vindicated. The market observes but does not enforce; enforcement is Musk's governing act.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, capital_market_mechanism, observer,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__real_options_technologist, capital_market_mechanism).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-term capital formation for high-variance, multi-decade technology portfolios by establishing a valuation frame (real-options theory) that legitimizes investing in portfolio optionality rather than near-term cash flow. Solves the capital-allocation problem: how do you fund Starship R&D and orbital compute while earning revenue from Starlink? Real-options framing answers: you value the entire portfolio, price in option success probabilities, and allocate capital to maximize option value creation.
% TRANSFER_FUNCTION: Transfers capital from investors who accept the real-options framing to the technology portfolio (Starlink operations, Starship development, orbital compute research, lunar/Mars exploration). The transfer is legitimized by the constraint: investors agree their capital is pricing optionality, not current earnings. The constraint also transfers governance power from ownership fraction to voting control—Musk holds 82.4% votes with 42% equity, concentrating capital-allocation authority.
% ABSENT_VOICES: DCF fundamentalist investors and governance-protection advocates are absent from the constraint's legitimacy frame. Fundamentalists argue unproven tech should not inflate valuation; governance skeptics argue minority shareholders need protection against option-overweighting. Neither group participates in setting the valuation frame—they are excluded by incompatible epistemology (options vs. cash flows) and by governance structure (minority votes cannot override option-spending).
% DISAPPEARANCE_RATIONALE: If the real-options valuation frame disappeared, SpaceX's capital structure would reorganize radically: either (1) valuation compresses to near Starlink cash flow (~$35–50B range), forcing abandonment of Starship and option-portfolio investment, OR (2) governance restructures to constrain Musk's unilateral option-spending authority, requiring board approval for R&D budgets or spinoff of option-heavy divisions. The constraint is not a natural feature; it is the current state of investor consensus and Musk's governing act. Its disappearance would force immediate capital reallocation.
% FOUNDING_PROBLEM: High-variance, long-duration technology portfolios (space access, orbital infrastructure, multiplanetary civilization) require capital and governance structures that tolerate decade-scale R&D, technical failure, and optionality. Traditional DCF valuation and dispersed shareholder governance are inadequate—they demand near-term returns and constrain risk-taking. Real-options framing solves this by legitimizing option-based valuation and concentrated governance for decisive capital allocation.
% FOUNDING_PROBLEM_CORROBORATION: Real-options theory (Stewart Myers, Avinash Dixit, Robert Pindyck) is peer-reviewed and taught in finance; it is attested by academic literature and technology-sector practitioners. However, the APPLICATION of real-options framing to SpaceX's specific valuation is contested: DCF fundamentalists and governance skeptics argue the founding problem (need for long-term, high-variance capital structures) does NOT require that SpaceX's current ~$1.75T valuation is legitimate—it only requires that SOME form of option-friendly governance exists, not that Musk's unilateral frame-setting is the necessary or only solution. Independent corroboration of the founding problem exists; corroboration of this specific constraint's necessity does not.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42 endpoint) and rising, not because the real-options framing is false, but because the constraint carries a secondary governance extraction: Musk's 82.4% voting control with 42% equity interest allows option-spending without shareholder veto. The optionality framing is legitimate (real-options theory is peer-reviewed), but the governance structure concentrates capital allocation authority. Theater is low (0.15) because the optionality is genuinely real—Starlink $7.2B EBITDA, Starship flight tests, orbital compute research are all material. Suppression is moderate (0.28) and rises slightly: DCF fundamentalists and governance skeptics are not coerced into accepting the frame, but they are excluded from the discourse by incompatible epistemology (options vs. cash flows) and by governance structure (minority votes cannot constrain option-spending). The measurement series show a plateau around t=15, reflecting stabilization of the constraint once option portfolios mature and investor consensus settles. All metrics are authored on a single shared time grid (t=0,5,10,15,20,25); observed/projected basis marks observed data (t=0–10) versus authorial projection for future evolution (t=15–25).
 *
 * PERSPECTIVAL GAP:
 *   The real-options investor and the DCF fundamentalist sit at opposite poles. From the real-options seat, the constraint is genuine coordination—it solves the capital-formation problem for portfolios with long duration and high variance. The legitimacy is epistemic: real-options theory correctly prices optionality. From the DCF seat, the same constraint is a cover story for option-overweighting that ignores proven cash flows. The governance skeptic sees extraction: Musk's voting control allows him to allocate capital to speculative ventures (Mars, lunar) while remaining insulated from minority shareholder constraint. These divergences are structural, not perceptual—they follow from the different epistemologies (option value vs. discounted cash flow) and the governance asymmetry (voting control concentrated, economic interest diffuse). The engine computes per-seat classifications from the structural data; this commentary explains why the seated disagreement is real, not rhetorical.
 *
 * DIRECTIONALITY LOGIC:
 *   The real-options investor is a net beneficiary (d near 0.2–0.3): they participate voluntarily, their capital is pricing optionality, and they exit freely by selling. Musk is the primary beneficiary at the governance level (d near 0.1): he sets the frame and allocates capital with minimal constraint. The technology ecosystem is a conditional beneficiary (d near 0.2): they benefit if options succeed, but have no seat at the valuation table. DCF fundamentalists and governance skeptics are targets in a softer sense (d near 0.7–0.8): their dissent is excluded by framing incompatibility; if they hold equity, they bear option-downside risk without having constrained option-upside. The governance skeptic specifically bears the extraction of voting-control asymmetry. Suppression is achieved not through coercion (no one is forced to hold equity) but through exclusion and framing: the discourse is set in real-options terms; those who reject optionality are outside the conversation. This is structural suppression, not coercive suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint sits at the boundary between genuine coordination and governance extraction. The founding problem (capital formation for high-variance, long-duration portfolios) is real and unsolved by traditional DCF + dispersed governance. Real-options framing solves it. However, the specific solution (Musk's frame-setting + voting control) carries secondary extraction: the governance asymmetry allows option-overweighting without shareholder constraint. The constraint has NOT reached mandatrophy (the founding problem is still live—SpaceX continues to invest in long-duration optionality and faces ongoing capital-formation challenges). The question is whether the constraint is rope (genuine coordination with minimal overhead) or tangled rope (coordination + asymmetric extraction). The metrics push toward tangled rope: extractiveness is moderate, suppression is non-trivial, and theater is low but rising. The sibling readings (DCF, governance skeptic, Musk cult) all propose different solutions—each would reframe the constraint to resolve the coordination/extraction boundary differently. None of the readings has mandatrophy resolved; all are live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    option_value_realization_risk,
    'What is the probability that the priced-in options (orbital compute, lunar economy, Mars) actually materialize with the implied $28.5T TAM, or what fraction collapses to near-zero value?',
    '20–30 year track record: do option markets pay out at rates consistent with ~6% success probability, or do they miss by orders of magnitude? Technical milestones (orbital refueling, payload capacity, Starship reusability) provide intermediate tests.',
    'If option success rates match the priced probability (~6%), the real-options frame is vindicated and extractiveness remains moderate. If success rates are much lower (>90% failure), the valuation is bubble-priced and the constraint becomes false legitimacy (snare). If much higher (<1% failure), the constraint is under-extracting and should be re-priced upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(option_value_realization_risk, empirical, 'Whether option values track implied success probabilities or diverge radically.').

omega_variable(
    governance_concentration_necessity,
    'Is the real-options coordination function structurally dependent on Musk''s voting-control concentration (82.4% votes, 42% equity), or could equivalent capital allocation occur under democratic-governance constraints (board veto, shareholder approval of R&D budgets)?',
    'Counterfactual comparison: do other high-R&D, long-duration companies (Tesla, Amazon) achieve equivalent optionality under dispersed governance? Do regulatory mandates for shareholder protection (EU governance codes, SEC proposals on executive compensation) compress innovation in option-dependent sectors?',
    'If governance concentration is NOT necessary, the secondary extraction (voting-control asymmetry) is pure rent-seeking, and the constraint becomes tangled_rope with higher measured extractiveness and suppression. If concentration IS necessary (board veto kills option-spending on low-probability ventures), the constraint''s governance is not extraction but necessary decisiveness, and extractiveness declines to ~0.20–0.25.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_concentration_necessity, conceptual, 'Whether the real-options frame requires concentrated governance or is compatible with democratic constraints.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Can the real-options reading and the DCF fundamentalist reading coexist within a single framework, or does accepting real-options theory logically foreclose the DCF reading?',
    'Theory analysis: real-options is a GENERALIZATION of DCF that includes option value as an addition to cash-flow value. DCF values CERTAIN cash flows; real-options values uncertain outcomes. DCF is not wrong; it is incomplete when assets hold optionality. The readings should coexist if SpaceX''s value can be decomposed (Starlink cash flows + Starship option value), or foreclose if the readings disagree on what optionality IS (fundamental value vs. bubble premium).',
    'If they coexist, the kernel dispute is epistemic (different bases for valuation) but not mutually exclusive—investors can price both cash flows AND options. If they foreclose, the kernel dispute is fundamental (one reading logically denies the other''s core claim), and the readings cannot coexist in a single framework—this investor must choose one frame entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether real-options and DCF are compatible frames or mutually exclusive.').

omega_variable(
    temporal_option_decay_vs_compounding,
    'As Starlink matures (cash flows stabilize), does the option value of vertical integration COMPOUND (as this reading claims: Starlink revenue funds Starship, which enables orbital compute, etc.) or DECAY (as fundamentalists argue: proved revenue shrinks option value, because capital is deployed to lower-probability ventures)?',
    'Measurement over 10–15 years: does Starlink revenue growth fund Starship advances and new venture launch, with success metrics improving? Or does Starlink''s cash flow get diverted to speculation with diminishing returns?',
    'Compounding would vindicate the real-options frame and validate vertical integration as value-creative; decay would support the governance skeptic''s claim that Musk deploys cash to speculative ventures under insufficient shareholder constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_option_decay_vs_compounding, empirical, 'Whether vertical integration creates compounding optionality or merely cascading speculation.').

omega_variable(
    kernel_reading_identity_lock,
    'Is the real-options reading sustained by genuine theoretical conviction (real-options theory accurately prices optionality) or by identity fusion with Musk''s mission narrative (believers are identity-locked to the ''multiplanetary'' framing regardless of evidence)?',
    'Observation of investor behavior under two conditions: (1) option success (Starship reusability achieved, orbital compute contracts signed) — do believers maintain the reading? (2) option failure (Mars timelines slip, orbital compute uncompetitive) — do they revise, or double down on the narrative? Identity-locked believers revise slowly or not at all; theoretically convinced investors update on evidence.',
    'If identity-locked, the reading is sustained by narrative adhesion, not by structural validation. The suppression measure may undercount — believers exclude fundamentalist voices not because of framing incompatibility alone but because identity fusion makes dissent feel like apostasy. If theoretically convinced, the reading is sustained by epistemic confidence, and the suppression measure is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_lock, empirical, 'Whether the real-options reading is theory-driven or narrative-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.08).
narrative_ontology:measurement(valu_tr_t5, valuation_legitimacy__real_options_technologist, theater_ratio, 5, 0.1).
narrative_ontology:measurement(valu_tr_t10, valuation_legitimacy__real_options_technologist, theater_ratio, 10, 0.13).
narrative_ontology:measurement(valu_tr_t15, valuation_legitimacy__real_options_technologist, theater_ratio, 15, 0.15).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__real_options_technologist, theater_ratio, 20, 0.15).
narrative_ontology:measurement(valu_tr_t25, valuation_legitimacy__real_options_technologist, theater_ratio, 25, 0.15).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(valu_be_t5, valuation_legitimacy__real_options_technologist, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(valu_be_t10, valuation_legitimacy__real_options_technologist, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(valu_be_t15, valuation_legitimacy__real_options_technologist, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__real_options_technologist, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(valu_be_t25, valuation_legitimacy__real_options_technologist, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__real_options_technologist, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(valu_su_t5, valuation_legitimacy__real_options_technologist, suppression_requirement, 5, 0.21).
narrative_ontology:measurement(valu_su_t10, valuation_legitimacy__real_options_technologist, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(valu_su_t15, valuation_legitimacy__real_options_technologist, suppression_requirement, 15, 0.27).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__real_options_technologist, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(valu_su_t25, valuation_legitimacy__real_options_technologist, suppression_requirement, 25, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__real_options_technologist, 0.18).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% The kernel 'valuation_legitimacy' decomposes into four reading-based constraints, each with a distinct ε, distinct beneficiary/victim structure, and distinct epistemic grounding. This story instantiates the real_options_technologist reading. Sibling readings (dcf_fundamentalist, governance_skeptic, musk_cult_believer) are authored as separate constraint stories with their own ε values and measurement series. The kernel itself is not a constraint; the readings are. Each reading applied to SpaceX's $1.75T valuation yields a different structural classification (this reading is rope, governance_skeptic is snare, dcf_fundamentalist is implicit mountain, musk_cult_believer is piton-candidate). The network edges establish the reading family and enable cross-kernel analysis of which readings converge on similar metrics and which radically diverge. See 'kernel_context' in commentary for full framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
