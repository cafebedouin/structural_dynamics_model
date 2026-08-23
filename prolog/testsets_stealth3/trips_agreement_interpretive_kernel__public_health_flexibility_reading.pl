% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Architecture (Broad Compulsory Licensing and Parallel Import Reading)
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the public-health-flexibility reading of the
 *   TRIPS interpretive kernel: the standing arrangement under contest is the
 *   TRIPS patent regime as conditioned by broad compulsory-licensing and
 *   parallel-import flexibilities (Articles 6, 30, 31, 31bis, fixed
 *   politically by the 2001 Doha Declaration, the 2003 waiver, and the 2005
 *   amendment). Per the ε-referent rule for kernel readings, extractiveness
 *   is authored for THAT standing arrangement as this reading construes it —
 *   never for the strong-exclusivity arrangement a sibling reading would
 *   instantiate. The reading assesses the arrangement as a genuine
 *   coordination device (a lawful, procedurally bounded pathway for securing
 *   medicine access) that nonetheless deliberately transfers substantial
 *   value from patent holders to generic producers, health systems, and
 *   patients; the transfer is bounded by remuneration obligations and by the
 *   fact that most entitlements remain unexercised. The claim (tangled_rope)
 *   and the metrics are authored independently: the metrics describe how the
 *   arrangement actually operates, and the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - generic_pharmaceutical_manufacturers: primary beneficiary (organized/mobile) — collects market entry and margin when licenses issue; pivots freely across portfolios
 *   - - developing_country_health_ministries: primary beneficiary (institutional/constrained) — converts flexibilities into negotiating leverage and lawful fallback
 *   - - low_income_patients_in_importing_countries: passive beneficiary (powerless/trapped) — captures price reductions with no independent exit or voice
 *   - - global_health_procurement_programs: aggregated beneficiary (organized/mobile) — pools demand and arbitrates supplier volume
 *   - - originator_pharmaceutical_patent_holders: primary target (powerful/constrained) — bears exclusivity erosion; maneuvers via bilateralism and voluntary licensing
 *   - - small_biotech_innovators: concentrated target (moderate/trapped) — single-asset exposure makes license risk existential
 *   - - pharma_exporting_developed_states: dual-positioned payer/agenda-setter (institutional/constrained) — bears revenue losses while shaping the TRIPS-plus counter-agenda
 *   - - wto_trips_council: agenda-setter (institutional/constrained) — administers and politically maintains the reading under consensus rules
 *   - - wto_dispute_settlement_body: analytical observer (institutional/analytical) — adjudicates the reading's practical reach
 *   - - civil_society_access_movements: mobilizing beneficiary (organized/mobile) — monitors, litigates-supports, and sustains the reading's political salience
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.62).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.55).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Architecture (Broad Compulsory Licensing and Parallel Import Reading)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '1d465ccb-6935-4380-838c-87a082674af9').
narrative_ontology:cs_kernel_codification('1d465ccb-6935-4380-838c-87a082674af9', fixed_text).
narrative_ontology:cs_authority_grounding('1d465ccb-6935-4380-838c-87a082674af9', lineage).
narrative_ontology:cs_interpretation_layer_present('1d465ccb-6935-4380-838c-87a082674af9').
narrative_ontology:cs_reading_relation('1d465ccb-6935-4380-838c-87a082674af9', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d465ccb-6935-4380-838c-87a082674af9', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('1d465ccb-6935-4380-838c-87a082674af9', foundational, public_health_measures_override_patent_exclusivity).
narrative_ontology:cs_axiom_status(public_health_measures_override_patent_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('1d465ccb-6935-4380-838c-87a082674af9', public_health_measures_override_patent_exclusivity, deontological).
narrative_ontology:cs_axiom('1d465ccb-6935-4380-838c-87a082674af9', foundational, flexibilities_are_integral_bargain_terms).
narrative_ontology:cs_axiom_status(flexibilities_are_integral_bargain_terms, holdable).
narrative_ontology:cs_axiom_grounding('1d465ccb-6935-4380-838c-87a082674af9', flexibilities_are_integral_bargain_terms, conventional).
narrative_ontology:cs_reference_frame('1d465ccb-6935-4380-838c-87a082674af9', doha_balanced_text).
narrative_ontology:cs_drift_state('1d465ccb-6935-4380-838c-87a082674af9', contemporary_post_covid_waiver_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1d465ccb-6935-4380-838c-87a082674af9', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developing_country_health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_patients_in_importing_countries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, global_health_procurement_programs).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, small_biotech_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, civil_society_access_movements).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharma_exporting_developed_states).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, doha_declaration_on_trips_and_public_health).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, right_to_health_norm).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, international_exhaustion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and export off-patent and compulsorily licensed medicines, primarily from Indian, Brazilian, and similar manufacturing bases. When a government issues a compulsory license or permits parallel importation, these firms gain market entry and margin that would otherwise flow to the patent holder, paying negotiated remuneration back under the licensing procedure. Exit is comparatively open: portfolios can pivot to off-patent products, other therapeutic areas, or regulated-market generics.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Procure medicines against hard budget ceilings and recurrent epidemic burdens. The flexibilities give them standing legal leverage in price negotiations with originator companies and a lawful fallback when negotiated prices fail. Their room to maneuver is bounded by donor funding conditions, fear of trade friction, and thin domestic regulatory capacity.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developing_country_health_ministries, beneficiary,
    institutional, biographical, constrained, national).

% Depend on affordable medicines for ongoing treatment and capture the price reductions that generic competition produces. They have no exit from disease burden or geography and participate only through the ministries and procurement programs that act on their behalf.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_patients_in_importing_countries, beneficiary,
    powerless, immediate, trapped, regional).

% Pooled buyers such as global health funds and regional procurement agencies rely on generic competition to drive price erosion across member countries. They source globally and can shift volume between suppliers, which keeps their position flexible.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, global_health_procurement_programs, beneficiary,
    organized, biographical, mobile, global).

% Research-based companies holding medicine patents. Where licenses issue or parallel imports flow, they lose exclusivity rents and face price erosion in affected markets. They retain remuneration rights under the licensing procedure and respond through voluntary licensing on their own terms, bilateral treaty provisions that narrow flexibilities, and portfolio choices. They cannot exit reliance on patent systems generally, but they maneuver extensively around this particular arrangement.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders, payer,
    powerful, generational, constrained, global).

% Single-product or narrow-portfolio developers whose company valuation rests almost entirely on patent exclusivity in a lead asset. A compulsory license covering that asset can collapse financing rounds outright. They have no diversification exit and little voice in trade fora dominated by larger incumbents and state coalitions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, small_biotech_innovators, payer,
    moderate, biographical, trapped, global).

% Home states of the research-based industry. They lose export revenue and royalty flows when flexibilities are exercised and press back through unilateral watch lists, bilateral agreement provisions that narrow compulsory-license grounds, and dispute threats. The same states also negotiate the multilateral text, so they shape the counter-agenda as well as bearing its costs.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharma_exporting_developed_states, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharma_exporting_developed_states, agenda_setter).

% Standing body of the WTO membership that administers the agreement: reviews licensing notifications, hosts the implementation agenda, and is the venue where the flexibility reading is politically maintained. Bound by consensus rules, it cannot readily alter the text it administers.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_trips_council, agenda_setter,
    institutional, generational, constrained, global).

% Panels and appellate review that adjudicate disputes touching the flexibilities. Their rulings condition how far the reading reaches in practice. They take testimony and argument from the other seats and decide; they neither collect nor pay under the arrangement.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_settlement_body, observer,
    institutional, generational, analytical, global).

% Transnational advocacy networks that campaigned for the Doha clarification and continue to document access gaps, support governments considering licenses, and contest bilateral provisions that narrow flexibilities. They advance their mission and sustain funding relevance through the arrangement's maintenance.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, civil_society_access_movements, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__public_health_flexibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a multilateral, pre-agreed legal pathway for governments to authorize generic production or importation of patented medicines during health crises, converting ad hoc access-versus-patent confrontations into a known procedure with remuneration obligations — solving the collective-action problem of medicine access without triggering cycles of unilateral defiance and trade retaliation.
% TRANSFER_FUNCTION: Moves pricing power and market-exclusivity value from originator patent holders to generic manufacturers, health systems, and patients in importing countries; moves remuneration payments from importing governments and generic producers back to patent holders at rates below monopoly prices.
% ABSENT_VOICES: Patients in countries lacking domestic manufacturing whose access runs through the cumbersome Article 31bis export mechanism were thinly represented when it was designed and remain the group worst served by it. Small biotech innovators hold a nominal seat but carry little weight in trade fora. Future patients whose treatments depend on innovation incentives the flexibilities erode have no seat at all — their interest is voiced only secondhand by exporting states and industry associations.
% DISAPPEARANCE_RATIONALE: If the flexibilities vanished overnight, health ministries would lose their lawful fallback and negotiating leverage, generic supply chains built on licenses would unwind into dependence on voluntary licensing offered at originator discretion, medicine prices in middle-income markets would climb, and access conflicts would migrate to unilateral patent disregard met with retaliation — the trade-and-health settlement would rearrange around coercion rather than procedure.
% FOUNDING_PROBLEM: The 1994 TRIPS settlement extended twenty-year product patents on medicines worldwide without adequate provision for public health emergencies; the flexibility reading crystallized after the South African medicines litigation (1998–2001) and the Doha Declaration (2001) to restore governments' capacity to secure affordable medicines during epidemics.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Doha Declaration was adopted by consensus of the full WTO membership including the exporting states, conceding the problem's reality; WHO and UNAIDS epidemiological reporting documents recurring access failures independent of generic-industry advocacy; and the 2020–2022 waiver negotiation record shows even opponent states engaging the access problem rather than denying it. No corroborating source denies the founding problem — the contest is over its remedy, not its existence.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the arrangement deliberately moves exclusivity value from patent holders — that is its function — but bounded because remuneration obligations and procedural requirements cap the transfer short of confiscation. Suppression (0.55) is moderate: the arrangement coerces originators within multilateral space (they cannot opt out of the treaty regime) while leaving them partial bilateral escape routes, and its own exercise is chilled by external trade pressure; the scalar records the net coercive texture without separating mechanisms (see the suppression-attribution omega). Theater ratio (0.55) is the most diagnostic score: formal availability vastly exceeds actual use — compulsory licenses number in the dozens across two decades, the Article 31bis export mechanism sat essentially dormant for years after adoption, and much of the machinery operates as negotiating leverage rather than invoked instrument. Accessibility collapse (0.40) is low-for-a-constraint because alternatives persist: the least-developed-country pharmaceutical waiver runs to 2033, voluntary licensing and pooled procurement operate alongside, and unilateral defiance remains a (costly) option. Resistance (0.68) is high: two decades of TRIPS-plus bilateralism, unilateral watch lists, and dispute threats constitute continuous organized counter-pressure. The temporal series run on one shared grid (t=0..24 maps to 2001..2025: Doha, the 31bis amendment, the TRIPS-plus FTA wave, the Thailand/Colombia/Malaysia license episodes, the COVID waiver fight) with all three metrics authored at every point. Suppression_requirement is tracked because the story's central dynamic is enforcement-capacity change: the buildout of bilateral narrowing machinery raised the force needed to keep flexibility exercise down, with a visible crisis-cycle oscillation — crisis produces a flexibility-revival spike (t=20, the COVID waiver fight), followed by counter-pressure and renewed chill (t=24 settles back). The oscillation is partly the extraction mechanism itself: intermittent reinforcement teaches ministries that entitlements are usable only in emergencies, depressing routine use.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute sharply different types from identical structural data. From the originator seat the arrangement is expropriation-flavored: value taken from a portfolio by political act, with remuneration set by the taker. From the ministry and patient seats the same structure is coordination: a pre-agreed procedure replacing coercive improvisation. Small biotech sits furthest toward the target pole because trapped exit concentrates the burden; originators' constrained-but-real maneuvering (bilateralism, voluntary licensing, portfolio shifts) moderates their effective burden below what a trapped payer would bear. Among same-power institutional seats, divergence is role-driven: exporting states both pay and set the counter-agenda, the TRIPS Council administers without capturing, and the dispute-settlement body observes analytically. An identity-lock dynamic runs through the originator seat: the industry's institutional self-concept is fused with exclusivity-as-incentive doctrine, making remuneration-based models feel like category error rather than price negotiation; if that frame broke, the computed extraction burden on that seat would fall materially even with no textual change.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation without overrides. Beneficiaries order by passivity: patients (powerless, trapped, no independent action) sit nearest the full-beneficiary end; ministries (institutional, constrained, but actively invoking) sit somewhat higher; generics and procurement programs (mobile, organized) sit higher still because their gains are contingent on exercise they can pivot away from. Payers order by exit: small biotech (trapped, single-asset) sits nearest the full-target end; originators (powerful, constrained) sit high but not maximal because bilateral and voluntary-licensing routes absorb part of the burden; exporting states derive high d as payers, moderated by their agenda-setting counter-leverage. No directionality_overrides entries are authored: the derivation chain already distinguishes these seats through role, power, and exit declarations, and an override keyed on a shared power atom (e.g., institutional) would mis-collateralize onto unrelated seats such as the TRIPS Council.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents two symmetric misreadings. Scored as pure coordination (rope), the arrangement's real extraction from patent holders — deliberate, structured, and growing over the interval — disappears into a harmony story. Scored as pure extraction (snare), the genuine collective-action function vanishes: without a pre-agreed flexibility procedure, medicine-access conflicts revert to unilateral defiance met with retaliation, which is strictly worse for every seat including the originators. The founding problem remains live (recurrent pandemics keep the access problem on the agenda), and the founding-status-by-disappearance pairing (live + world_rearranges) raises no zombie flag: the arrangement persists because the problem persists, not because anyone is administering a corpse. The rising theater ratio is the drift signal to watch — if formal availability continues to decouple from use, the arrangement slides toward inertial maintenance even while the underlying problem stays live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the public_health_flexibility_reading of trips_agreement_interpretive_kernel; what structural facts change if the strong_exclusivity_reading is instantiated instead?',
    'Comparative instantiation: compile the sibling story and diff the beneficiary/victim sets and epsilon against this file; the disagreement is located in the breadth-of-flexibilities element of Articles 6, 30, 31, and 31bis.',
    'Under the sibling reading, generic manufacturers and health ministries exit the beneficiary set, originator patent holders exit the victim set, epsilon rises toward the exclusivity pole, and this file''s classification is replaced wholesale rather than amended.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: this story is one reading of a contested kernel, and the sibling reading swaps the structural polarity.').

omega_variable(
    formal_availability_vs_actual_use,
    'Is the flexibility architecture functioning coordination, or is it mostly latent negotiating leverage whose operative life is theatrical?',
    'Longitudinal tracking of compulsory-license issuances, Article 31bis notifications, and parallel-import disputes per period, normalized against eligibility.',
    'Sustained exercise drives theater_ratio down and strengthens the coordination reading; persistent dormancy drives theater_ratio up and activates the piton-drift hypothesis even while the founding problem stays live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_availability_vs_actual_use, empirical, 'Whether the gap between entitlement and exercise is incidental or constitutive.').

omega_variable(
    remuneration_adequacy_threshold,
    'Do Article 31(h) remuneration rates actually preserve innovation incentives, or do they function as confiscatory pricing that guts them?',
    'Econometric comparison of awarded licensing rates against R&D-elasticity benchmarks and against voluntary-license terms offered by the same firms.',
    'Low adequacy raises the extractiveness attributable to the arrangement (uncompensated-taking component); adequate remuneration confines extraction to a bounded corrective transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remuneration_adequacy_threshold, empirical, 'Whether the transfer from patent holders is compensated rebalancing or uncompensated taking.').

omega_variable(
    innovation_erosion_empirics,
    'Does broad deployment of the flexibilities measurably reduce R&D targeting diseases concentrated in flexible jurisdictions?',
    'Portfolio-shift analysis following the Thailand, Colombia, and Malaysia license episodes: did originator pipelines for relevant therapeutic areas contract relative to controls?',
    'Measurable erosion substantiates the sibling reading''s incentive premise and raises the long-run cost side of the ledger; a null result undermines it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_erosion_empirics, empirical, 'The dynamic-innovation consequence the static transfer analysis cannot see.').

omega_variable(
    suppression_mechanism_attribution,
    'Is the observed non-use of the flexibilities driven by structural capacity gaps (no domestic generic industry, procedural cost) or by strategic suppression (trade pressure, investor-state threats, watch-list retaliation)?',
    'Cross-country comparison of flexibility use conditional on manufacturing capacity and TRIPS-plus/FTA exposure: if capacity-similar countries diverge by treaty exposure, the strategic component is identified.',
    'Strategic attribution assigns the measured suppression to external enforcement machinery and raises effective burden on originator-aligned seats; structural attribution locates it in domestic capacity and lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_attribution, empirical, 'Decomposing the suppression scalar into structural versus imposed components.').

omega_variable(
    cs_framing_under_determination,
    'Is the kernel the treaty TEXT (as framed here: fixed_text under lineage authority with an interpretive layer) or the ENFORCEMENT architecture (dispute settlement with retaliation backing)?',
    'Signals favoring the text-framing: Doha''s political fixation of interpretation and the rarity of adverse panel rulings on flexibilities. Signals favoring the enforcement-framing: interpretation in practice migrates to wherever retaliation risk lives. Adjudicate by observing which locus actually resolves contests over the coming period.',
    'Under the enforcement framing, this reading becomes a minority position whose survival depends on political forums rather than settled text, its authority position downgrades, and its drift magnitude increases; the reading_relations topology also shifts, since the dispute-settlement sibling would move from influenced to controlling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Alternative coherent framings of the same kernel produce different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_ph_flex_tr_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(trips_ph_flex_tr_t4, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 4, 0.42).
narrative_ontology:measurement(trips_ph_flex_tr_t8, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 8, 0.46).
narrative_ontology:measurement(trips_ph_flex_tr_t12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 12, 0.49).
narrative_ontology:measurement(trips_ph_flex_tr_t16, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement(trips_ph_flex_tr_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 20, 0.53).
narrative_ontology:measurement(trips_ph_flex_tr_t24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(trips_ph_flex_be_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(trips_ph_flex_be_t4, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(trips_ph_flex_be_t8, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(trips_ph_flex_be_t12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(trips_ph_flex_be_t16, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(trips_ph_flex_be_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(trips_ph_flex_be_t24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(trips_ph_flex_su_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(trips_ph_flex_su_t4, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 4, 0.49).
narrative_ontology:measurement(trips_ph_flex_su_t8, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(trips_ph_flex_su_t12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(trips_ph_flex_su_t16, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(trips_ph_flex_su_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(trips_ph_flex_su_t24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resource_allocation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% The colloquial label 'TRIPS' conflates at least three structurally distinct claims; per the epsilon-invariance principle the kernel decomposes into a constraint family. This story authors epsilon for the standing arrangement as the flexibility reading construes it (broad flexibilities conditioning exclusivity, moderate-high transfer from patent holders). strong_exclusivity_reading authors epsilon for the same treaty read as mandating high uniform protection with narrow flexibilities (higher epsilon toward patient/access seats, inverted beneficiary/victim polarity). dispute_settlement_interpretive_authority authors epsilon for the enforcement-and-interpretation architecture itself. The upstream claim (the text exists and binds its parties) feeds both downstream readings; edges run from this file to both siblings, and the sibling files carry reciprocal links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
