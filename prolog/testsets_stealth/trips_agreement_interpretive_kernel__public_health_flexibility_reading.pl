% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Regime (Broad-Flexibility Reading)
 *   domain: international_trade_law/public_health/intellectual_property
 *
 * SUMMARY:
 *   The arrangement under examination is the public-health flexibility
 *   architecture of the international patent treaty system as operated since
 *   1995: members may authorize generic production of patented medicines
 *   without the patent holder's consent under case-by-case compulsory
 *   licensing with compensation, may permit parallel importation of
 *   legitimately sold products under their own exhaustion policy, and, since
 *   the 2001 ministerial affirmation and the temporary waiver made permanent
 *   as a treaty amendment in 2017, may export licensed generics to members
 *   lacking manufacturing capacity. This story authors that arrangement as
 *   instantiated by the public-health-flexibility reading of the TRIPS
 *   interpretive kernel: flexibilities treated as embedded member rights
 *   serving health access, with originator patent holders bearing
 *   concentrated costs of pricing erosion and exclusivity loss. Contest
 *   detail lives in commentary.kernel_context and the omega variables; the
 *   constraint itself is authored clean with a single stable epsilon over the
 *   standing arrangement.
 *
 * KEY AGENTS:
 *   - originator_pharmaceutical_patent_holders: primary target (institutional/arbitrage) — bears pricing erosion, licensed-supply obligations, and exclusivity loss when licenses issue
 *   - generic_pharmaceutical_manufacturers: primary beneficiary (organized/mobile) — gains market entry and negotiating leverage through licensed and parallel-import channels
 *   - developing_country_health_ministries: beneficiary with procedural control (organized/constrained) — invoke flexibilities to procure affordable medicines and discipline price negotiations
 *   - low_income_country_patient_populations: end beneficiary (powerless/trapped) — receive access and price relief but hold no seat in trade fora
 *   - trips_council_ministerial_conference: agenda setter (institutional/constrained) — administers the regime, adopts clarifications and waivers by consensus
 *   - innovative_biotech_firms: secondary payer (moderate/constrained) — narrower portfolios make any exclusivity loss proportionally heavier
 *   - least_developed_country_members: excluded voice (powerless/trapped) — formally entitled to every flexibility yet lacking the manufacturing and legal capacity to use them
 *   - world_health_organization: analytical observer (institutional/analytical) — documents access outcomes and supplies the evidence base ministries cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.56).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.39).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.39).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Regime (Broad-Flexibility Reading)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b').
narrative_ontology:cs_kernel_codification('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', fixed_text).
narrative_ontology:cs_authority_grounding('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', lineage).
narrative_ontology:cs_interpretation_layer_present('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b').
narrative_ontology:cs_reading_relation('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', foundational, public_health_precedence_over_patent_enforcement).
narrative_ontology:cs_axiom_status(public_health_precedence_over_patent_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', public_health_precedence_over_patent_enforcement, deontological).
narrative_ontology:cs_axiom('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', secondary, flexibilities_are_member_rights_not_exceptions).
narrative_ontology:cs_axiom_status(flexibilities_are_member_rights_not_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', flexibilities_are_member_rights_not_exceptions, conventional).
narrative_ontology:cs_reference_frame('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', doha_affirmed_flexibility_baseline).
narrative_ontology:cs_drift_state('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', post_covid_waiver_negotiations, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cd5848e4-e8f3-4c67-b9a3-b0ef02545d4b', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developing_country_health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_country_patient_populations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, innovative_biotech_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, doha_declaration_on_trips_and_public_health).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, compulsory_licensing_price_discipline_hypothesis).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, vienna_convention_object_and_purpose_canon).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts clarifications, waivers, and amendments governing the flexibility regime by consensus, and reviews members' use of compulsory licenses and parallel importation. Every member holds a formal seat; in practice agenda and text emerge from negotiation among the largest trading blocs. Its own hands are tied by the rules it administers: no change to the treaty without the consent it is seeking.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_council_ministerial_conference, agenda_setter,
    institutional, generational, constrained, global).

% Produce off-patent medicines and, where licenses or parallel-import channels open, patented ones. Each authorization expands their addressable market and strengthens their hand in voluntary-license negotiations with originators. They can shift production across products and markets, but their growth strategy in regulated markets depends on the licensing channel staying open.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Procure medicines for national health programs under hard budget constraints. The flexibility regime gives them a lawful path to cheaper supply and a credible walk-away position in price negotiations with originators. Invoking it carries diplomatic and trade-relationship costs, and drafting a defensible license requires legal capacity many ministries must borrow or buy.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developing_country_health_ministries, beneficiary,
    organized, generational, constrained, national).

% Need continuous access to essential medicines; for many, the alternative to affordable generic supply is going without. They receive the regime's benefits through lower prices and program coverage but hold no representation in the trade bodies that maintain it.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_country_patient_populations, beneficiary,
    powerless, immediate, trapped, global).

% Fund long-horizon research out of monopoly returns on patented products. Licenses and parallel imports cut expected returns in affected markets and compress the price umbrella worldwide. They respond with portfolio shifts toward harder-to-license product classes, selective filing, litigation, and bilateral agreements that restore protection, while the same treaty system guarantees the exclusivity their business model depends on everywhere else.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders, beneficiary).

% Smaller developers with one or two products and no diversified revenue. A single compulsory license covering a significant market can threaten solvency in a way diversified multinationals absorb easily. They lack the legal departments and trade leverage to shape how licensing provisions are drafted or applied.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, innovative_biotech_firms, payer,
    moderate, biographical, constrained, global).

% Hold formal rights to every flexibility plus extended transition periods, but mostly lack factories, regulatory staff, and negotiating teams. Delegates attend council sessions; few table texts. The regime's benefits presuppose capabilities they do not have, so entitlement arrives without use.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, least_developed_country_members, excluded,
    powerless, biographical, trapped, national).

% Monitors medicine prices and access outcomes, prequalifies generic products, and publishes the evidence ministries cite when invoking health grounds. It holds no vote in trade bodies and depends on member funding, which limits how far its findings press against patent-holding members.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_health_organization, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__public_health_flexibility_reading, diffuse).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__public_health_flexibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining a global minimum patent standard while preventing patent enforcement from cutting off access to essential medicines during health crises: members may authorize generic production case-by-case with compensation, set their own import-exhaustion policy, and reach capacity-poor populations through a permanent export mechanism, keeping innovation incentives intact through remuneration rather than exclusivity.
% TRANSFER_FUNCTION: Moves pricing power and market exclusivity from originator patent holders to governments and generic suppliers in health-critical situations; moves affordable medicine access to patient populations; and moves negotiating leverage to importing states, paid for by originator revenue in licensed and parallel-imported markets.
% ABSENT_VOICES: Patient populations in importing countries hold no seat in trade fora; least-developed-country delegations attend council sessions but typically lack the legal-technical staff to table proposals; and future patients whose medicines depend on continued research investment are represented by no one in the room.
% DISAPPEARANCE_RATIONALE: If the flexibility regime vanished overnight, compulsory-license threats would evaporate, originators would reprice upward in middle-income markets, generic supply chains built on licensed production would contract, treatment programs would face immediate funding gaps, and the wider treaty bargain under which many members accepted patent obligations would lose its agreed counterweight, inviting renegotiation or noncompliance.
% FOUNDING_PROBLEM: In the late 1990s, uniform patent enforcement priced HIV/AIDS antiretroviral treatment far beyond reach in the countries with the worst epidemics, while patent-holding governments pressured countries that tried to license or import cheaper generics; the arrangement was built to secure members' ability to protect public health without breaching the patent treaty.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the 2001 ministerial declaration anchoring the regime was adopted by consensus including the major patent-exporting members themselves; World Health Organization and United Nations human-rights bodies have repeatedly documented continuing access gaps for high-priced medicines; and independent pandemic-preparedness reviews reached the same conclusion during COVID-19.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.56 at interval end): the regime takes real, concentrated value from originator patent holders — licensed supply at government-set remuneration, parallel-import erosion of price discrimination, and the negotiating shadow both cast — but bounds the taking through case-by-case procedure, compensation requirements, and predominant-domestic-supply limits, and this reading assesses much of the transfer as the agreed counterweight of the patent bargain rather than as rent. Suppression (0.39) is the coercive edge the regime applies to patent holders, who cannot veto or opt out of a lawful license in a given jurisdiction; it is authored as a raw structural property and deliberately left unscaled — only extractiveness is scaled by directionality and scope downstream. Theater (0.41) reflects a real functioning core (licenses issued in Thailand, India, Malaysia, and South Africa; sharp antiretroviral price declines after generic entry) wrapped in a growing performative layer: safeguard laws adopted and never used, a permanent import mechanism almost never invoked, and solidarity declarations substituting for invocation. Accessibility_collapse (0.42) is moderate because alternatives persist — voluntary licensing, tiered pricing, direct negotiation — and resistance (0.62) is substantial because originators actively litigate, lobby, and pursue bilateral agreements that re-narrow the flexibility space. The temporal series run on one shared seven-point grid (1995-2024) with all three tracked metrics authored at every point; the mid-series rise and slight end-of-interval retreat track the ministerial affirmation, the implementation wave, the pandemic-era leverage peak, and the narrow waiver outcome. Resource_allocation is declared as the coordination type: the regime's core function is allocating production and import rights for patented medicines across parties with compensation, which carries meaningful inherent transaction cost.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from identical treaty text. From the originator seat the regime operates as a standing expropriation risk repriced into every portfolio decision: exit is available only as arbitrage — shifting research toward less licensable categories, filing selectively, pricing around the threat. From the ministry and generic seats the same articles operate as procured affordability and market access. The council seat experiences neither cost nor gain directly: it administers a balance that others live inside. The engine derives these divergent per-seat classifications from the declared roles, power levels, and exit options; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (generic manufacturers, health ministries, patient populations) sit near the beneficiary end of directionality; their differing exits — generics mobile across products and markets, ministries constrained inside the treaty system, patients trapped by medical necessity — spread their effective positions along the beneficiary half rather than collapsing them to a point. Declared victims (originator patent holders, innovative biotech firms) sit near the target end; originators carry secondary_role beneficiary because the same instrument guarantees the global exclusivity floor their business model rests on, moderating their effective position below full-target. No directionality_overrides are authored: the derivation from declared roles plus exit options already produces the right relationships, and overrides key on power atoms, which would misfire here because the seat needing correction (originators' partial shielding) shares its power level with seats needing none (the council, the observer). The excluded least-developed-country seat derives weakly from the structural arrays by design — its situation is precisely that formal entitlement does not cash out as captured benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves visible. Reading the regime as pure coordination would erase the concentrated losses that make originators contest it in every available forum; reading it as pure extraction would erase the access outcomes and the preserved patent bargain that make ministries defend it. The founding problem — patent enforcement pricing lifesaving medicines out of reach while patent-holding governments penalized flexible members — remains live (recurrent pandemics, high-priced biologics and oncology therapies), so no mandatrophy is declared: the arrangement has not outlived its function, and the live-status finding combined with the world_rearranges disappearance verdict produces no capture-or-zombie mismatch flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the public_health_flexibility_reading of the trips_agreement_interpretive_kernel; what would the strong_exclusivity_reading change structurally?',
    'Not resolvable by data within this story — it is resolved by adopting a different reading; the corpus models the alternative as a separate constraint file linked through network.affects_constraints.',
    'Under the exclusivity reading, originator patent holders move from victim set to beneficiary set, generic manufacturers and health ministries become targets, epsilon rises sharply, and the claimed type shifts toward enforced extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame marker: one reading of the TRIPS kernel; the sibling reading swaps the beneficiary and victim structure.').

omega_variable(
    interpretive_authority_location,
    'Does the breadth of the flexibilities get fixed by member autonomy under the ministerial declaration''s rights language, or by binding dispute-panel interpretation — that is, where does interpretive authority over the kernel sit?',
    'Observe whether members treat adverse panel rulings as settling flexibility scope or as revisable by ministerial clarification; track the effect of appellate-body paralysis on flexibility disputes.',
    'If panel authority dominates, this reading''s stability depends on litigation outcomes; if member autonomy dominates, it depends on political coalitions — different persistence mechanics and different suppression trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_location, conceptual, 'Location of interpretive authority over the kernel: panels versus member autonomy.').

omega_variable(
    remuneration_adequacy,
    'Is the treaty''s adequate-remuneration standard sufficient to compensate originators for the exclusivity taken, or does licensed entry systematically undercompensate?',
    'Arbitration awards under the licensing and export provisions, royalty rates in issued licenses versus voluntary-license benchmarks, and originator revenue-attribution studies for licensed markets.',
    'Systematic undercompensation raises the extraction borne by the payer seats above the authored profile; adequate compensation supports the coordination framing and lowers the target seat''s effective position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remuneration_adequacy, empirical, 'Whether compulsory-license compensation matches the value of the exclusivity taken.').

omega_variable(
    capacity_vs_entitlement_gap,
    'Do paper flexibilities convert into actual access for members without domestic manufacturing capacity, or does the entitlement-to-use gap leave the regime''s coordination function partly theatrical?',
    'Track use rates of the export mechanism, voluntary-license coverage, and price outcomes in capacity-poor members after flexibility adoption.',
    'If the gap persists, theater_ratio is understated and the regime''s coordination credit shrinks; if capacity mechanisms mature, the regime functions as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_vs_entitlement_gap, empirical, 'Whether legal entitlement translates into effective medicine access without manufacturing capacity.').

omega_variable(
    trips_plus_erosion_trajectory,
    'Will bilateral TRIPS-plus commitments continue to narrow the multilateral flexibility baseline faster than multilateral clarifications widen it?',
    'Compare flexibility space in recent free-trade-agreement intellectual-property chapters against the ministerial-declaration baseline; monitor waiver-extension negotiations.',
    'Continued erosion pushes base_extractiveness and theater_ratio upward over time and dates a drift toward enforced extraction; successful multilateral reaffirmation flattens the trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trips_plus_erosion_trajectory, empirical, 'Trajectory of bilateral erosion versus multilateral reaffirmation of the flexibility baseline.').

omega_variable(
    cs_framing_under_determination,
    'Is the regime''s authority structure better framed as lineage (text, then ministerial affirmation, then practice, with the council as interpretive buffer) or as distributed (live member coalitions producing competing readings with no designated interpreter)?',
    'Test whether council clarifications actually bind dissenting members in practice; if clarification settles nothing short of consensus among the willing, the distributed framing fits better.',
    'Under the distributed framing, the interpretive-layer declaration drops out and drift reads as permanent contest rather than correctable lag; consequences flow through the commitment-system track rather than the metric track.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Two coherent authority framings for the same regime; the lineage framing is chosen and the alternative documented here.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_ph_flex_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement_basis(trips_ph_flex_tr_t1995, observed).
narrative_ontology:measurement(trips_ph_flex_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement_basis(trips_ph_flex_tr_t2001, observed).
narrative_ontology:measurement(trips_ph_flex_tr_t2005, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement_basis(trips_ph_flex_tr_t2005, observed).
narrative_ontology:measurement(trips_ph_flex_tr_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement_basis(trips_ph_flex_tr_t2010, observed).
narrative_ontology:measurement(trips_ph_flex_tr_t2017, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2017, 0.36).
narrative_ontology:measurement_basis(trips_ph_flex_tr_t2017, observed).
narrative_ontology:measurement(trips_ph_flex_tr_t2021, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2021, 0.44).
narrative_ontology:measurement_basis(trips_ph_flex_tr_t2021, observed).
narrative_ontology:measurement(trips_ph_flex_tr_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(trips_ph_flex_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(trips_ph_flex_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement_basis(trips_ph_flex_be_t1995, observed).
narrative_ontology:measurement(trips_ph_flex_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.46).
narrative_ontology:measurement_basis(trips_ph_flex_be_t2001, observed).
narrative_ontology:measurement(trips_ph_flex_be_t2005, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement_basis(trips_ph_flex_be_t2005, observed).
narrative_ontology:measurement(trips_ph_flex_be_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement_basis(trips_ph_flex_be_t2010, observed).
narrative_ontology:measurement(trips_ph_flex_be_t2017, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2017, 0.54).
narrative_ontology:measurement_basis(trips_ph_flex_be_t2017, observed).
narrative_ontology:measurement(trips_ph_flex_be_t2021, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement_basis(trips_ph_flex_be_t2021, observed).
narrative_ontology:measurement(trips_ph_flex_be_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2024, 0.56).
narrative_ontology:measurement_basis(trips_ph_flex_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(trips_ph_flex_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.22).
narrative_ontology:measurement_basis(trips_ph_flex_su_t1995, observed).
narrative_ontology:measurement(trips_ph_flex_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.28).
narrative_ontology:measurement_basis(trips_ph_flex_su_t2001, observed).
narrative_ontology:measurement(trips_ph_flex_su_t2005, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2005, 0.33).
narrative_ontology:measurement_basis(trips_ph_flex_su_t2005, observed).
narrative_ontology:measurement(trips_ph_flex_su_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement_basis(trips_ph_flex_su_t2010, observed).
narrative_ontology:measurement(trips_ph_flex_su_t2017, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2017, 0.37).
narrative_ontology:measurement_basis(trips_ph_flex_su_t2017, observed).
narrative_ontology:measurement(trips_ph_flex_su_t2021, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement_basis(trips_ph_flex_su_t2021, observed).
narrative_ontology:measurement(trips_ph_flex_su_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2024, 0.39).
narrative_ontology:measurement_basis(trips_ph_flex_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resource_allocation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the TRIPS interpretive kernel per the epsilon-invariance principle: the colloquial label 'the TRIPS Agreement' covers structurally distinct claims that cannot share one stable epsilon. This file authors the public_health_flexibility_reading (moderate extraction borne by originator patent holders, coordination credit for access). The strong_exclusivity_reading authors the same text with inverted beneficiary and victim structure and sharply higher extraction; the dispute_settlement_interpretive_authority reading authors the enforcement-and-interpretation layer that conditions how both substantive readings operate. Family edges run from this reading to both siblings; the dispute-settlement reading is upstream in the sense that its interpretive-authority claim shapes the operating environment of the two substantive readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
