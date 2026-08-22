% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__competitive_moat_reading, []).

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
 *   constraint_id: article17_erasure_right__competitive_moat_reading
 *   human_readable: GDPR Article 17 Erasure Duty - Competitive Moat Reading
 *   domain: technological/legal/economic
 *
 * SUMMARY:
 *   A dominant account treats GDPR Article 17 - the right to erasure - as a
 *   privacy guarantee. This story instantiates a rival reading: that the
 *   erasure duty operates, in practice, as an incumbent-protection device.
 *   The duty imposes a largely fixed compliance floor - records of
 *   processing, verified deletion-request handling, propagation of erasure to
 *   third-party processors, audit trails - whose per-revenue burden scales
 *   inversely with firm size. Large platforms amortize the floor across
 *   enormous revenues and convert it into a competitive shield; early-stage
 *   challengers meet the same floor before earning revenue, and capital
 *   markets price it accordingly. The supervision apparatus (national
 *   authorities, a coordinating board, turnover-indexed fines) enforces the
 *   duty uniformly, which is precisely what makes the incidence asymmetric.
 *   Per the epsilon-invariance principle this is one of three linked stories
 *   decomposing the Article 17 label; the referent here is the standing
 *   erasure regime as operated, assessed by this reading's lights. The
 *   claimed type and the metrics are authored independently: the structure
 *   shows a genuine coordination layer (a single rulebook replacing
 *   twenty-eight divergent regimes; a working, heavily used deletion remedy)
 *   carrying asymmetric incidence, while the metrics describe how the
 *   arrangement actually distributes its burdens.
 *
 * KEY AGENTS:
 *   - large_platform_incumbents: Primary beneficiary (institutional/arbitrage) - amortizes fixed erasure-compliance costs across vast revenue; nets relative advantage as smaller rivals absorb the same fixed costs
 *   - eu_data_protection_authorities: Agenda setter (institutional/constrained) - administers enforcement, issues corrective orders and turnover-indexed fines, publishes binding interpretive guidance
 *   - early_stage_startups: Primary target (powerless/trapped) - bears the same fixed compliance floor with a fraction of the revenue; exiting the EU market forfeits the addressable market
 *   - small_medium_online_businesses: Secondary target (moderate/constrained) - absorbs compliance overhead as permanent margin erosion
 *   - noncommercial_data_processors: Collateral target (powerless/trapped) - volunteer archives and hobbyist services facing erasure demands without legal staff
 *   - privacy_compliance_service_industry: Secondary beneficiary (organized/mobile) - sells the tooling, counsel, and assurance the duty makes necessary
 *   - eu_data_subjects: Nominal rights-holder (powerless/constrained) - holds the deletion right; carries indirect costs through reduced competition and pass-through pricing
 *   - venture_capital_investors: Risk bearer (powerful/arbitrage) - prices the compliance floor into term sheets, steering capital away from data-heavy early-stage bets
 *   - civil_society_privacy_organizations: Analytical observer (organized/analytical) - litigates test cases, documents enforcement gaps, defends the right's scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.66).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.56).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "GDPR Article 17 Erasure Duty - Competitive Moat Reading").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technological/legal/economic").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, 'bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8').
narrative_ontology:cs_kernel_codification('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', formalized).
narrative_ontology:cs_authority_grounding('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', lineage).
narrative_ontology:cs_interpretation_layer_present('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8').
narrative_ontology:cs_reading_relation('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', foundational, compliance_incidence_determines_function).
narrative_ontology:cs_axiom_status(compliance_incidence_determines_function, holdable).
narrative_ontology:cs_axiom_grounding('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', compliance_incidence_determines_function, empirically_contingent).
narrative_ontology:cs_axiom('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', foundational, de_facto_barrier_trumps_formal_openness).
narrative_ontology:cs_axiom_status(de_facto_barrier_trumps_formal_openness, holdable).
narrative_ontology:cs_axiom_grounding('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', de_facto_barrier_trumps_formal_openness, empirically_contingent).
narrative_ontology:cs_reference_frame('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', formally_neutral_compliance_regime).
narrative_ontology:cs_drift_state('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', post_enforcement_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd3b2ccd-35cf-4ba7-b025-acdd5d8e23d8', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, large_platform_incumbents).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, privacy_compliance_service_industry).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, early_stage_startups).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, small_medium_online_businesses).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, noncommercial_data_processors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, eu_data_subjects).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, eu_data_subjects).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, venture_capital_investors).
narrative_ontology:constraint_vindicates(article17_erasure_right__competitive_moat_reading, compliance_economies_of_scale_hypothesis).
narrative_ontology:constraint_vindicates(article17_erasure_right__competitive_moat_reading, regulatory_incidence_asymmetry_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate multi-service platforms processing EU personal data at planetary scale. Maintain standing privacy-engineering teams, automated erasure pipelines, and dedicated counsel, so the per-user cost of honoring deletion requests is marginal. The same fixed investments that satisfy the duty are out of reach for smaller rivals, and these firms' shares of EU digital advertising and commerce widened in the years after uniform application. Leaving is not on the table: they shape implementation through consultation channels, trade bodies, and litigation strategy rather than exit.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, large_platform_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% National supervisory authorities and the European board coordinating them. Receive and adjudicate complaints, audit processors, issue corrective orders and turnover-indexed fines, and publish interpretive guidance that binds practice below the statutory text. Funded by member-state budgets; their mandates and staffing scale with the framework they administer, and they cannot set the erasure duty aside without legislative change.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, constrained, continental).

% Founding teams building data-driven products for EU users. Face the same fixed obligations - records of processing, deletion-request handling, identity verification, third-party propagation - as firms a thousand times their size, typically before reaching revenue. One privacy hire can consume a meaningful share of a seed round. Exit means geo-blocking EU visitors or abandoning the market; staying means absorbing the floor.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, early_stage_startups, payer,
    powerless, immediate, trapped, regional).

% Established small and mid-sized firms - retailers, publishers, software vendors - with EU customer bases. Carry permanent compliance overhead in staff time, tooling subscriptions, and external counsel; many treat deletion requests as a loss-making line item. Relocating outside EU jurisdiction is technically possible but severs their customer base, so they stay and pay.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, small_medium_online_businesses, payer,
    moderate, biographical, constrained, continental).

% Volunteer-run archives, hobbyist forums, independent researchers, and fan communities that handle personal data incidentally. Receive erasure demands with no legal staff to evaluate them; some have shut down rather than build deletion infrastructure, others quietly remove contested material to avoid exposure. No consultation channel represents them.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, noncommercial_data_processors, payer,
    powerless, biographical, trapped, regional).

% Consent-management platforms, erasure-workflow vendors, privacy consultancies, and audit firms. Sell precisely the tooling and assurance the duty makes necessary; revenue scales with the obligation's complexity and reach. Highly mobile: the same products sell into any jurisdiction that adopts comparable rules.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, privacy_compliance_service_industry, beneficiary,
    organized, biographical, mobile, global).

% Individuals whose data the covered firms process. Hold an enforceable right to have personal data deleted at no cost, with substantial awareness and use. Bear indirect costs as reduced competition and pass-through pricing in concentrated digital markets; most never connect the two.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, eu_data_subjects, beneficiary,
    powerless, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, eu_data_subjects, payer).

% Institutional investors financing early-stage technology companies. Price the compliance floor into diligence: data-heavy consumer startups carry a known regulatory liability from day one, and some investors steer toward sectors or geographies where the floor is lower. Fully mobile across asset classes and regions.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, venture_capital_investors, payer,
    powerful, biographical, arbitrage, global).

% Digital-rights groups and academic centers that litigate test cases, document enforcement gaps, and defend the deletion right's scope in public consultation. Neither fund the system nor pay its costs; their seat is analytical and advocacy-shaped, with standing in the conversation that commercial challengers lack.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, civil_society_privacy_organizations, observer,
    organized, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__competitive_moat_reading, large_platform_incumbents).
narrative_ontology:fixing_cost_class(article17_erasure_right__competitive_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform deletion mechanism for personal data across twenty-seven member states and harmonizes data-protection obligations into a single rulebook, replacing fragmented national regimes with inconsistent enforcement.
% TRANSFER_FUNCTION: Moves compliance expenditure - legal review, engineering rework, records infrastructure, vendor fees - from every firm handling EU personal data toward compliance service providers and internal overhead, and competitively shifts market share toward firms large enough to amortize fixed compliance costs.
% ABSENT_VOICES: Pre-revenue founders and would-be entrants who never materialize under the cost structure; volunteer-run archives and hobbyist processors with no representation in consultations; EU consumers as a diffuse class absorbing reduced competition indirectly. All three groups would object if seated, and none is.
% DISAPPEARANCE_RATIONALE: Erasure request pipelines, officer roles, consent tooling, and the vendor market built around the duty would unwind within quarters; entry rates for data-driven startups would respond as the fixed compliance floor dropped; incumbents would lose a documented share-protection mechanism; deletion practice would revert to patchy national norms.
% FOUNDING_PROBLEM: Before uniform application, personal data persisted indefinitely with no individual deletion remedy, and firms faced twenty-eight divergent national regimes with inconsistent enforcement.
% FOUNDING_PROBLEM_CORROBORATION: European Commission impact assessments and pre-2016 fragmentation studies corroborate that the founding problem existed; competition economists and startup trade associations attest, from outside the benefiting parties, that the current operation diverges from it; supervisory authorities and digital-rights groups attest the deletion remedy remains actively used. No party disputes that the original problem was real; the dispute is over whether the present arrangement still serves it.
narrative_ontology:disappearance_verdict(article17_erasure_right__competitive_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__competitive_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__competitive_moat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__competitive_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__competitive_moat_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__competitive_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__competitive_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66 reflects the reading's core mechanism: a fixed compliance floor whose per-revenue burden scales inversely with firm size, converting a uniform legal duty into a proportional advantage for the largest firms. Suppression 0.56 is authored as a raw structural property - unscaled by power or scope - capturing that any firm targeting EU residents has no lawful alternative to the duty, and that the enforcement machinery (audits, orders, turnover-indexed fines) matured steadily after the transition period. Theater 0.37: consent-banner proliferation, boilerplate impact assessments, and checkbox erasure portals substitute paperwork for privacy outcomes - Goodhart drift visible in the rising series. Accessibility_collapse 0.50: alternatives exist (geo-blocking, non-EU incorporation, serving other markets) but collapse for any firm whose market is the EU. Resistance 0.60: sustained industry litigation and lobbying, diplomatic pressure from exporting states, and member-state complaints about enforcement funding. The claimed type tangled_rope is asserted from structure - a genuine coordination layer (single rulebook, functioning deletion remedy invoked at scale) carrying asymmetric incidence and requiring active enforcement - while the metrics are authored descriptively; the engine computes per-seat classifications independently of the claim. All three series share one time grid (points 0, 2, 4, 6, 8 years since uniform application); the 2026 endpoints are projected. The trajectories are monotonic rather than cyclical - no intermittent-reinforcement dynamic is present; the drift is cumulative ratchet, not oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the incumbent seat the arrangement presents as manageable overhead wrapped around a competitive shield - the same fixed costs that burden rivals are, at incumbent scale, rounding errors that double as barriers. From the challenger seat the identical structure presents as an existential fixed cost with no exit: comply unrecoverably, geo-block the market, or never launch. From the supervisory-authority seat it presents as a legitimate mandate executing legislative will, with complaint volumes as evidence of function rather than burden. From the data-subject seat it presents as a paper right exercised at no charge, with the competitive costs diffuse and unattributed. Same referent - the standing erasure regime - different directionalities; the engine computes this divergence from the structural data, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents sit near the beneficiary end despite paying the largest absolute compliance sums: the moat benefit dominates their net position, and arbitrage-grade options (absorbing compliance into product engineering, shaping guidance through consultation channels) dampen their effective burden further. Challengers sit near the full-target end: trapped exit (forfeit the EU market or forgo launch) amplifies effective extraction beyond the nominal cost. The compliance-vendor industry approaches pure beneficiary - it receives fees scaled to the duty's complexity with full mobility. Data subjects sit near symmetric: an enforceable right received, an indirect competitive cost paid. Supervisory authorities approximate symmetric administrators - they run the machinery without collecting its product, though their budgets scale with the framework they police. Venture investors are pulled toward the target end on their portfolio exposure but retain full arbitrage across geographies and asset classes, damping the pull.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - personal data persisting indefinitely with no deletion remedy, twenty-eight divergent national regimes - was real and is corroborated outside the benefiting parties. Its fragmentation half is substantially solved; its sovereignty half remains live. The arrangement persists with a growing compliance apparatus whose revenue depends on the duty's complexity - the classic drift surface where coordination decays toward performance. Classifying as tangled_rope keeps both facts alive: calling the arrangement pure extraction would erase a deletion remedy that data subjects invoke in the millions; calling it pure coordination would erase the documented incidence asymmetry that filters entrants. The founding_problem_status is contested and the disappearance verdict is world_rearranges, so the dead-mandate zombie signature does not fire - but the rising theater series marks the seam to monitor: if enforcement activity continues substituting paperwork for outcomes while the asymmetry persists, the arrangement drifts pitonward with incumbents as residual beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_operative_function,
    'Which of the three readings of the article17_erasure_right kernel identifies the regime''s operative function - data sovereignty (privacy_fundamental_reading), speech-suppression vector (censorship_mechanism_reading), or incumbent protection (this competitive_moat_reading)?',
    'Comparative evaluation holding the referent fixed - the standing erasure regime - across jurisdictions and time: firm-size-stratified compliance-cost ratios, entry and survival rates around uniform application, erasure-request volume and outcome distributions, and delisting-effect studies, weighed together rather than singly.',
    'Each sibling relocates the victim set: under privacy_fundamental_reading the targets are retaining firms as such and measured burden falls toward the coordination floor; under censorship_mechanism_reading the targets are speakers and archived content; under this reading they are challengers priced out by the compliance floor. Resolving the contest changes which seats compute as harmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_operative_function, conceptual, 'Committer contest: three readings of one kernel assign different victim sets and epsilon to the same standing arrangement.').

omega_variable(
    moat_effect_real_or_rhetorical,
    'Is the competitive-moat effect empirically real, or is it a deregulatory narrative advanced by firms that would prefer lighter rules regardless?',
    'Difference-in-differences designs on entry, survival, and market-share series around uniform application and later enforcement milestones, controlling for funding cycles and sector composition; firm-size-stratified compliance-cost surveys independent of industry sponsorship.',
    'If the effect is unreal, this reading collapses toward the privacy_fundamental sibling, the incumbent beneficiary declaration fails, and incumbent burden complaints register as rent defense; if real, the moat is a measurable transfer running through the compliance channel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moat_effect_real_or_rhetorical, empirical, 'Whether the moat mechanism exists or the reading itself serves interested parties.').

omega_variable(
    article17_specific_cost_share,
    'What share of measured compliance cost is attributable to the Article 17 erasure machinery specifically, rather than to framework-wide duties such as lawful-basis documentation, breach notification, and officer appointments?',
    'Provision-level cost accounting in supervisory-authority reports and audited firm disclosures separating erasure-pipeline spend from general data-protection program spend.',
    'If the erasure-specific share is small, this reading over-attributes the burden and its epsilon should fall toward the family baseline; if large, the moat claim tightens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article17_specific_cost_share, empirical, 'Cost attribution between the erasure duty and the surrounding framework.').

omega_variable(
    proportionality_reform_counterfactual,
    'Would proportionality thresholds or small-processor carve-outs dissolve the asymmetric incidence without dissolving the deletion remedy itself?',
    'Member-state variation in enforcement intensity toward small processors, and piloted threshold regimes, evaluated for entry response and remedy availability.',
    'If thresholds suffice, the arrangement is reformable toward pure coordination and the hybrid structure is contingent; if the asymmetry is intrinsic to any universal erasure duty, the hybrid character hardens and reform talk registers as cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_reform_counterfactual, empirical, 'Reformability of the incidence asymmetry versus its intrinsicality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article17_moat_tr_t0, article17_erasure_right__competitive_moat_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(article17_moat_tr_t0, observed).
narrative_ontology:measurement(article17_moat_tr_t2, article17_erasure_right__competitive_moat_reading, theater_ratio, 2, 0.27).
narrative_ontology:measurement_basis(article17_moat_tr_t2, observed).
narrative_ontology:measurement(article17_moat_tr_t4, article17_erasure_right__competitive_moat_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement_basis(article17_moat_tr_t4, observed).
narrative_ontology:measurement(article17_moat_tr_t6, article17_erasure_right__competitive_moat_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(article17_moat_tr_t6, observed).
narrative_ontology:measurement(article17_moat_tr_t8, article17_erasure_right__competitive_moat_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement_basis(article17_moat_tr_t8, projected).

% Extraction over time
narrative_ontology:measurement(article17_moat_be_t0, article17_erasure_right__competitive_moat_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(article17_moat_be_t0, observed).
narrative_ontology:measurement(article17_moat_be_t2, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2, 0.53).
narrative_ontology:measurement_basis(article17_moat_be_t2, observed).
narrative_ontology:measurement(article17_moat_be_t4, article17_erasure_right__competitive_moat_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement_basis(article17_moat_be_t4, observed).
narrative_ontology:measurement(article17_moat_be_t6, article17_erasure_right__competitive_moat_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(article17_moat_be_t6, observed).
narrative_ontology:measurement(article17_moat_be_t8, article17_erasure_right__competitive_moat_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement_basis(article17_moat_be_t8, projected).

% Suppression requirement over time
narrative_ontology:measurement(article17_moat_su_t0, article17_erasure_right__competitive_moat_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(article17_moat_su_t0, observed).
narrative_ontology:measurement(article17_moat_su_t2, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2, 0.47).
narrative_ontology:measurement_basis(article17_moat_su_t2, observed).
narrative_ontology:measurement(article17_moat_su_t4, article17_erasure_right__competitive_moat_reading, suppression_requirement, 4, 0.51).
narrative_ontology:measurement_basis(article17_moat_su_t4, observed).
narrative_ontology:measurement(article17_moat_su_t6, article17_erasure_right__competitive_moat_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement_basis(article17_moat_su_t6, observed).
narrative_ontology:measurement(article17_moat_su_t8, article17_erasure_right__competitive_moat_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(article17_moat_su_t8, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label Article 17 covers structurally distinct claims; per the epsilon-invariance principle the kernel decomposes into three stories - this competitive-moat reading, the privacy-fundamental reading, and the censorship-mechanism reading - each with its own epsilon, beneficiary/victim structure, and claimed type, linked through affects_constraints. The privacy-fundamental reading is upstream: its rights-framing supplies the legitimating vocabulary this reading says carries the moat, and it holds the highest empirical confidence in the family. The censorship reading shares the strategic-abuse mechanism family. Each file links the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
