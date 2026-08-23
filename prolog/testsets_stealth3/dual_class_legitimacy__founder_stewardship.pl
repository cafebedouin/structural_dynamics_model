% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Founder Stewardship Reading of Dual-Class Control Legitimacy
 *   domain: economic/legal/corporate_governance
 *
 * SUMMARY:
 *   Large technology and media corporations commonly issue dual-class capital
 *   structures: founders hold supervoting shares (often ten votes per share)
 *   while public investors hold limited-vote Class A shares that supply most
 *   of the capital at risk. This file instantiates the founder_stewardship
 *   reading of the dual_class_legitimacy kernel — concentrated founder
 *   control understood as fiduciary coordination that insulates long-horizon
 *   missions from short-term market pressure, with Class A holders
 *   compensated indirectly through mission execution. Epsilon is authored for
 *   the standing dual-class arrangement as THIS reading assesses it; the
 *   sibling files dual_class_legitimacy__minority_extraction and
 *   dual_class_legitimacy__disclosure_consent instantiate other constraints
 *   over the same arrangement with their own reading-indexed epsilon, linked
 *   via network.affects_constraints. The historical interval maps t=0 to 1994
 *   (withdrawal of the mandatory one-share-one-vote listing rule) through
 *   t=30 to 2024. KEY AGENTS (by structural relationship): -
 *   founder_controlling_holders: Agenda-setting beneficiary
 *   (powerful/identity_locked) — administers the structure and receives the
 *   control premium as stewardship returns - class_a_public_investors:
 *   Reading-declared indirect beneficiary and residual cost-bearer
 *   (powerless/mobile) — supplies most capital at risk without reaching any
 *   winning vote threshold - index_fund_asset_managers: Mandate-bound holder
 *   with conflicted advocacy (institutional/trapped) -
 *   long_horizon_employees: Mission-side beneficiary (organized/constrained)
 *   - hostile_takeover_intermediaries: Excluded corrective channel
 *   (powerful/arbitrage) — objection structurally silenced -
 *   securities_and_exchange_commission: Offering-disclosure observer
 *   (institutional/analytical) - exchange_listing_standards_bodies:
 *   Listing-and-index observer (institutional/analytical) -
 *   proxy_advisory_firms: Recommendation-layer observer
 *   (institutional/analytical)
 *
 * KEY AGENTS:
 *   - founder_controlling_holders: agenda-setting beneficiary (powerful/identity_locked) — administers the arrangement, receives the control premium
 *   - class_a_public_investors: indirect beneficiary and residual cost-bearer per this reading (powerless/mobile)
 *   - index_fund_asset_managers: mandate-bound holders, conflicted advocates (institutional/trapped)
 *   - long_horizon_employees: mission-side beneficiaries (organized/constrained)
 *   - hostile_takeover_intermediaries: excluded corrective channel (powerful/arbitrage)
 *   - securities_and_exchange_commission: disclosure-stage observer (institutional/analytical)
 *   - exchange_listing_standards_bodies: listing/index observer (institutional/analytical)
 *   - proxy_advisory_firms: recommendation-layer observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.32).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.36).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.32).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.24).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Founder Stewardship Reading of Dual-Class Control Legitimacy").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "economic/legal/corporate_governance").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '2333e68a-5cfa-45f8-9402-a1324e761c49').
narrative_ontology:cs_kernel_codification('2333e68a-5cfa-45f8-9402-a1324e761c49', distributed).
narrative_ontology:cs_authority_grounding('2333e68a-5cfa-45f8-9402-a1324e761c49', distributed).
narrative_ontology:cs_reading_relation('2333e68a-5cfa-45f8-9402-a1324e761c49', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('2333e68a-5cfa-45f8-9402-a1324e761c49', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('2333e68a-5cfa-45f8-9402-a1324e761c49', foundational, control_legitimized_by_fiduciary_performance).
narrative_ontology:cs_axiom_status(control_legitimized_by_fiduciary_performance, holdable).
narrative_ontology:cs_axiom_grounding('2333e68a-5cfa-45f8-9402-a1324e761c49', control_legitimized_by_fiduciary_performance, empirically_contingent).
narrative_ontology:cs_axiom('2333e68a-5cfa-45f8-9402-a1324e761c49', secondary, insulation_from_market_pressure_is_productive).
narrative_ontology:cs_axiom_status(insulation_from_market_pressure_is_productive, holdable).
narrative_ontology:cs_axiom_grounding('2333e68a-5cfa-45f8-9402-a1324e761c49', insulation_from_market_pressure_is_productive, instrumental).
narrative_ontology:cs_reference_frame('2333e68a-5cfa-45f8-9402-a1324e761c49', fiduciary_stewardship_equilibrium).
narrative_ontology:cs_drift_state('2333e68a-5cfa-45f8-9402-a1324e761c49', contemporary_index_exclusion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2333e68a-5cfa-45f8-9402-a1324e761c49', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_controlling_holders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_public_investors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_horizon_employees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, index_fund_asset_managers).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, class_a_public_investors).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, stewardship_theory_of_control).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, managerial_myopia_insulation_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the supervoting share class created at the offering, giving durable command of board elections with a minority of the economic equity. Charter terms give this bloc effective veto over any reclassification or conversion proposal. Frames continued control as a duty owed to the mission and to all shareholders. Personal wealth is concentrated in the enterprise; selling down would erode voting control and signal lost conviction, so exit from the control position is bound up with identity as the company's author.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_controlling_holders, agenda_setter,
    powerful, generational, identity_locked, global).

% Buys limited-vote shares at the offering and on the open market, supplying the bulk of the capital at risk. Under the charter their accumulated votes cannot reach a winning threshold on any governance question. They receive whatever mission execution delivers in enterprise value and bear the accountability gap; their practical recourse is to sell, which is liquid and immediate.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_public_investors, beneficiary,
    powerless, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, class_a_public_investors, payer).

% Hold permanent, mandate-bound positions in every index constituent including dual-class issuers, and vote the proxies under published stewardship policies. Their public posture favors voting parity for new listings, yet incumbent conversions are rarely sponsored, and fee-generating commercial relationships with covered issuers temper confrontational campaigns. Exiting individual holdings is incompatible with index tracking.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, index_fund_asset_managers, beneficiary,
    institutional, generational, trapped, global).

% Are compensated substantially in equity and build careers around the mission. Insulation from activist cost programs and breakup acquisitions protects hiring promises and project continuity. Departure forfeits unvested grants and abandons invested mission commitment, so mobility is real but costly.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, long_horizon_employees, beneficiary,
    organized, biographical, constrained, global).

% Activist funds and potential acquirers whose standard corrective channel — accumulating a blocking or controlling stake, running a proxy contest, or tendering for the company — is foreclosed by the supervoting architecture regardless of how much limited-vote stock they buy. Capital is redeployable across the market; their objection never enters the issuer's governance process because no vote they can win exists.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, hostile_takeover_intermediaries, excluded,
    powerful, immediate, arbitrage, global).

% Reviews the offering disclosures through which dual-class terms are presented to initial buyers, and has repeatedly studied concentrated-vote structures without prohibiting them. Its levers are disclosure quality and trading-rule enforcement rather than charter design.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_and_exchange_commission, observer,
    institutional, generational, analytical, national).

% Set listing standards that admitted dual-class structures after the mandatory voting-parity rule was withdrawn, and operate index committees that since 2018 have excluded newly dual-class firms from flagship indices. These judgments reshape the arrangement's market environment without touching any charter.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, exchange_listing_standards_bodies, observer,
    institutional, generational, analytical, global).

% Publish voting recommendations applied across institutional client portfolios; they recommend against dual-class structures at offering and favor sunset adoption afterward. Their influence operates through recommendation uptake rather than ownership.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, proxy_advisory_firms, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_controlling_holders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates strategic decision authority in a durable founder bloc so that long-dated investments, platform reinvestment, and identity-defining projects survive quarterly earnings cycles and unsolicited takeover approaches without continuous renegotiation of direction.
% TRANSFER_FUNCTION: Moves residual control rights — and the private value attached to commanding them — from dispersed Class A capital toward founder-held supervoting shares, while cash-flow claims remain distributed as purchased; accountability risk for that command falls on public holders who cannot reach a winning vote threshold.
% ABSENT_VOICES: Would-be acquirers and activist investors whose corrective channel is structurally foreclosed; Class A dissenters below any winnable threshold; and future generations of shareholders who inherit the structure without having participated in the offering decision. They sit outside the conversation because charter amendment runs exclusively through the founder-controlled board and supermajority locks that include the founder class veto.
% DISAPPEARANCE_RATIONALE: Overnight removal collapses supervoting into one-share-one-vote: founder blocs lose working majorities, boards reconstitute through ordinary elections, takeover vulnerability reprices immediately, and long-dated strategic commitments face quarterly scrutiny at once — the governance economy of mission-insured firms reorganizes around vote-responsive management within a few reporting cycles.
% FOUNDING_PROBLEM: Founders raising outside capital at scale needed to fund growth without surrendering strategic direction either to quarterly-oriented public markets or to the hostile-acquisition wave; the dual-class charter solved 'raise the capital, keep the mission.'
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting set: academic corporate-finance work documents managerial-myopia pressure (earnings-guidance cycles, takeover-discipline externalities), and large asset-owner stewardship letters attest short-term market pressure — genuine external corroboration for the founding problem itself. No party outside the benefiting set attests that concentrated founder control specifically is the necessary or uniquely effective remedy; efficacy testimony comes chiefly from founders, affiliated venture investors, and issuer-side counsel.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.32: the reading acknowledges a real, priced transfer — the control premium, the accountability asymmetry, entrenchment tail risks — while holding it justified by mission insurance. That value sits well above the resource_allocation floor of 0.15, so excess-over-coordination-cost remains visible in the data rather than excused by the coordination story. Suppression 0.36 reflects total foreclosure of voice combined with fully open exit: alternatives do not collapse (accessibility_collapse 0.24), because investors can buy parity-vote firms and founders can incorporate single-class. Resistance 0.52 tracks sustained proxy-advisor opposition, index-exclusion policy since 2018, and academic sunset campaigns. Theater 0.16 keeps stewardship rhetoric substantively load-bearing under this reading. All three temporal series share one time grid (t=0..30, mapped to 1994–2024) as the alignment rule requires; the suppression_requirement series is authored because the narrative specifically tracks enforcement-capacity change — charter locks hardened and poison-pill defenses matured precisely as index and proxy-advisor pressure mounted. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's arithmetic.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently from the same structural data. The founder seat experiences the arrangement as stewardship it personally guarantees; the Class A seat experiences purchased exposure with foreclosed voice; index managers experience mandate-bound holding with commercially conflicted advocacy; excluded intermediaries experience a closed door. The founder's identity_locked exit fuses personal authorship with the enterprise — the lock is ideological and relational (the founder IS the mission's guarantor) as much as economic (undiversified wealth). If that frame broke — through succession, sale pressure, or governance defeat — the control premium would convert into saleable value and the seat's computed position would migrate toward bearing its own structure differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries drive the derived directionality: the founder bloc and long-horizon employees sit near the beneficiary pole; Class A public investors derive low-to-moderate d from their dual declaration (beneficiary with secondary payer position) — this reading indexes their cost as a priced governance risk rather than extraction. Index fund managers mix fee-side benefit with mandate-trapped exposure. Excluded takeover intermediaries sit outside the beneficiary/victim derivation entirely; their position is recorded as structural absence, not as authored directionality. Global spatial scope modestly amplifies any target-side effective extraction the engine computes. Receipt is kept distinct from benefit: gain_flow names the founder seat because the arrangement's private gains demonstrably accrue there, even though Class A holders are the reading-declared indirect beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Authoring this reading separately from its siblings is what blocks both mislabels. Read through the minority_extraction criterion, the same arrangement counts as extraction with named victims; read through this file's criterion, the coordination function dominates and the frictions remain visible-but-priced. The metrics guard against rope-washing in turn: extraction is non-zero, rising across the interval, and tracked against the resource_allocation floor. Because the founding problem remains live and the insulation function remains operative, no mandatrophy declaration is made; the rising theater and extraction series are monitored for onset of drift rather than assumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'Which reading of the dual_class_legitimacy kernel governs this evaluation — founder stewardship (instantiated in this file), minority proportionality, or disclosure consent?',
    'Resolution is conceptual, not empirical: the three readings locate legitimacy in different criteria (fiduciary performance, capital-proportional entitlement, informed consent). Corpus consumers compare the sibling files'' authored epsilon and beneficiary/victim declarations over the identical standing arrangement.',
    'Switching readings changes epsilon, beneficiary/victim declarations, and computed classification wholesale; this file''s rope claim and low-moderate extraction are valid only within the stewardship criterion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer-frame routing: this constraint is one reading of a contested kernel; sibling files instantiate the others.').

omega_variable(
    mission_execution_causal_claim,
    'Does concentrated founder control causally produce superior long-horizon outcomes for all shareholder classes, as the stewardship premise requires?',
    'Matched-sample event studies and panel regressions comparing dual-class adopters with one-share-one-vote controls on long-run returns, R&D persistence, and survival; meta-analysis of the existing dual-class performance literature.',
    'Null or negative causal findings dissolve this reading''s coordination justification and shift the arrangement toward the minority_extraction account; robust positive findings stabilize the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_execution_causal_claim, empirical, 'Empirical foundation of the stewardship axiom: performance-justified control.').

omega_variable(
    private_benefit_consumption,
    'How much private benefit of control do founder blocs actually consume — above-market compensation, related-party transactions, legacy projects, entrenchment rents?',
    'Audit and proxy disclosures of related-party dealings and pay ratios in dual-class firms versus matched controls; regulatory enforcement records.',
    'Large measured private benefits raise effective extraction at the founder seat even under this reading''s charitable indexing, pushing computed per-seat classification toward hybrid or extractive types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_benefit_consumption, empirical, 'Magnitude of insider consumption of the control premium.').

omega_variable(
    consent_stewardship_boundary,
    'Where does this reading draw the line between governance risks priced into the Class A purchase decision (which it accepts) and foreclosed governance voice (which it discounts) — is the disagreement with the consent reading located in pricing or in voice rights?',
    'Conceptual separation of ex ante pricing of governance risk from ex post voice entitlements; structured comparison against the disclosure_consent sibling file''s criterion.',
    'If voice rights themselves ground legitimacy, the reading''s suppression assessment rises sharply and its rope claim weakens; if pricing suffices, current values hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_stewardship_boundary, conceptual, 'Locates the structural point of disagreement between the stewardship and consent readings of the kernel.').

omega_variable(
    charter_sunset_heterogeneity,
    'Does the arrangement''s suppressive force vary systematically between dual-class charters carrying time- or contingency-based sunset clauses and those with perpetual founder-veto locks?',
    'Classify listed dual-class issuers by charter sunset terms; compare drift trajectories of control concentration and minority-holder outcomes across the subsets.',
    'If the perpetual-lock subset computes substantially more extractive, the constraint family may need subdivision; a strongly sunset-dominated population would trend the arrangement toward transitional support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_sunset_heterogeneity, empirical, 'Charter-term heterogeneity within the standing arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.06).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__founder_stewardship, theater_ratio, 5, 0.07).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__founder_stewardship, theater_ratio, 10, 0.09).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__founder_stewardship, theater_ratio, 15, 0.11).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.13).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__founder_stewardship, theater_ratio, 25, 0.15).
narrative_ontology:measurement(dual_tr_t30, dual_class_legitimacy__founder_stewardship, theater_ratio, 30, 0.16).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__founder_stewardship, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__founder_stewardship, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__founder_stewardship, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__founder_stewardship, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(dual_be_t30, dual_class_legitimacy__founder_stewardship, base_extractiveness, 30, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__founder_stewardship, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__founder_stewardship, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__founder_stewardship, suppression_requirement, 15, 0.27).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__founder_stewardship, suppression_requirement, 25, 0.34).
narrative_ontology:measurement(dual_su_t30, dual_class_legitimacy__founder_stewardship, suppression_requirement, 30, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% The natural-language label 'dual-class legitimacy' decomposes by legitimacy criterion into three sibling constraints over one standing arrangement (concentrated founder control via supervoting shares). This file (founder_stewardship) authors low-moderate reading-indexed epsilon with no declared victims; dual_class_legitimacy__disclosure_consent grounds legitimacy in informed consent and authors intermediate values; dual_class_legitimacy__minority_extraction authors the highest epsilon with named victims. Every file links the others via affects_constraints. No upstream/downstream ordering is asserted among the siblings — they are parallel readings of one kernel, not a dependency chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
