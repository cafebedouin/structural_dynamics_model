% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems: Balanced Coexistence Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   One Country, Two Systems (OCTS) is the constitutional arrangement
 *   governing Hong Kong's status within the People's Republic of China
 *   following the 1997 handover. This story instantiates the BALANCED
 *   COEXISTENCE READING: a reading that treats OCTS as requiring ongoing
 *   negotiation between PRC sovereignty and Hong Kong autonomy, with neither
 *   absolute, boundaries contested and renegotiated through political
 *   accommodation rather than legal supremacy. Under this reading, both legal
 *   systems acknowledge limits on their authority; crises periodically
 *   trigger renegotiation; and civil society retains bargaining power through
 *   economic leverage and international pressure. This reading coexists with
 *   two sibling readings: (1) AUTONOMY PRIMACY (Hong Kong's autonomy is
 *   treaty-guaranteed and internationally enforceable, with meaningful checks
 *   on mainland interference), and (2) SOVEREIGNTY PRIMACY (Hong Kong
 *   autonomy is delegated and revocable, overrideable when national
 *   security/territorial integrity require). The constraint instantiated here
 *   is the balanced reading's structural claim: medium-epsilon coordination
 *   with periodic extraction and real resistance. The measurements capture
 *   the cyclical dynamic of accommodation (early years, low extractiveness) →
 *   crisis (2019-2020 mass protests, 2020 National Security Law triggering
 *   extraction spike) → renegotiated equilibrium (2021-2024, moderate
 *   extraction sustained with reduced theater as suppression becomes
 *   operational rather than performative).
 *
 * KEY AGENTS:
 *   - PRC Central Leadership: Sovereign authority; agenda-setter (institutional power); sets boundaries of autonomy; enforces through legislation and appointment. Arbitrage exit (can unilaterally absorb Hong Kong if accommodation fails, but faces international cost).
 *   - Hong Kong Civil Society: Organized, constrained power; pays through periodic security restrictions; receives institutional voice in renegotiation under balanced reading (unlike sovereignty-primacy reading where voice is consultative only).
 *   - Hong Kong Business Sector: Powerful, mobile exit; benefits from separate legal system and international status; arbitrage-capable (can relocate if autonomy erodes beyond viability).
 *   - Hong Kong Autonomy Advocates: Organized, trapped; systematically constrained by One Country premise; cannot challenge sovereignty but negotiate boundaries; suffer extraction through identity-lock (cannot leave without abandoning political identity).
 *   - Mainland Dissidents/Asylum-Seekers: Powerless, trapped; depend on HK legal separation; first to lose protection when boundaries shift toward sovereignty.
 *   - International Observers/Trade Partners: Institutional, analytical; exert external enforcement through capital/trade pressure; monitor boundary stability as signal of PRC legal credibility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.45).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.38).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems: Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, 'af9191d9-99a3-4ff3-adc4-cb1637898379').
narrative_ontology:cs_kernel_codification('af9191d9-99a3-4ff3-adc4-cb1637898379', fixed_text).
narrative_ontology:cs_authority_grounding('af9191d9-99a3-4ff3-adc4-cb1637898379', extraction).
narrative_ontology:cs_interpretation_layer_present('af9191d9-99a3-4ff3-adc4-cb1637898379').
narrative_ontology:cs_reading_relation('af9191d9-99a3-4ff3-adc4-cb1637898379', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('af9191d9-99a3-4ff3-adc4-cb1637898379', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('af9191d9-99a3-4ff3-adc4-cb1637898379', foundational, dual_legal_system_mutually_acknowledging_limits).
narrative_ontology:cs_axiom_status(dual_legal_system_mutually_acknowledging_limits, holdable).
narrative_ontology:cs_axiom_grounding('af9191d9-99a3-4ff3-adc4-cb1637898379', dual_legal_system_mutually_acknowledging_limits, conventional).
narrative_ontology:cs_axiom('af9191d9-99a3-4ff3-adc4-cb1637898379', foundational, boundaries_settled_through_political_accommodation_not_legal_supremacy).
narrative_ontology:cs_axiom_status(boundaries_settled_through_political_accommodation_not_legal_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('af9191d9-99a3-4ff3-adc4-cb1637898379', boundaries_settled_through_political_accommodation_not_legal_supremacy, instrumental).
narrative_ontology:cs_reference_frame('af9191d9-99a3-4ff3-adc4-cb1637898379', negotiated_autonomy_within_sovereignty).
narrative_ontology:cs_drift_state('af9191d9-99a3-4ff3-adc4-cb1637898379', post_2020_national_security_law, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('af9191d9-99a3-4ff3-adc4-cb1637898379', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_sector).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, mainland_prc_leadership).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_autonomy_advocates).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, mainland_dissidents_sheltered_in_hk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary_and_legal_profession).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary_and_legal_profession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the territorial and sovereignty boundaries of One Country, Two Systems and enforces the framework through legislative acts (Hong Kong security laws, constitutional interpretations), appointment of officials, and control of military/police assets. Under this balanced reading, must negotiate disputed boundaries rather than unilaterally impose them, but retains override authority in national security matters. Collects political legitimacy from the arrangement's stability.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from preserved civil liberties, judicial independence, and rule of law relative to mainland system; can organize, speak, and litigate in ways mainland citizens cannot. Pays through periodic security restrictions, surveillance expansion, and self-censorship when boundaries are redrawn. Under the balanced reading, retains meaningful institutional voice in renegotiation — not absolute veto power, but genuine input channel and credible exit threat (capital flight, intellectual-talent emigration).
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer).

% Benefits from Hong Kong's international financial status, separate legal system that enforces contracts predictably, and access to mainland markets without full mainland regulatory regime. Pays through tariffs, regulatory uncertainty during crisis periods, and political loyalty expectations. Under the balanced reading, possesses arbitrage power: can relocate to Singapore or London if Hong Kong's special status erodes beyond viability threshold.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_sector, beneficiary,
    powerful, biographical, mobile, global).

% Benefits from Hong Kong as a demonstration of political integration, international business hub that stabilizes the regional economy, and a channel for international engagement. The arrangement vindicates national sovereignty while managing the complexity of retaining economic dynamism. Under the balanced reading, absorbs political cost of negotiation and periodic concessions to civil society rather than unilateral assertion.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, mainland_prc_leadership, beneficiary,
    institutional, civilizational, analytical, national).

% Seek expansion of Hong Kong autonomy beyond current boundaries — democratic accountability of the executive, directly elected legislature, independent constitutional authority. They are systematically constrained by the framework under the balanced reading: they can negotiate on specific provisions but cannot challenge the One Country principle or PRC sovereignty. Their exit is migration (identity-locked to Hong Kong political status) or continued struggle with limited institutional channels.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_autonomy_advocates, payer,
    organized, biographical, trapped, local).

% Depend on Hong Kong's separate legal jurisdiction to maintain asylum status and organize political speech. Under the balanced reading, this protection is periodically negotiated and weakened through security law expansions (political asylum deprioritized, extradition exposure increased). They are systematically exposed when PRC leadership prioritizes national security over civil liberty accommodation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, mainland_dissidents_sheltered_in_hk, payer,
    powerless, biographical, trapped, local).

% Monitor Hong Kong's autonomy status as a signal of PRC commitment to legal boundaries and international obligation. Can impose trade/investment penalties if Hong Kong's status degrades, or offer capital and diplomatic recognition if accommodation is sustained. Under the balanced reading, serve as external enforcement infrastructure for renegotiation: credibly threaten departure if boundaries collapse.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_observers_and_trade_partners, observer,
    institutional, biographical, analytical, global).

% Benefit from independent judicial authority that differentiates Hong Kong from mainland system. Pay through case law reversals (final appeals lodged to Standing Committee of NPC rather than Hong Kong Court of Final Appeal on security matters), disciplinary oversight from Beijing-aligned authorities, and brain drain as judges and lawyers emigrate to retain professional independence. Under the balanced reading, the judiciary retains operational independence on commercial and civil matters but acknowledges PRC authority on security/sovereignty questions.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary_and_legal_profession, beneficiary,
    powerful, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary_and_legal_profession, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_leadership).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles two incompatible legal systems — PRC constitutional sovereignty and Hong Kong common law autonomy — into a stable modus vivendi that allows both to coexist without constant collision. Solves the governance problem of integrating Hong Kong economically while preserving institutional differentiation. Provides international credibility to PRC legal boundaries and Hong Kong commercial predictability.
% TRANSFER_FUNCTION: Transfers political power and legal authority from Hong Kong elected bodies to PRC-appointed bodies in security/sovereignty domains; transfers institutional stability and autonomy preservation from PRC to Hong Kong in civil/commercial/cultural domains. Moves loyalty expectations from Hong Kong toward mainland in national-security moments; moves investment and talent retention favoring Hong Kong during accommodation periods.
% ABSENT_VOICES: Hong Kong independence advocates (structurally excluded by the One Country premise); dissidents from mainland and Tibet (structurally constrained by PRC security authority); international human rights organizations (lacking institutional seat but exerting pressure through leverage mechanisms). Also absent: direct voices of mainland Han citizens, who bear no direct cost but legitimize the arrangement through territorial cohesion narratives.
% DISAPPEARANCE_RATIONALE: If One Country, Two Systems framework vanished, Hong Kong's stock market and currency would likely experience immediate pressure; international capital would relocate; the PRC would face legitimacy questions about treaty fidelity and legal predictability; and Hong Kong civil society would face either accelerated assimilation into mainland institutions or escalated independence movements with reduced international support. The arrangement is load-bearing for regional economic stability and PRC credibility on international law.
% FOUNDING_PROBLEM: Hong Kong's 1997 handover required reconciling British common-law jurisdiction, developed civil society, and international commercial integration with PRC sovereignty and constitutional authority. The founding problem: how to integrate Hong Kong into the PRC while preserving the institutional features that made Hong Kong economically valuable and internationally credible.
% FOUNDING_PROBLEM_CORROBORATION: PRC leadership attests the founding problem remains live: Hong Kong's unique status requires continuous management to prevent either separatism or erosion of commercial confidence. Hong Kong business sector attests the problem persists: institutional differentiation is the foundation of Hong Kong's continued role in global finance. International observers attest the founding problem is partially solved but not resolved: Hong Kong's autonomy has eroded substantially since 1997 (2020 National Security Law, 2024 security expansion), yet the framework still functions better than outright absorption would. Academic and journalistic corroboration from outside the benefiting parties documents the steady renegotiation process and periodically unsuccessful attempts to stabilize boundaries.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).
:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under the balanced coexistence reading, extractiveness is MEDIUM (0.45 at equilibrium, peaked at 0.48 during 2019-2020 crisis) because the constraint operates as genuine coordination (both legal systems acknowledged) PLUS asymmetric extraction (PRC retains final authority in security/sovereignty matters, Hong Kong civil society absorbs the cost of renegotiation). Suppression is MODERATE (0.38 baseline, spiked to 0.48 during 2020 National Security Law enforcement) because the framework itself acknowledges limits — suppression operates within an accommodation logic, not total dominance logic. If sovereignty-primacy reading were instantiated, suppression would be higher and extraction would plateau higher; if autonomy-primacy reading were instantiated, suppression would be lower and extraction would approach zero. Theater is MODERATE-HIGH (0.42 equilibrium, peaked at 0.46 during crisis when performative nationalism and security theater were maximized) because the arrangement requires continuous legitimation through constitutional narrative and operational security performance. The measurement series shows the cyclical pattern: years 0-3 (early accommodation post-1997: low extraction, low theater); years 9-15 (2016-2022 security law expansion and protest suppression: extraction and theater rise sharply); years 21-27 (post-crisis equilibrium: extraction and theater settle at moderate levels as suppression becomes operational and less performative). This cyclical structure is diagnostic of tangled rope: periodic crises trigger renegotiation (coordination function reasserts), enforcement ratchets down post-crisis (suppression moderate), then gradually rises again until next crisis. A pure snare would show monotonic extraction rise; a pure rope would show flat low metrics; this constraint's oscillation is the signature of contested boundaries requiring periodic re-accommodation.
 *
 * PERSPECTIVAL GAP:
 *   PRC Central Leadership and Hong Kong Civil Society should compute VERY DIFFERENTLY from the same structural data. From the PRC leadership seat, the arrangement is genuine coordination: OCTS is functional, economically stabilizing, and demonstrates PRC's capacity for sophisticated governance (vindicates sovereignty_with_flexibility doctrine). From the Hong Kong civil society seat, the same arrangement is extractive: civil liberties and democratic voice are periodically suspended, asylum protection erodes, and autonomy expands only when PRC calculates political cost of restriction. From Hong Kong business's seat, it is a PURELY COORDINATIVE arrangement with real-option value: their arbitrage exit (threat to relocate) credibly constrains PRC extraction, so they compute the constraint as rope with residual negotiation premium. From autonomy advocates' seat, it is a SNARE: they are trapped (cannot advocate independence without violating One Country premise), face systematic suppression, and lose every binding renegotiation on the autonomy margin. The engine's per-seat computation will surface these divergences from the structural data — PRC's high power and analytical exit push toward low d (beneficiary end); autonomy advocates' organized-but-trapped status and systematic-loss history push toward high d (target end).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (d → beneficiary end, ~0.15-0.25): (1) Hong Kong civil society: receives preserved civil liberties, judicial independence, organized-action space; organized power + constrained exit make them partially mobile but largely capture-dependent; d ≈ 0.20 (moderate beneficiary). (2) Hong Kong business: receives international status, contract predictability, arbitrage option; powerful + mobile exit means low directionality toward constraint (they could leave); d ≈ 0.10 (strong beneficiary). (3) Mainland PRC leadership: receives political legitimacy, economic stability, international credibility; institutional power + arbitrage makes them independence-capable; d ≈ 0.15 (moderate beneficiary). TARGET DIRECTIONALITY (d → target end, ~0.75-0.95): (1) Hong Kong autonomy advocates: trapped + identity-locked + systematically lose boundary renegotiations; organized but structurally powerless in final decision chain; d ≈ 0.85 (strong target). (2) Mainland dissidents in HK: powerless + trapped + first to lose protection when boundaries shift; zero exit; d ≈ 0.92 (extreme target). The effective extraction (χ) computed from these directionalities will be MODULATED by scope (GLOBAL scope for Hong Kong business amplifies their beneficiary position via arbitrage; LOCAL scope for autonomy advocates keeps them trapped despite organization). The engine's computation will show: business sector faces low χ (beneficiary modulation); autonomy advocates face high χ (target+trapped modulation).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is TANGLED ROPE under the balanced coexistence reading because it possesses both genuine coordination function (reconciles incompatible legal systems into stable modus vivendi; both sides acknowledge authority limits) AND asymmetric extraction (PRC retains final sovereignty authority; Hong Kong civil society bears the cost of periodic renegotiation and security-boundary expansion). The chart-preventive classification is: (1) Genuine coordination: without OCTS, Hong Kong economy would face chaos (capital flight, international credit freeze); without OCTS, PRC would face legitimacy cost on rule-of-law and integration credibility. The arrangement solves a real coordination problem. (2) Active enforcement: the arrangement requires PRC legislative acts (Basic Law interpretations, National Security Law implementation), appointment of officials, and police/military enforcement of security perimeter. It does not self-maintain. (3) Asymmetric extraction is clear: civil society pays through restricted liberties during crisis periods; autonomy advocates pay through systematic loss of boundary renegotiation; mainland dissidents pay through asylum exposure. Beneficiaries (PRC leadership, HK business, HK civil society during accommodation periods) are named in base_properties.beneficiaries; victims (autonomy advocates, mainland dissidents) are named in base_properties.victims. The alternative readings diverge on which side of coordination/extraction the arrangement occupies: sovereignty-primacy reading would argue OCTS is primarily extraction (Hong Kong autonomy is revocable; PRC authority is supreme) masquerading as coordination; autonomy-primacy reading would argue OCTS is primarily coordination (Hong Kong autonomy is treaty-protected) with only incidental extraction. This balanced reading occupies the middle: genuine coordination + genuine extraction, held in tension through periodic renegotiation. This is the definition of tangled rope, not a misallocation to rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_risk,
    'As enforcement intensifies and autonomy protections erode, does the balanced reading eventually logically foreclose the autonomy-primacy reading (making them incoherent in a single framework)?',
    'Monitored through constitutional interpretation outcomes: if final appeals (NPC Standing Committee interpretations) systematically narrow what counts as autonomy, the structural basis for coexistence erodes and readings move from coexist to foreclose. Trigger: if PRC interprets OCTS to mean autonomy has zero veto power over security matters, the autonomy-primacy reading becomes empirically untenable (not foreclosed, but abandoned as descriptively false).',
    'If readings foreclose rather than coexist, the constraint reclassifies from TANGLED ROPE (negotiated tension) to SNARE (autonomy is nominally preserved but substantively evacuated). This would signal terminal drift toward sovereignty-primacy reading as the operative framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_risk, empirical, 'Whether coexistence of readings remains structurally possible or whether erosion of autonomy boundaries forecloses balanced/autonomy readings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.38-0.48) primarily structural (external barriers: legal prohibition, enforcement infrastructure) or primarily internalized (cognitive adaptation, organizational learned caution)?',
    'Post-crisis natural experiment: if suppression barriers are formally relaxed (NSL enforcement becomes selective, security perimeter is explicitly narrowed), would autonomy advocates resume expansive claims immediately, or would internalized caution persist?',
    'If suppression is primarily structural, relaxation would re-activate autonomous organization and reduce extraction. If suppression is primarily internalized, organizational caution would persist even after barrier relaxation, and extraction would remain stable — the constraint would have shifted from structural to internalized, reducing visible suppression while maintaining behavioral suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    business_sector_exit_credibility,
    'Does Hong Kong business sector''s arbitrage exit option (threat to relocate capital/talent to Singapore/London) remain credible as a constraint on PRC extraction?',
    'Measured through foreign direct investment flows, talent emigration rates, and corporate relocation announcements. Trigger: if capital and talent remain stable or increase despite autonomy erosion, exit threat is revealed as non-credible (firms are locked in through integration with mainland supply chains or PRC market dependence).',
    'If exit remains credible, HK business retains beneficiary-to-negotiator position and can moderate PRC extraction. If exit becomes non-credible (sunk costs/market dependence too high), HK business effectively becomes trapped alongside autonomy advocates, and extraction pressure would rise substantially across all Hong Kong seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(business_sector_exit_credibility, empirical, 'Whether Hong Kong business arbitrage exit threat remains credible as enforcement mechanism on constraint boundaries.').

omega_variable(
    international_enforcement_capacity,
    'Do international observers (trade partners, human rights bodies, investment regulators) retain capacity to enforce OCTS boundaries through capital flows and trade pressure?',
    'Monitored through: (1) Hong Kong''s international financial status (forex reserves, credit ratings, capital inflows); (2) bilateral trade agreements with HK-specific status; (3) migration/talent flows from developed democracies; (4) corporate ESG investment decisions. Trigger: if international pressure dissipates (no capital flight despite autonomy erosion, trade continues at full volume, migration remains low-pressure), enforcement mechanism is revealed as performative.',
    'If international enforcement capacity erodes, the constraint loses its external enforcement infrastructure and becomes dependent entirely on internal negotiation dynamics. PRC extraction would rise, theater would increase (performative restoration of autonomy narrative to restore international confidence), and the constraint would drift toward SNARE.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_capacity, empirical, 'Whether international enforcement mechanisms (capital flows, trade pressure) constrain PRC extraction or have become decorative.').

omega_variable(
    alternative_reading_activation,
    'Which sibling reading (autonomy-primacy or sovereignty-primacy) becomes the operative framework when the balanced reading fails to produce renegotiation outcomes?',
    'Tracked through political rhetoric, constitutional interpretation, and institutional decision-making: if crises consistently resolve toward autonomy expansion, autonomy-primacy reading becomes operative; if crises consistently resolve toward PRC security authority, sovereignty-primacy reading becomes operative.',
    'Whichever reading becomes operative will instantiate a different constraint type and classification: autonomy-primacy would be ROPE (low extraction, genuine coordination); sovereignty-primacy would be SNARE (high extraction, autonomy is performative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_activation, conceptual, 'Which alternative reading becomes operative if balanced coexistence fails as a framework for renegotiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(one__tr_t3, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 3, 0.24).
narrative_ontology:measurement(one__tr_t9, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 9, 0.35).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(one__tr_t21, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 21, 0.42).
narrative_ontology:measurement(one__tr_t27, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 27, 0.42).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(one__be_t3, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(one__be_t9, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 9, 0.38).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(one__be_t21, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 21, 0.45).
narrative_ontology:measurement(one__be_t27, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 27, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(one__su_t3, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 3, 0.21).
narrative_ontology:measurement(one__su_t9, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 9, 0.32).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(one__su_t21, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 21, 0.41).
narrative_ontology:measurement(one__su_t27, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 27, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__balanced_coexistence_reading, 0.18).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_security_law_implementation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, prc_constitutional_authority_over_hong_kong).

% DUAL FORMULATION NOTE:
% This story is one reading of the One Country, Two Systems kernel. Three constraint stories instantiate the three readings: autonomy-primacy (ROPE: genuine coordination, international enforceability), sovereignty-primacy (SNARE: delegated autonomy, PRC override), and balanced-coexistence (TANGLED ROPE: negotiated tension, periodic renegotiation). All three share the same referent (the OCTS arrangement itself) but author different epsilon values and classify into different types because the readings differ on what the arrangement actually IS. The network links show epistemic influence: sovereignty-primacy reading forecloses certain aspects of autonomy-primacy framing (PRC absolute authority makes treaty-guaranteed international enforceability incoherent), but balanced reading coexists with both (treats both as live positions held by different constituencies). This is not a redundant triple-counting of one constraint — it is the proper decomposition of a contested kernel into its constituent readings per the ε-invariance principle and Rule 1 of kernel authoring.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
