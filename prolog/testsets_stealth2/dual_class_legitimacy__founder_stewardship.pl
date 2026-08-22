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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Founder Stewardship Reading of Dual-Class Control
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   A founder takes a company public with two (or three) classes of stock:
 *   the founder bloc holds supervoting shares carrying a multiple of votes
 *   per unit of economics, while the public buys low-vote or non-voting
 *   shares. The founder_stewardship reading holds this arrangement is
 *   legitimate coordination: concentrated control insulates mission-critical,
 *   slow-payoff investment from quarterly earnings pressure, activist
 *   campaigns, and hostile takeover, and the founder acts as fiduciary
 *   steward whose control is service rather than rent. Public holders are
 *   said to benefit indirectly through mission success. The standing
 *   arrangement under contest — the dual-class structure itself — nonetheless
 *   carries a real transfer: voting power decoupled from capital exposure
 *   concentrates strategic discretion and private benefits of control in the
 *   founder bloc, and public holders bear the pricing discount and the loss
 *   of removal rights. This file instantiates ONLY the stewardship reading as
 *   a clean, epsilon-invariant constraint; the minority_extraction and
 *   disclosure_consent readings of the same kernel are separate stories with
 *   their own epsilon and victim sets. KEY AGENTS (by structural
 *   relationship): - founding_founders: Agenda-setter and primary beneficiary
 *   (institutional/arbitrage) — designs, administers, and collects from the
 *   structure - supervoting_insider_holders: Aligned beneficiary
 *   (powerful/identity_locked) — early holders whose standing and wealth ride
 *   on the founder bloc - class_a_public_investors: Cost-bearing participant
 *   (organized/constrained) — supplies most capital, holds few or no votes,
 *   bears the discount - employee_equity_holders: Secondary beneficiary with
 *   cost exposure (moderate/constrained) — mission upside via equity,
 *   concentration risk via compensation - blocked_hostile_bidders: Excluded
 *   party (powerful/mobile) — the demand side the structure keeps outside -
 *   proxy_advisory_firms, index_providers, securities_regulators: Observers
 *   shaping legitimacy conditions from analytical and institutional seats
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.38).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.52).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.38).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Founder Stewardship Reading of Dual-Class Control").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b').
narrative_ontology:cs_kernel_codification('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', formalized).
narrative_ontology:cs_authority_grounding('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', lineage).
narrative_ontology:cs_interpretation_layer_present('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b').
narrative_ontology:cs_reading_relation('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', foundational, control_concentration_enables_long_horizon_mission_execution).
narrative_ontology:cs_axiom_status(control_concentration_enables_long_horizon_mission_execution, holdable).
narrative_ontology:cs_axiom_grounding('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', control_concentration_enables_long_horizon_mission_execution, empirically_contingent).
narrative_ontology:cs_axiom('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', foundational, founder_control_is_fiduciary_service_not_rent).
narrative_ontology:cs_axiom_status(founder_control_is_fiduciary_service_not_rent, holdable).
narrative_ontology:cs_axiom_grounding('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', founder_control_is_fiduciary_service_not_rent, deontological).
narrative_ontology:cs_reference_frame('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', founder_as_fiduciary_steward).
narrative_ontology:cs_drift_state('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', contemporary_entrenchment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2d4182d9-b81e-4ddf-8c1e-f3cf3f3b1c6b', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founding_founders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, supervoting_insider_holders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, employee_equity_holders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_public_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, class_a_public_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, employee_equity_holders).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, founder_stewardship_doctrine).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, long_horizon_superiority_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold supervoting shares carrying a multiple of votes per unit of economic ownership, chair the board, and set strategy for the listed company. Designed the share-class structure before the IPO and can amend, defend, or collapse it through charter provisions they control. Their personal wealth, public identity, and stated life project are bound to the company's mission; what flows to them is durable strategic discretion, compensation-setting power, and immunity from removal, alongside the labor of executing the mission they chose.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founding_founders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, founding_founders, beneficiary).

% Early employees, pre-IPO investors, and family members holding high-vote shares acquired before the listing. Their liquid net worth is concentrated in the company and their standing inside it depends on continued alignment with the founder bloc. Selling would mean exiting both the control-aligned upside and the custodian role they occupy inside the organization, so holdings persist across decades and their self-concept is constituted through the mission they help shield.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, supervoting_insider_holders, beneficiary,
    powerful, biographical, identity_locked, global).

% Buy publicly traded low-vote or non-voting shares after the listing, supplying most of the company's outside capital while holding few or no votes. They receive dividends and appreciation if the mission succeeds and bear the issuance discount and the absence of removal rights if it does not. Exit consists of selling on the open market, typically at the discount the share structure itself produces; index-tracking funds among them must continue holding while the stock remains eligible, and coordinated dissent cannot reach the charter threshold the founder bloc controls.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_public_investors, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, class_a_public_investors, beneficiary).

% Current and former staff paid partly in restricted stock and options tied to the low-vote class. Unvested tranches, vesting schedules, and the specialized labor market tie them to the employer for years at a time. They share mission upside through equity compensation and stable long-horizon employment, and they bear concentration risk through undiversified pay with no governance voice attached to it.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, employee_equity_holders, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, employee_equity_holders, payer).

% Strategic acquirers and activist funds that would pay a control premium to public holders in exchange for influence over strategy, assets, or direction. The share structure forecloses their route in regardless of the price offered, because the votes they would need are not for sale in any quantity that matters. They deploy capital toward other targets and enter this story only as the demand side that the structure keeps permanently outside the room.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, blocked_hostile_bidders, excluded,
    powerful, immediate, mobile, global).

% Publish voting recommendations and governance ratings consumed by institutional investors. They campaign against perpetual dual-class structures and for sunset clauses, and their recommendations shape votes on director elections and charter amendments that they do not themselves cast. Their leverage is reputational and informational rather than proprietary.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, proxy_advisory_firms, observer,
    institutional, biographical, analytical, global).

% Set index eligibility rules that determine which funds must hold which stocks. One major provider excludes newly listed dual-class companies from its flagship index; others admit them with weighting discounts or committee review. Their rulebooks shift where dual-class issuers can list, what their cost of capital is, and who is compelled to hold them.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, index_providers, observer,
    institutional, generational, analytical, global).

% Permit dual-class listings under disclosure regimes and have repeatedly declined to mandate one-share-one-vote, reviewing offering documents rather than post-listing governance terms. They revisit the question after listing waves and governance failures, balancing capital-formation interests against holder-protection mandates, and their jurisdiction stops at the boundary of the disclosure event.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founding_founders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the long-horizon commitment problem: public markets repriced quarterly interrupt multi-year investments, and concentrated founder control commits the firm to slow-payoff strategies (research programs, infrastructure, content libraries) that activist pressure or hostile takeover would otherwise truncate mid-build. It also protects a founder-specific match — the founder's human capital is inseparable from the mission, and control preserves that pairing against separation events.
% TRANSFER_FUNCTION: Moves governance control disproportionate to capital from the public float to the founder bloc at the IPO; continuously moves the control premium, private benefits of control (compensation-setting, related-party latitude, strategic discretion), and entrenchment insurance toward insiders; moves the pricing discount and removal-risk onto public holders in exchange for contingent participation in mission upside.
% ABSENT_VOICES: Blocked hostile bidders who would pay public holders a control premium are structurally outside; sunset advocates inside institutional-investor coalitions can speak in proxy season but cannot reach the charter threshold; future cohorts of public shareholders inherit the structure years after the consent event that allegedly legitimated it, without ever having been in the room. The charter-amendment process is where they would need to be, and the founder bloc's supervoting majority is what keeps the door shut.
% DISAPPEARANCE_RATIONALE: If the structure vanished overnight, control premia would redistribute to public holders, several long-cycle investment programs would come under activist and takeover pressure within quarters, founder-firm matches would reprice, and the market for corporate control would reopen across a cohort of firms currently immune to it — the governance landscape would reorganize around one-share-one-vote rather than stay roughly as it is.
% FOUNDING_PROBLEM: At IPO, founders confronting public capital markets faced a credible threat: quarterly earnings discipline, activist campaigns, and hostile bids interrupt mission-specific investments whose payoffs mature over decades. Dual-class structures were built to solve the founder's control-loss problem while still accessing public capital — protecting the long path from the short clock.
% FOUNDING_PROBLEM_CORROBORATION: That short-horizon pressure exists is corroborated from outside the benefiting parties: proxy-advisor research, Council of Institutional Investors policy statements, and the academic finance literature on managerial myopia all attest the phenomenon. But the same outside sources dispute whether the problem remains unsolvable under one-share-one-vote and whether permanent dual-class is proportionate to it; no source outside the founder bloc attests that the protective function still requires indefinite, sunset-free control. Plainly stated: corroboration for the problem's existence is external; corroboration for the structure's continuing necessity is almost entirely internal to the benefiting parties.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.38, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.38 as the stewardship reading assesses the standing arrangement by its own lights: the coordination function is genuine (multi-year investment programs at dual-class firms demonstrably survive market cycles that interrupt comparable single-class programs), so epsilon sits well below what an extraction-focused reading would author over the same referent — but not near zero, because the reading itself concedes a real transfer (vote-economics wedge, private benefits of control, no sunset) that no fiduciary framing fully dissolves. Suppression is 0.52: the structure forecloses the one-share-one-vote alternative through charter supermajorities the founder bloc controls, and dissenting holders' exit is sale at the very discount the structure creates — contractual lock-in rather than coercion, but real. Theater is 0.28: mission-protection is performed and real, but a growing share of stewardship discourse (annual engagement rituals, vision letters) substitutes for the sunset mechanisms that would make the stewardship claim testable. Accessibility_collapse is 0.45 — capital retains workable alternatives (invest elsewhere; the option set does not collapse for mobile money), while governance voice does collapse for anyone already inside. Resistance is 0.55: proxy-advisor adverse recommendations, flagship-index exclusion, and institutional-investor sunset campaigns constitute sustained, organized opposition that has raised listing costs without displacing existing structures. Suppression is authored as a raw structural property and is not scaled by context; the engine scales only extractiveness, by directionality and spatial scope. All three temporal series run on one shared grid (t=0,4,8,12,16,20) so every metric is authored at every examined point; trajectories are monotonic — extraction and theatrical maintenance creep upward as structures age past their mission phase without sunsets, and enforcement effort rises as defense of the structure shifts from novelty to active litigation and charter defense.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the founder seat, the arrangement is infrastructure the founder built and personally guarantees: control equals accountability to the mission, and the vote wedge equals the price of being unable to walk away from a life's work. From the class A holder seat, the same charter reads as paying full price for partial voice: the discount is observable at issuance, removal rights are absent, and the promised offset arrives only if the mission succeeds — a contingency the holder cannot audit. From the regulator seat, the arrangement is a disclosure artifact: consent was obtained under Securities Act process, and post-IPO governance quality is outside the permitting mandate. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Founders sit near the beneficiary pole (override d=0.08): they collect strategic discretion, private benefits, and entrenchment insurance while bearing mission-execution labor they would undertake anyway. Supervoting insiders derive a low d from their beneficiary declaration — their costs (wealth concentration) are voluntarily aligned. Class A public investors are the pivot seat and the reason overrides exist: they appear in BOTH the beneficiary and victim arrays (indirect mission upside; direct vote-dilution and discount costs), so mechanical derivation cannot resolve their net position — the override sets d=0.58, slightly target-side, reflecting that the transfer runs from them even under the reading's most charitable accounting. Employee equity holders override to d=0.28: mostly subsidized by mission stability, partially paying via undiversified compensation. Observers (proxy advisors, index providers, regulators) and the excluded bidders carry analytical or out-of-scope positions and are left to structural derivation. Scope amplification applies modestly: these are globally listed firms where verification of the stewardship claim is dispersed across jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite misreadings. Reading the arrangement as pure rope accepts the stewardship cover story and misses the transfer that the reading itself concedes; reading it as pure snare erases the documented coordination function (mission-phase investment survival) and predicts repeal pressure that the empirical record does not show. Tangled rope holds both truths: genuine coordination AND asymmetric transfer, joined by active enforcement (charter supermajorities, takeover defenses, litigation posture). The mandatrophy question turns on the founding problem's status: short-horizon market pressure is real and corroborated from outside the benefiting parties, but whether it remains unsolvable under one-share-one-vote is disputed by the same outside sources. If the horizon_attribution_confound omega resolves against insulation value, the coordination half atrophies while the transfer half persists under lock-in — the classic drift path toward piton, detectable via the rising theater_ratio series. The absence of any sunset clause is the structural fact that keeps this question live rather than closed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This constraint is the founder_stewardship reading of the dual_class_legitimacy kernel; how would instantiating the sibling readings (minority_extraction, disclosure_consent) change the epsilon and victim structure over the same standing arrangement?',
    'Author the sibling stories separately and compare computed classifications; the disagreement is located in whether the vote-economics asymmetry is a coordination cost fairly priced at IPO (this reading), an entitlement violation requiring proportional governance (minority_extraction), or a settled matter of informed consent under disclosure law (disclosure_consent).',
    'Under minority_extraction, epsilon rises substantially and class_a_public_investors become unambiguous victims; under disclosure_consent, epsilon falls toward procedural minimums and the victim set empties. This file''s 0.38 is valid only for the stewardship reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Reading-indexed epsilon over a shared kernel referent; sibling readings are separate constraints.').

omega_variable(
    compensation_sufficiency,
    'Does indirect participation in mission upside actually compensate Class A holders for surrendered voting rights and the governance discount, or is the compensation claim unfalsifiable as authored?',
    'Event studies matching dual-class firms to one-share-one-vote twins at IPO and over decade horizons; natural experiments from firms that adopted time-based sunsets versus matched firms that did not.',
    'If compensation is sufficient on average, the transfer component is a priced exchange and the coordination reading strengthens; if not, the same structure computes as extraction riding on mission rhetoric and the tangled_rope verdict hardens toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_sufficiency, empirical, 'Whether the mission-upside offset to vote dilution is real, priced, and sufficient.').

omega_variable(
    horizon_attribution_confound,
    'Are long-horizon outcomes attributable to control insulation, or to founder ability — such that the structure takes credit for selection effects?',
    'Compare post-IPO mission investment persistence and operating outcomes for founder-led dual-class firms against founder-led single-class firms with equivalent founder equity, isolating the vote wedge from the economic stake.',
    'If insulation adds nothing beyond founder ability, the coordination half of the arrangement atrophies while the transfer half persists — the lifecycle signature of a constraint drifting from tangled_rope toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizon_attribution_confound, empirical, 'Selection-versus-treatment confound in the stewardship causal claim.').

omega_variable(
    sunset_counterfactual,
    'Would time-based sunsets (votes converging to one-share-one-vote after a fixed period or trigger) preserve the mission-protection function while eliminating indefinite entrenchment?',
    'Observe the growing population of sunset-equipped dual-class IPOs and compare governance outcomes, takeover incidence, and mission investment against perpetual dual-class firms.',
    'If sunsets preserve the coordination benefit, the absence of a sunset clause in this arrangement is revealed as extraction-preserving choice rather than functional necessity, raising effective extraction for the target seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_counterfactual, preference, 'Whether a transitional variant could deliver the same coordination at lower transfer.').

omega_variable(
    entrenchment_vs_mission_phase,
    'Is the structure still protecting an active mission-critical build phase, or has it outlived the phase it was built for while persisting through charter lock-in?',
    'Assess per-firm whether current capital allocation still exhibits the long-horizon profile the structure is said to protect, and whether founders cite mission necessity or competitive-prevention when defending the structure in proxy materials.',
    'If the mission phase has closed, the founding problem is dead while the arrangement persists — the mismatch flag fires and the constraint''s maintenance becomes predominantly theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_vs_mission_phase, empirical, 'Whether the protective justification tracks a live mission phase or inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcls_fs_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(dcls_fs_tr_t0, observed).
narrative_ontology:measurement(dcls_fs_tr_t4, dual_class_legitimacy__founder_stewardship, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(dcls_fs_tr_t4, observed).
narrative_ontology:measurement(dcls_fs_tr_t8, dual_class_legitimacy__founder_stewardship, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(dcls_fs_tr_t8, observed).
narrative_ontology:measurement(dcls_fs_tr_t12, dual_class_legitimacy__founder_stewardship, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(dcls_fs_tr_t12, observed).
narrative_ontology:measurement(dcls_fs_tr_t16, dual_class_legitimacy__founder_stewardship, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(dcls_fs_tr_t16, observed).
narrative_ontology:measurement(dcls_fs_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(dcls_fs_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(dcls_fs_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(dcls_fs_be_t0, observed).
narrative_ontology:measurement(dcls_fs_be_t4, dual_class_legitimacy__founder_stewardship, base_extractiveness, 4, 0.32).
narrative_ontology:measurement_basis(dcls_fs_be_t4, observed).
narrative_ontology:measurement(dcls_fs_be_t8, dual_class_legitimacy__founder_stewardship, base_extractiveness, 8, 0.34).
narrative_ontology:measurement_basis(dcls_fs_be_t8, observed).
narrative_ontology:measurement(dcls_fs_be_t12, dual_class_legitimacy__founder_stewardship, base_extractiveness, 12, 0.36).
narrative_ontology:measurement_basis(dcls_fs_be_t12, observed).
narrative_ontology:measurement(dcls_fs_be_t16, dual_class_legitimacy__founder_stewardship, base_extractiveness, 16, 0.37).
narrative_ontology:measurement_basis(dcls_fs_be_t16, observed).
narrative_ontology:measurement(dcls_fs_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(dcls_fs_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(dcls_fs_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(dcls_fs_su_t0, observed).
narrative_ontology:measurement(dcls_fs_su_t4, dual_class_legitimacy__founder_stewardship, suppression_requirement, 4, 0.43).
narrative_ontology:measurement_basis(dcls_fs_su_t4, observed).
narrative_ontology:measurement(dcls_fs_su_t8, dual_class_legitimacy__founder_stewardship, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(dcls_fs_su_t8, observed).
narrative_ontology:measurement(dcls_fs_su_t12, dual_class_legitimacy__founder_stewardship, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(dcls_fs_su_t12, observed).
narrative_ontology:measurement(dcls_fs_su_t16, dual_class_legitimacy__founder_stewardship, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(dcls_fs_su_t16, observed).
narrative_ontology:measurement(dcls_fs_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(dcls_fs_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, hostile_takeover_market_discipline).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'dual-class share structures' decomposes into three structurally distinct claims over one kernel (dual_class_legitimacy). This file is the founder_stewardship member (epsilon 0.38, coordination-forward, Class A holders net-compensated by hypothesis). The minority_extraction member authors high epsilon over the same referent with an unambiguous victim set; the disclosure_consent member authors near-procedural epsilon with legitimacy resting on the IPO consent event. Each member links the others via network.affects_constraints; the upstream member (disclosure_consent, highest empirical confidence — the consent record is documentary) influences the downstream contested members because issuers cite completed disclosure process as evidence of legitimacy in stewardship defenses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__founder_stewardship, institutional, 0.08).
constraint_indexing:directionality_override(dual_class_legitimacy__founder_stewardship, organized, 0.58).
constraint_indexing:directionality_override(dual_class_legitimacy__founder_stewardship, moderate, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
