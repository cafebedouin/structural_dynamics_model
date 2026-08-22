% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__disclosure_consent, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Control Legitimized by Securities Act Disclosure and Informed Consent
 *   domain: economic/legal/governance
 *
 * SUMMARY:
 *   Modern technology and media IPOs routinely pair public equity sales with
 *   super-voting founder stock: Class A shares carry one vote, founder-held
 *   Class B shares carry ten, and the ratio is fixed in the charter at
 *   listing. This story instantiates the disclosure_consent reading of the
 *   dual_class_legitimacy kernel: on this reading the arrangement is neither
 *   coordination scheme nor extraction device but a disclosed contractual
 *   choice -- the S-1 registration statement states the vote differential
 *   plainly, buyers purchase with that knowledge, and the governance
 *   disparity is compensated in the price Class A buyers accept. The referent
 *   of every metric below is the standing dual-class arrangement itself,
 *   assessed by this reading's own lights; the sibling readings
 *   (founder_stewardship, minority_extraction) assess the identical
 *   arrangement under different legitimacy criteria and are authored as
 *   separate constraint files linked through the network section.
 *   Reading-indexed epsilon therefore lands low (0.30 at interval end): the
 *   reading concedes a residual gap between formal disclosure and actual
 *   comprehension, and a growing stratum of holders whose acquiescence is
 *   mechanical rather than consensual, but it holds that disclosed, priced,
 *   voluntary purchase dissolves the parity objection. Claim and metrics are
 *   authored independently: the rope claim is this reading's structural
 *   verdict under test, and the engine computes per-seat classifications from
 *   the structural data without reference to it. KEY AGENTS (by structural
 *   relationship): - founder_controlling_holders: agenda-setting insider seat
 *   (powerful/identity_locked) -- drafts the charter, sets the vote ratio,
 *   collects the control allocation - class_a_active_investors: consenting
 *   buyer seat (organized/mobile) -- prices the governance discount, exits
 *   freely - passive_index_funds: mandate-bound holder seat
 *   (institutional/trapped) -- holds mechanically, bears exposure without a
 *   consent act - retail_class_a_buyers: thin-consent seat (powerless/mobile)
 *   -- buys on headline disclosure - sec_disclosure_administrators: regime
 *   administrator (institutional/constrained) -- polices disclosure adequacy,
 *   silent on parity - proxy_advisors_and_governance_bodies: analytical
 *   opposition seat (organized/analytical) - foreign_listing_venues: excluded
 *   jurisdictional seat (institutional/constrained)
 *
 * KEY AGENTS:
 *   - founder_controlling_holders: agenda-setting insider seat (powerful/identity_locked) -- authors the structure and receives its control allocation
 *   - class_a_active_investors: consenting buyer seat (organized/mobile) -- the seat whose priced consent the reading's legitimacy rests on most securely
 *   - passive_index_funds: mandate-bound holder seat (institutional/trapped) -- same power atom as active investors, opposite exit structure
 *   - retail_class_a_buyers: thin-consent seat (powerless/mobile) -- the consent the claim depends on is thinnest here
 *   - sec_disclosure_administrators: administrator of the disclosure machinery the reading leans on
 *   - proxy_advisors_and_governance_bodies: analytical seat generating the arrangement's visible resistance
 *   - foreign_listing_venues: excluded seat -- would condition or refuse dual-class listings but loses issuers to venue competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.3).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.22).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.3).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Control Legitimized by Securities Act Disclosure and Informed Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "economic/legal/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '97776203-5765-4688-9326-8c5b7409f7f9').
narrative_ontology:cs_kernel_codification('97776203-5765-4688-9326-8c5b7409f7f9', formalized).
narrative_ontology:cs_authority_grounding('97776203-5765-4688-9326-8c5b7409f7f9', practice).
narrative_ontology:cs_interpretation_layer_present('97776203-5765-4688-9326-8c5b7409f7f9').
narrative_ontology:cs_reading_relation('97776203-5765-4688-9326-8c5b7409f7f9', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('97776203-5765-4688-9326-8c5b7409f7f9', dual_class_legitimacy__minority_extraction, forecloses).
narrative_ontology:cs_axiom('97776203-5765-4688-9326-8c5b7409f7f9', foundational, informed_consent_displaces_control_parity).
narrative_ontology:cs_axiom_status(informed_consent_displaces_control_parity, holdable).
narrative_ontology:cs_axiom_grounding('97776203-5765-4688-9326-8c5b7409f7f9', informed_consent_displaces_control_parity, conventional).
narrative_ontology:cs_axiom('97776203-5765-4688-9326-8c5b7409f7f9', secondary, vote_differential_is_priced_consideration).
narrative_ontology:cs_axiom_status(vote_differential_is_priced_consideration, holdable).
narrative_ontology:cs_axiom_grounding('97776203-5765-4688-9326-8c5b7409f7f9', vote_differential_is_priced_consideration, empirically_contingent).
narrative_ontology:cs_reference_frame('97776203-5765-4688-9326-8c5b7409f7f9', disclosed_consent_transaction_baseline).
narrative_ontology:cs_drift_state('97776203-5765-4688-9326-8c5b7409f7f9', contemporary_index_ownership_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97776203-5765-4688-9326-8c5b7409f7f9', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founder_controlling_holders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_active_investors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, retail_class_a_buyers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, class_a_active_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, passive_index_funds).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founded the company, retain super-voting Class B shares (typically ten votes per share) written into the charter at IPO, decide whether to include sunset provisions, and nominate a majority of the board indefinitely. They drafted the disclosure and set the vote ratio themselves; their exit would mean selling down or converting their stock, which they experience as abandoning the mission the firm exists to pursue.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founder_controlling_holders, agenda_setter,
    powerful, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, founder_controlling_holders, beneficiary).

% Institutional asset managers and hedge funds that buy Class A at the offering or in the aftermarket knowing the vote ratio, underwrite the governance discount into their valuation models, and can sell on any trading day. They receive access to founder-led growth firms that one-share-one-vote compulsion would keep private, and they bear the agency risk they believe they priced.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_active_investors, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, class_a_active_investors, payer).

% Index-tracking managers must hold every constituent of the indices they track, including dual-class issuers added by index committees. Their holding is mechanical rather than chosen; they cannot sell without breaking their tracking mandate, and their votes are cast by stewardship teams operating under engagement policies rather than through any consent act tied to this structure. They carry the governance-risk exposure of every dual-class constituent.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, passive_index_funds, payer,
    institutional, generational, trapped, global).

% Individual investors who buy Class A shares through brokers or brokerage products on the strength of headline prospectus disclosure. Few read the full risk-factor section; their consent is real in form but thin in comprehension. They can sell at will but rarely revisit the governance terms they accepted at purchase.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, retail_class_a_buyers, beneficiary,
    powerless, biographical, mobile, national).

% Securities-regulator staff review S-1 registration statements, issue comment letters demanding clearer risk-factor disclosure of the vote differential, and police the adequacy of the disclosure on which this reading's legitimacy rests. They neither set the vote ratio nor opine on whether consent should legitimize it; their mandate ends at disclosure adequacy. Exit looks like career rotation within or out of the agency.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, sec_disclosure_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Proxy-advisory firms and institutional governance coalitions publish voting policies against dual-class structures, campaign for sunset clauses, and downgrade issuer governance ratings. They collect no rents from the arrangement and bear none of its direct costs; their seat is analytical, and their persistent opposition is the visible resistance the arrangement meets.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, proxy_advisors_and_governance_bodies, observer,
    organized, biographical, analytical, global).

% Exchange and regulator counterparts in jurisdictions that long barred dual-class listings watched issuers list elsewhere rather than accept parity conditions, and several eventually admitted dual-class structures under competitive pressure. They would condition or refuse such listings if their objections governed, but venue competition places them outside the effective conversation whenever an issuer can shop jurisdictions.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, foreign_listing_venues, excluded,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, founder_controlling_holders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dissolves the founder's dilemma at the IPO boundary: founders obtain public growth capital without ceding mission control, and capital obtains access to founder-led firms it would otherwise be denied -- the vote ratio is set ex ante, disclosed in the registration statement, and taken or left by each buyer.
% TRANSFER_FUNCTION: Moves voting control from dispersed Class A purchasers to concentrated Class B founders at a ratio the charter fixes (commonly ten-to-one), while moving capital from public markets into the firm; this reading holds the control transfer is compensated by the valuation discount Class A buyers accept at purchase.
% ABSENT_VOICES: Future Class A holders who inherit the structure decades after issuance never sat at the consent table; index-mandated holders are present mechanically but absent as consenters; governance coalitions that object are heard but outvoted by the very votes the structure concentrates; jurisdictions that would condition listings lost their seat when issuers began shopping venues.
% DISAPPEARANCE_RATIONALE: Overnight abolition would reroute the IPO pipeline: founder-controlled firms would delay listing, list in permissive venues, or adopt fallback control devices such as golden shares, staggered boards, and takeover defenses; index compositions and stewardship agendas would shift; the supply of founder-led public firms would thin until substitute arrangements stabilized.
% FOUNDING_PROBLEM: At the public-offering boundary, founders faced a binary: sell control to dispersed holders and expose the mission to short-termism and hostile takeover, or forgo public capital entirely. The arrangement was built to dissolve that binary -- sell the economics, keep the control, and tell every buyer exactly what they are getting.
% FOUNDING_PROBLEM_CORROBORATION: Corporate-law scholarship (the Bebchuk-Kamar-Dammann/Kraakman exchange on dual-class IPOs), securities-regulator rulemaking records from the 1980s one-share-one-vote episode, and exchange consultation papers corroborate from outside the founder set that the founder's dilemma is real and recurring; the same sources dispute whether disclosure plus pricing resolves it, which is the contest this reading sits inside.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__disclosure_consent_tests).
:- end_tests(dual_class_legitimacy__disclosure_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.30: by this reading's lights the arrangement transfers little uncompensated value -- the vote differential is disclosed and discounted -- leaving residual extraction only where consent is thin (retail comprehension, index mechanics) and where pricing lags entrenchment risk; the modest upward drift across the series tracks the growth of sunset-less structures and passive ownership rather than any change in the disclosure machinery itself. Suppression 0.22: entry is voluntary, exit is a keystroke for active holders, and abundant single-class alternatives persist; the residue reflects index-mandate trapping and lockup windows, which the reading treats as ordinary market friction. Suppression is authored as a raw structural property and is left unscaled -- the engine owns any directional or scope scaling of extractiveness only. Theater_ratio 0.24: the disclosure function is real and load-bearing -- risk factors do state the structure -- but risk-factor boilerplate has inflated faster than informational content, so a rising minority of disclosure activity is protective recitation rather than communication. Accessibility_collapse 0.30: understanding the structure does not foreclose alternatives; investors can allocate to single-class firms, and issuers can adopt sunset clauses or roll-off structures. Resistance 0.58: proxy-advisor policies, governance coalitions, and several listing venues have opposed dual-class structures for two decades; the reading deems this resistance misplaced on parity grounds but does not deny it exists. All series run on one shared seven-point grid so every tracked metric is authored at every examined time point; suppression_requirement is deliberately untracked because the enforcement picture is static -- the disclosure regime's intensity is not the dynamic this story traces.
 *
 * PERSPECTIVAL GAP:
 *   Seats at nominally similar power compute differently. Two institutional-power holder seats diverge on exit alone: class_a_active_investors hold mobile exit and experience the arrangement as a priced trade, while passive_index_funds hold mandate-trapped exit and experience the same shares as unchosen exposure -- the engine derives sharply different directionality for them from the identical power atom. The founder seat experiences the structure as self-authored and identity-fused: the people who wrote the vote ratio are the people it protects, and relinquishing it reads as abandoning the mission. The administrator seat touches the constraint only at its legitimacy input and takes no position on parity. Coalition note: index funds collectively could wield decisive voting power against the structure, but collective-action costs and client-concentration pressures keep that coalition latent -- relevant if the engine evaluates coalition potential for low-power or trapped seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (founder_controlling_holders, class_a_active_investors, retail_class_a_buyers) derive low directionality -- the arrangement subsidizes their positions: founders receive the control allocation they drafted, active buyers receive access they priced, retail receives participation on disclosed terms. passive_index_funds carry role payer with trapped exit, deriving high directionality: they bear governance-risk exposure without a consent act or an exit path. No directionality_overrides are authored: the derivation chain distinguishes every seat this story contains, and the coarse power-atom keying of overrides would mislabel same-power seats -- an institutional-level override would drag the trapped index seat toward neutrality alongside the neutral administrator.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem -- the founder's binary between ceding control and forgoing public capital -- remains live for every IPO cohort, so this is not a mandatrophy case: the arrangement's original function still operates. The classification discipline cuts both ways here. Against overreach: calling the arrangement pure extraction erases the real access function -- public capital reaches founder-led firms that would otherwise stay private or list in permissive jurisdictions. Against cover: accepting the consent formula without testing it would launder whatever extraction rides on thin consent -- which is exactly what the consent_quality_attenuation and pricing_sufficiency_of_vote_discount omegas hold open. The rope claim survives only while consent is substantively informed and pricing substantively compensates; the omegas are the tripwires, and the mismatch consumer should read the live founding-problem status against the named-seat gain_flow as the standing tension this family exists to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the disclosure_consent reading of the dual_class_legitimacy kernel; which reading governs assessment determines the constraint''s epsilon, victim set, and type -- how should evaluation proceed when the sibling readings (founder_stewardship, minority_extraction) assign materially different structural verdicts to the identical arrangement?',
    'Cross-reading comparison over the fixed referent: compile all three sibling stories and compare per-seat classifications. The disagreement is located in the legitimacy criterion (disclosed consent vs. stewardship outcome vs. proportional entitlement), not in the facts of the arrangement.',
    'Under minority_extraction the same arrangement computes as substantially extractive with named victims; under founder_stewardship as near-benign coordination. This file''s low reading-indexed epsilon is valid only inside the consent criterion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a contested kernel; siblings are separate constraints over the same referent.').

omega_variable(
    consent_quality_attenuation,
    'Does S-1 disclosure produce actually-informed consent for the holder classes whose acquiescence the legitimacy claim rests on -- retail buyers and index-mandated funds?',
    'Comprehension studies of prospectus readership, order-flow analysis distinguishing informed underwriting allocations from momentum retail demand, and stewardship-policy audits of index managers'' voting on dual-class charter amendments.',
    'If consent is largely nominal for those classes, this reading''s epsilon rises sharply toward the minority_extraction account and the rope claim fails; if consent is substantiated, the low epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_quality_attenuation, empirical, 'Whether formal disclosure yields substantive consent across holder classes.').

omega_variable(
    pricing_sufficiency_of_vote_discount,
    'Is the governance disparity fully priced into Class A valuations at issuance and sustained thereafter, as the reading''s compensation logic requires?',
    'Event studies of dual-class IPO initial returns and long-run return differentials against matched single-class firms, controlling for industry, vintage, and index-inclusion effects.',
    'Full pricing supports the contractual-choice framing; systematic underpricing of entrenchment risk converts the vote differential into an uncompensated transfer and pushes classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pricing_sufficiency_of_vote_discount, empirical, 'Whether market pricing actually compensates Class A holders for surrendered governance weight.').

omega_variable(
    perpetual_binding_without_sunset,
    'Can one-time consent at IPO legitimately bind governance terms that persist indefinitely, including for future holders who never consented, absent any sunset clause?',
    'Comparative doctrine across jurisdictions that mandate time-based sunsets, and longitudinal study of whether sunset-less structures show escalating private-benefit extraction relative to sunset structures.',
    'If perpetual binding fails, the reading survives only for sunset structures and the arrangement''s legitimacy decays with each holding generation removed from the consent event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_binding_without_sunset, conceptual, 'Temporal scope of consent: whether a single disclosed purchase binds perpetual governance disparity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcl_consent_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dcl_consent_tr_t6, dual_class_legitimacy__disclosure_consent, theater_ratio, 6, 0.14).
narrative_ontology:measurement(dcl_consent_tr_t12, dual_class_legitimacy__disclosure_consent, theater_ratio, 12, 0.17).
narrative_ontology:measurement(dcl_consent_tr_t18, dual_class_legitimacy__disclosure_consent, theater_ratio, 18, 0.19).
narrative_ontology:measurement(dcl_consent_tr_t24, dual_class_legitimacy__disclosure_consent, theater_ratio, 24, 0.21).
narrative_ontology:measurement(dcl_consent_tr_t30, dual_class_legitimacy__disclosure_consent, theater_ratio, 30, 0.23).
narrative_ontology:measurement(dcl_consent_tr_t36, dual_class_legitimacy__disclosure_consent, theater_ratio, 36, 0.24).

% Extraction over time
narrative_ontology:measurement(dcl_consent_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dcl_consent_be_t6, dual_class_legitimacy__disclosure_consent, base_extractiveness, 6, 0.23).
narrative_ontology:measurement(dcl_consent_be_t12, dual_class_legitimacy__disclosure_consent, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(dcl_consent_be_t18, dual_class_legitimacy__disclosure_consent, base_extractiveness, 18, 0.26).
narrative_ontology:measurement(dcl_consent_be_t24, dual_class_legitimacy__disclosure_consent, base_extractiveness, 24, 0.28).
narrative_ontology:measurement(dcl_consent_be_t30, dual_class_legitimacy__disclosure_consent, base_extractiveness, 30, 0.29).
narrative_ontology:measurement(dcl_consent_be_t36, dual_class_legitimacy__disclosure_consent, base_extractiveness, 36, 0.3).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dual_class_legitimacy__disclosure_consent, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'is dual-class control legitimate?' decomposes under the epsilon-invariance principle into three readings of one kernel, each a separate file over the fixed referent of the standing dual-class arrangement. founder_stewardship authors near-zero epsilon (control serves all holders); this file authors low reading-indexed epsilon (consent plus pricing legitimizes); minority_extraction authors substantially higher epsilon (uncompensated control rents with named victims). The disclosure regime's adequacy record is cited as evidence by the stewardship defense and attacked by the extraction account, so this reading structurally influences both siblings' operating environments without resolving the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
