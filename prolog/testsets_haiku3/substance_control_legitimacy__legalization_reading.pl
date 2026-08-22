% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Legalization-with-Third-Party-Harm-Limits Autonomy Constraint
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   Under the legalization reading of the substance-control-legitimacy
 *   kernel, competent adults have an autonomy right to use substances, and
 *   state authority is limited to preventing third-party harm (impaired
 *   driving, secondhand exposure, sales to minors, occupational hazards).
 *   This reading removes users from the victim set—they are no longer targets
 *   of criminalization—and shifts the constraint's focus to harm boundaries.
 *   The constraint becomes tangled: it coordinates a legal market (users gain
 *   access, industries gain market, state gains tax revenue), AND it extracts
 *   from communities exposed to secondhand harm and from workers in the
 *   substance industries. The reading contrasts sharply with the prohibition
 *   reading (state authority derives from moral duty to prevent self-harm)
 *   and coexists with the harm-reduction reading (which accepts
 *   decriminalization but emphasizes treatment access as a structural right,
 *   not just a user responsibility). The legalization reading's claim/metric
 *   independence is deliberate: it is claimed as a coordination framework but
 *   authored with substantial extractiveness (0.68) because corporate capture
 *   of the legal market and under-enforcement of harm boundaries mean the
 *   coordination story masks asymmetric extraction from exposed communities.
 *
 * KEY AGENTS:
 *   - competent_adult_users (powerless, mobile) — gain decriminalization; the autonomy framing moves them from victim to beneficiary
 *   - legal_substance_industries (institutional, arbitrage) — set regulatory agenda, extract profits, engage in regulatory capture to weaken harm boundaries
 *   - state_harm_prevention_authority (institutional, constrained) — enforces age limits, public-use bans, impairment prosecution; the autonomy/harm boundary is institutionally contested
 *   - communities_exposed_to_secondhand_harm (powerless, trapped) — bear costs not captured by the autonomy frame (secondhand smoke, environmental contamination)
 *   - impaired_driver_victims (powerless, trapped) — absorb third-party harm; enforcement is weaker than the harm boundary suggests
 *   - workers_in_regulated_markets (moderate, constrained) — gain legal employment but bear occupational exposure costs
 *   - harm_reduction_advocates (organized, constrained) — excluded from the legalization reading; argue that structural vulnerability (poverty, trauma) undermines the 'competent adult' assumption
 *   - prohibition_advocates (organized, mobile) — excluded; hold that autonomy does not override the duty to prevent self-harm
 *   - regulation_arbitrage_networks (moderate, mobile) — exploit cross-jurisdiction variation; profit spread between lax and strict regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.68).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.42).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Legalization-with-Third-Party-Harm-Limits Autonomy Constraint").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '615a2fda-5846-4b97-a01f-7e96557ff20d').
narrative_ontology:cs_kernel_codification('615a2fda-5846-4b97-a01f-7e96557ff20d', distributed).
narrative_ontology:cs_authority_grounding('615a2fda-5846-4b97-a01f-7e96557ff20d', lineage).
narrative_ontology:cs_interpretation_layer_present('615a2fda-5846-4b97-a01f-7e96557ff20d').
narrative_ontology:cs_reading_relation('615a2fda-5846-4b97-a01f-7e96557ff20d', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('615a2fda-5846-4b97-a01f-7e96557ff20d', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('615a2fda-5846-4b97-a01f-7e96557ff20d', foundational, competent_adult_autonomy_primary).
narrative_ontology:cs_axiom_status(competent_adult_autonomy_primary, holdable).
narrative_ontology:cs_axiom_grounding('615a2fda-5846-4b97-a01f-7e96557ff20d', competent_adult_autonomy_primary, deontological).
narrative_ontology:cs_axiom('615a2fda-5846-4b97-a01f-7e96557ff20d', foundational, third_party_harm_legitimizes_state_intervention).
narrative_ontology:cs_axiom_status(third_party_harm_legitimizes_state_intervention, holdable).
narrative_ontology:cs_axiom_grounding('615a2fda-5846-4b97-a01f-7e96557ff20d', third_party_harm_legitimizes_state_intervention, deontological).
narrative_ontology:cs_created_at('615a2fda-5846-4b97-a01f-7e96557ff20d', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_substance_industries).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, users_freed_from_criminalization).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, communities_exposed_to_secondhand_harm).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, impaired_driver_victims).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, workers_in_regulated_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adult_users).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, workers_in_regulated_markets).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, regulation_arbitrage_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to substances without criminalization or incarceration risk. Can use substances in private without state surveillance or prosecution. Exit from the constraint means relocation to prohibitionist jurisdictions or black-market engagement.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adult_users, beneficiary,
    powerless, biographical, mobile, national).

% Extract substantial profits from licensed production, distribution, and retail of previously prohibited substances. Set marketing norms, lobby for regulatory capture, and define what 'harm prevention' means in practice. Their exit option is geographic arbitrage—moving operations to more permissive jurisdictions.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_substance_industries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, legal_substance_industries, beneficiary).

% Enforces limits on third-party harm through traffic safety laws, public-use restrictions, occupational safety standards, and age-of-purchase controls. Must distinguish between consensual adult use (outside state authority) and harm to non-consenting parties (within authority). Constrained exit: cannot unilaterally change the autonomy premise without renegotiating the kernel.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_harm_prevention_authority, agenda_setter,
    institutional, generational, constrained, national).

% Absorb secondhand smoke, aerosol, and environmental exposure from legalized substances in shared spaces (housing, workplaces, transit, public areas). Do not consent to the exposure; cannot easily exit residential/work location. Bear health costs without direct choice.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, communities_exposed_to_secondhand_harm, payer,
    powerless, biographical, trapped, local).

% Suffer death and injury from substance-impaired drivers; family members bear grief and long-term care costs. No choice in exposure; enforcement gap means harm-prevention boundary is not fully enforced. Exit is impossible without abandoning roads.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, impaired_driver_victims, payer,
    powerless, immediate, trapped, regional).

% Gain legal employment in legalized substance industries (cultivation, production, retail), replacing black-market work with regulated wages and labor standards. Simultaneously bear occupational exposure to the substances they handle (cultivation dust, opioid handling, alcohol fumes). Constrained exit: few alternative sectors absorb displaced workers at equivalent pay.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, workers_in_regulated_markets, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, workers_in_regulated_markets, beneficiary).

% Argue that legalization without treatment infrastructure concentrates harms on vulnerable users (low-income, trauma-survivors). Excluded from authority-setting: the legalization reading's harm boundary assumes individual competence and omits structural vulnerability. Their objection is not heard in the kernel's core formulation.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, harm_reduction_advocates, excluded,
    organized, biographical, constrained, national).

% Hold that substance use is inherently harmful and the state has a duty to prevent self-harm. Excluded from the legalization framework: the autonomy premise forecloses their core claim. They advocate for the prohibition reading instead (separate constraint).
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibition_advocates, excluded,
    organized, biographical, mobile, national).

% Exploit cross-jurisdiction arbitrage: move capital and supply chains to jurisdictions with looser regulations, then export to higher-regulation markets. Gain profit spread between lax and strict enforcement regimes. Mobile exit: relocate to next laxer jurisdiction when enforcement tightens.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, regulation_arbitrage_networks, beneficiary,
    moderate, biographical, mobile, global).

% Enforces age-of-purchase restrictions, public-use bans, occupational safety standards, and impaired-driving prosecution. Must distinguish between private consensual use (not their remit) and third-party harm (their mandate). The boundary is contested, creating enforcement ambiguity.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, enforcement_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Provides evidence on substance-specific harms, impairment thresholds, secondhand exposure risk, and addiction pathophysiology. Analytical position: evidence is the reference frame. Their research informs how the harm boundary is drawn, but their seat does not set the autonomy vs. harm priority.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, scientific_and_medical_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, legal_substance_industries).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a legal, regulated market for substance access, replacing black-market supply with standardized production, age-verification, and quality control. Solves the problem of uncontrolled drug purity and trafficking violence by moving supply into a taxed, inspected legal framework. Coordinates harm-prevention boundaries: age restrictions, public-use bans, impairment prosecution, occupational safety.
% TRANSFER_FUNCTION: Moves tax revenue from substance sales to the state (education, treatment, enforcement). Moves profits from black-market suppliers to legal industries and the state. Moves regulatory compliance costs onto producers and retailers. Moves enforcement burden onto public agencies (police, courts, medical system). Moves secondhand exposure costs onto non-consenting communities.
% ABSENT_VOICES: Harm-reduction advocates are excluded: they would argue that legalization without treatment infrastructure concentrates harms on vulnerable users and that 'competent adults' framing erases structural barriers to competence. Prohibition advocates are excluded: they would argue that autonomy does not justify self-harm and that the state has a duty to prevent it. Vulnerable users (heavily stigmatized as lacking competence) are nominally beneficiaries but are largely absent from authority-setting conversations.
% DISAPPEARANCE_RATIONALE: If the legalization-with-harm-limits constraint disappeared overnight, substance access would revert to black markets, supply chains would shift underground, state tax revenue and regulatory oversight would vanish, and enforcement resources would reallocate to drug prohibition. The legal substance industries would lose their license and market. Decriminalization gains for users would reverse to criminalization risk. Secondhand-harm regulations would lose enforceability. The legal market infrastructure (licensing, testing, distribution) would collapse.
% FOUNDING_PROBLEM: Criminalization of substance use created mass incarceration, racial disparities in prosecution, black-market violence, supply contamination, and uncontrolled potency. The legalization reading was built to solve this by separating adult autonomy (decriminalized) from third-party harm (still regulated), replacing criminal enforcement with market regulation and harm-specific law.
% FOUNDING_PROBLEM_CORROBORATION: Users and criminal-justice reformers attest criminalization harms are real and substantial (incarceration, collateral consequences, racial targeting). Legal industries attest they solve supply safety and eliminate trafficking violence. However, harm-reduction research and public-health data show that decriminalization alone, without treatment access, does not prevent escalating use or overdose death: the founding problem is partially solved and partially replaced with new problems. Prohibition advocates attest that decriminalization creates new founding problems (youth accessibility, normalization, escalation). External corroboration: incarceration data show dramatic reductions where legalization occurs; overdose mortality trends are mixed (declining in some legalized jurisdictions, rising in others depending on treatment access); comparative outcomes across Portugal (decriminalization + treatment), Canada (legalization + weak treatment infrastructure), and Netherlands (regulated market + strong public health) show the founding problem was partially solved, and success depends on what complementary commitments accompany legalization.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness runs 0.48→0.69 over the interval, peaking around year 25 as industry consolidation and regulatory capture mature. Theater ratio rises 0.25→0.39 as harm-prevention rhetoric grows while enforcement remains patchy (impaired-driving prosecutions lag exposure risk, secondhand regulations are weakly enforced). Suppression is moderate (0.35→0.42) because the legalization reading itself rejects criminalization, so enforcement burden shifts from jailing users to regulating harms—enforcement is lighter but also more selective (targeting poor communities for impaired driving while wealthy areas face weaker policing). The measurement grid is one shared time axis: every metric is authored at every examined point (0,5,10,15,20,25,30,40) so lifecycle drift detection has aligned data. Extractiveness plateaus around 0.68-0.69 after year 25 as regulatory capture stabilizes and the harm-boundary gaps become institutionalized (they stop expanding). Theater ratio stabilizes as 'harm prevention' messaging becomes routine and the gap between rhetoric and enforcement becomes normalized.
 *
 * PERSPECTIVAL GAP:
 *   Users and legal industries sit on opposite sides of the autonomy gain: users see decriminalization as liberation; industries see it as market access and extraction opportunity. From the payer seats (secondhand-harm communities, impaired-driver victims), the legalization reading looks like a naked transfer: users gain autonomy, industries gain profit, but exposure harms persist and enforcement gaps widen. The state_harm_prevention_authority seat experiences institutional tension: the autonomy premise constrains their authority (they cannot prosecute private use) but the harm boundary remains contested (they must prosecute third-party harms, but what counts as 'harm' and where enforcement should focus is a political battlefield). The engine should compute this as sharp divergence: user and industry seats compute as rope-leaning beneficiaries; payer seats (secondhand, impaired-driver victims) compute as snare-targeted; the authority seat computes as tangled-rope-trapped (enforcing a boundary it does not fully control). The harm-reduction and prohibition excluded seats would compute as contenders if included.
 *
 * DIRECTIONALITY LOGIC:
 *   Users: role=beneficiary, powerless, mobile exit. Directionality d~0.15 (beneficiary end): they escape criminalization, have mobile exit (relocation to legalization jurisdictions), and bear no direct extraction from the legal market. Legal_substance_industries: role=agenda_setter + beneficiary, institutional, arbitrage exit. Directionality d~0.25 (beneficiary end): they set regulatory terms, capture rents, and can arbitrage to looser jurisdictions. Communities_exposed_to_secondhand_harm: role=payer, powerless, trapped exit. Directionality d~0.85 (full target end): they bear costs without consent, cannot exit (residential/work location lock), and receive no direct benefit. Impaired_driver_victims: role=payer, powerless, trapped. Directionality d~0.90 (full target end): they suffer unexpected harm, no choice in exposure, no exit. Workers_in_regulated_markets: role=payer + beneficiary, moderate power, constrained exit. Directionality d~0.60 (symmetric-to-target): they gain employment but bear occupational exposure, cannot easily exit (few alternative sectors). The divergence between beneficiary and payer seats is structural: the autonomy frame produces winners and losers at different power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   The legalization reading claims coordination (legal market, tax revenue, harm boundaries) and satisfies the tangled_rope gate (beneficiaries + victims + active enforcement). But mandatrophy risk is present: the founding_problem (criminalization harms) was PARTIALLY solved, and legalization in the absence of treatment infrastructure has created a new founding problem (escalating use among vulnerable populations, overdose mortality trends, secondhand exposure normalization). The constraint persists because (1) users have exited the victim set and now benefit from decriminalization, and (2) legal industries have strong incentive to maintain it. But the harm-boundary function has atrophied: secondhand regulations exist on paper but are weakly enforced; impaired-driving prosecution rates have not kept pace with exposure; treatment access remains limited. Theater_ratio rising to 0.38-0.39 indicates performative harm-prevention rhetoric ('we regulate for safety') masking under-enforcement. The constraint is NOT yet piton (it still extracts and coordinates), but it is drifting toward piton-like properties: the coordination story is increasingly cover, the real function is extractive maintenance. The disappearance_verdict='world_rearranges' and founding_problem_status='contested' together flag this: if the constraint vanished, arrangements WOULD rearrange (legal industries would lose license, users would face re-criminalization risk, state would lose tax revenue), BUT there is strong disagreement about whether the founding problem is solved or has been replaced. That contestation is exactly the mandatrophy signal: the legitimacy claim (autonomy + harm prevention) is unraveling; the constraint persists anyway because beneficiary interests are strong enough.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_boundary_ambiguity,
    'What makes an adult ''competent'' to use substances under the legalization reading? Is competence a stable individual property, or is it undermined by poverty, trauma, addiction progression, and structural vulnerability?',
    'Empirical and conceptual: addiction neuroscience shows that repeated use alters decision-making capacity; social epidemiology shows that poverty and trauma reduce exit options and compress decision-making context. The legalization reading assumes competence is binary and stable; evidence suggests competence is contextual and degradable.',
    'If competence is contextual, the autonomy premise does not apply universally to all users, and the state''s authority to intervene expands beyond third-party harm (includes structural vulnerability). This would collapse the legalization reading toward the harm-reduction reading or foreclose it entirely. The victim set would expand to include structurally vulnerable users.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_boundary_ambiguity, empirical, 'Whether autonomy premise applies universally or is undermined by structural vulnerability.').

omega_variable(
    harm_boundary_enforcement_gap,
    'Is the gap between harm-prevention rhetoric and actual enforcement a feature or a bug of the legalization reading? Is weak secondhand-harm enforcement acceptable as a cost of autonomy, or does it reveal the harm boundary is illusory?',
    'Comparative jurisdictions with strong vs. weak secondhand enforcement show that enforcement gaps are policy choices, not structural necessities. The question is whether the legalization reading''s legitimacy depends on robust enforcement or tolerates enforcement gaps as the price of decriminalization.',
    'If the legalization reading requires robust harm enforcement to be coherent, under-enforcement indicates constraint failure (mandatrophy signal). If the reading tolerates gaps, it admits that some third-party harm is acceptable because autonomy and regulatory capture both resist full enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_boundary_enforcement_gap, conceptual, 'Whether the harm boundary is enforced or merely theatrical.').

omega_variable(
    regulatory_capture_versus_autonomy,
    'Does the legalization reading''s decriminalization premise inevitably lead to regulatory capture by legal industries, or can the harm boundary be maintained against industry lobbying?',
    'Historical pattern: every legalization regime has experienced industry capture (alcohol, tobacco, cannabis, pharmaceuticals). The pattern suggests capture is structural, not accidental. But some regimes (e.g., alcohol in Nordic countries) have resisted capture through strong state monopolies or aggressive taxation.',
    'If capture is inevitable, the legalization reading devolves into a vehicle for industry extraction masquerading as autonomy. If capture is avoidable, legalization requires parallel commitments to maintain the harm boundary against industry pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_versus_autonomy, empirical, 'Whether regulatory capture is structural or contingent.').

omega_variable(
    committer_frame_autonomy_versus_harm,
    'The legalization reading''s core axiom is that competent-adult autonomy is primary and harm-prevention is secondary. But the sibling harm-reduction reading asserts that preventing harm to vulnerable users is primary and autonomy is constrained by that obligation. Which reading''s axiom is morally foundational?',
    'This is a preference omega: the choice between autonomy-first and harm-prevention-first is a value choice, not empirically resolvable. However, the choice determines which vulnerability (vulnerable users exploited by the autonomy frame) is visible. Neither reading can logically foreclose the other without claiming that values are facts.',
    'If autonomy-first is foundational, vulnerable users can be left without treatment access (''your choice to use is your responsibility''). If harm-prevention-first is foundational, the legalization reading collapses toward harm-reduction. The axiom choice determines who appears in the victim set and who is counted as a beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_autonomy_versus_harm, preference, 'Whether autonomy or harm-prevention is the foundational value under the legalization reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__legalization_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__legalization_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__legalization_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__legalization_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__legalization_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__legalization_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__legalization_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__legalization_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__legalization_reading, base_extractiveness, 30, 0.69).
narrative_ontology:measurement(subs_be_t40, substance_control_legitimacy__legalization_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__legalization_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__legalization_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__legalization_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__legalization_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__legalization_reading, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__legalization_reading, 0.2).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% The substance_control_legitimacy kernel instantiates three distinct readings: (1) legalization_reading (this constraint) centers competent-adult autonomy and limits state authority to third-party harms; (2) harm_reduction_reading shares decriminalization but prioritizes treatment access as a structural right; (3) prohibition_reading asserts inherent harmfulness and state duty to prevent self-harm through criminalization. Each reading has distinct ε, distinct beneficiary/victim structures, and distinct axiomatic foundations. The three constraints form a constraint family linked by their shared kernel: all three interpret the same institutional commitment (how substance control is justified) but diverge on core premises. The legalization reading forecloses prohibition but coexists_with harm_reduction. Each reading is a separate constraint file; this file instantiates only the legalization_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
