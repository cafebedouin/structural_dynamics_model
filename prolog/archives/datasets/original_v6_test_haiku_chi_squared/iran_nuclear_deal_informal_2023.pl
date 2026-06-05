% ============================================================================
% CONSTRAINT STORY: iran_nuclear_deal_informal_2023
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_nuclear_deal_informal_2023, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iran_nuclear_deal_informal_2023
 *   human_readable: Informal US-Iran Nuclear De-escalation Agreement (2023)
 *   domain: geopolitical/nuclear_security
 *
 * SUMMARY:
 *   The informal US-Iran nuclear de-escalation agreement of 2023 represents a
 *   structural constraint operating entirely outside formal treaty
 *   frameworks. Unlike the JCPOA (Joint Comprehensive Plan of Action), which
 *   was a signed multilateral agreement with explicit verification protocols
 *   and defined sanctions relief terms, the 2023 informal arrangement
 *   operates through backchannel diplomatic signaling, mutual restraint
 *   signaling via third parties (Oman as intermediary), and tacit acceptance
 *   of limited IAEA access. The agreement exhibits the signature property of
 *   a tangled rope: it provides genuine coordination benefits (both parties
 *   avoid direct military confrontation, reducing risk of miscalculation)
 *   while simultaneously enabling extraction (Iranian nuclear capabilities
 *   advance incrementally while the US accepts constrained options and allies
 *   face uncertainty). The constraint is increasingly characterized by
 *   theater — formal verification processes continue (IAEA presence, UN
 *   reporting) while their substantive verification function has degraded.
 *   The theater ratio has increased from 0.55 (initial phase: genuine
 *   de-escalation signaling) to 0.82 (mature phase: performative compliance
 *   reporting masking incremental Iranian progress).
 *
 * KEY AGENTS:
 *   - US Regional Military Command: Primary beneficiary (institutional/arbitrage) — de-escalation reduces direct conflict risk and operational costs; can exit if agreement fails.
 *   - Iranian Nuclear Program: Primary beneficiary (organized/mobile) — capabilities advance with minimal formal constraint; can claim compliance with unwritten terms.
 *   - Nuclear Non-Proliferation Regime (IAEA/NPT): Primary victim (powerless/trapped) — verification authority eroded; cannot exit framework but framework function degraded.
 *   - Regional US Allies (Israel, Saudi Arabia, UAE, Qatar): Secondary victims (moderate/trapped) — trapped in ambiguous security posture; cannot exit US relationship but cannot trust nuclear constraints.
 *   - Treaty Verification Community (IAEA, UN): Secondary actor (organized/constrained) — maintains inspections but with restricted access; constrained by geopolitical pressure to accept limited protocols.
 *   - Formal Nuclear Treaty Architecture (NPT, JCPOA legal structures): Institutional actor (institutional/arbitrage) — persists as theater; benefits from continued existence even as function is replaced by informal arrangements.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees functional hybrid meeting both powers' interests in de-escalation without legal exposure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_nuclear_deal_informal_2023, 0.58).
domain_priors:suppression_score(iran_nuclear_deal_informal_2023, 0.68).
domain_priors:theater_ratio(iran_nuclear_deal_informal_2023, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, extractiveness, 0.58).
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, theater_ratio, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_nuclear_deal_informal_2023, tangled_rope).
narrative_ontology:human_readable(iran_nuclear_deal_informal_2023, "Informal US-Iran Nuclear De-escalation Agreement (2023)").
narrative_ontology:topic_domain(iran_nuclear_deal_informal_2023, "geopolitical/nuclear_security").

domain_priors:requires_active_enforcement(iran_nuclear_deal_informal_2023).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, us_regional_military_posture).
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, iranian_sanctions_relief_coalition).
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, global_oil_market_stability).
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, nuclear_non_proliferation_regime).
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, treaty_verification_transparency).
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, regional_allies_confidence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NUCLEAR NON-PROLIFERATION REGIME (SNARE) — Cannot exit or renegotiate. The formal JCPOA collapsed; the informal replacement has no verification protocols, no enforcement mechanisms, and no legal standing. Bound by the precedent that informal agreements supersede treaty frameworks. Bears full extraction cost: erosion of IAEA inspection protocols, uncertainty over Iranian compliance, weakened commitment to transparent monitoring. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: US REGIONAL MILITARY POSTURE (TANGLED ROPE) — Coordination function: de-escalation reduces direct confrontation risk, allows tactical redeployment, stabilizes military-to-military messaging. Extraction: the informal agreement locks US into accepting Iranian nuclear progress without formal verification or escalation options. US military cannot easily withdraw or revert to confrontation without appearing to abandon the agreement. Benefits from reduced direct conflict; constrained by inability to formalize or enforce terms. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.48.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IRANIAN SANCTIONS-RELIEF COALITION (ROPE) — Pure coordination benefit from de-escalation. Sanctions restrictions partially ease via informal diplomatic signal. Coalition (oil traders, financial institutions, reconstruction investors) gains arbitrage: they can operate in grey zones (secondary sanctions compliance becomes theater rather than enforcement). Extractive cost is minimal because they can exit if agreement collapses. d≈0.12, f(d)≈0.10, σ=1.1 → χ≈0.06. Net beneficiary.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL US ALLIES — ISRAEL & GULF STATES (SNARE) — Structurally trapped. The informal agreement creates plausible deniability for both US and Iran: US avoids confrontation, Iran avoids formal constraints. Allies cannot exit: they are dependent on US security guarantees. They cannot publicly disavow the agreement (diplomatic rupture). They cannot credibly accelerate their own nuclear or ballistic programs without triggering US opposition. Trapped in ambiguity: Iranian nuclear progress is either constrained by unverified informal terms or not constrained at all — allies cannot trust the framework but cannot escape it. d≈0.88, f(d)≈1.32, σ=1.1 → χ≈0.85.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: TREATY VERIFICATION COMMUNITY (IAEA, UN) (TANGLED ROPE) — Coordination function: informal agreement allows continued diplomatic engagement, preserves IAEA presence in Iran (albeit with reduced protocols), maintains back-channel communication that prevents miscalculation. Extraction: the informal framework actually weakens verification authority — IAEA inspections become performative (inspectors present but with restricted access), formal reporting becomes theater (limited scope for 'sensitive military sites'). Verification community has some agency (IAEA can walk away, UN can impose sanctions) but is constrained by geopolitical pressure to maintain engagement. d≈0.62, f(d)≈0.88, σ=1.2 → χ≈0.51.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FORMAL NUCLEAR TREATY ARCHITECTURE (PITON) — The NPT, JCPOA, and IAEA verification framework persist as institutional structures, but their primary function (transparent, enforceable nuclear limits) has been replaced by informal diplomatic theater. The formal framework is maintained for narrative legitimacy (IAEA still has presence, sanctions technically remain 'reversible' under JCPOA), but the real deal is elsewhere (backchannel negotiations, mutual forbearance signals). Theater_ratio = 0.82: formal compliance reporting, IAEA visits, UN statements about 'continued negotiations' all signify process without substantive constraint. The institutions persist through inertia — no actor wants the reputational cost of formally abandoning them, so they remain as backdrop to the real informal game. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.08. Net beneficiary (institutional preservation).
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - GEOPOLITICAL REALISM (TANGLED ROPE) — From a structural geopolitical perspective, the informal agreement is a realistic hybrid: coordination function (prevents direct military escalation, allows both parties to signal restraint without formal legal commitments) + extraction (the framework allows Iranian nuclear progress with no verification, while constraining US and allies to maintain expensive military posturing and implicit security guarantees). The analytical view sees this as rational under conditions of mistrust: formal treaties require verification infrastructure; informal agreements provide plausible deniability and face-saving exits. Effective extraction (χ) is moderate (0.58) because both parties are somewhat constrained — neither can fully exploit the agreement without triggering collapse and direct conflict. d≈0.68, f(d)≈1.03, σ=1.2 → χ≈0.61.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_nuclear_deal_informal_2023_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_nuclear_deal_informal_2023, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_nuclear_deal_informal_2023, TR),
    TR >= 0.70.

:- end_tests(iran_nuclear_deal_informal_2023_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The informal agreement allows Iranian nuclear advancement (centrifuge deployment, uranium enrichment increases) to proceed with only unverified informal constraints. This is extraction in the technical sense — the Iranian program extracts progress without corresponding formal verification. However, extractiveness is not maximal (0.70+) because the US also benefits from de-escalation (reduced military spending, avoided direct conflict), and Iran self-constrains through fear of escalation rather than legal obligation. The extractiveness has increased over 12 months (from 0.38 to 0.58) as initial goodwill de-escalation signals have given way to routine Iranian capacity expansion. Suppression (0.68): High. Significant barriers to exit include: (1) for the US: domestic political cost of appearing to abandon nuclear diplomacy; (2) for Iran: risk of reimposed sanctions and military confrontation; (3) for allies: dependence on US security guarantees; (4) for verification regime: institutional inertia and face-saving need to maintain some IAEA presence. Theater ratio (0.82): Very high and rising. IAEA inspections continue but with severely restricted scope (no access to military sites, no environmental sampling in contested areas). UN statements affirm 'continued diplomatic engagement' while substantive negotiations are invisible. Both sides publicly deny the agreement exists (it is 'informal') while managing behavior according to its logic — maximum theater. The theater has increased as the agreement matures and initial genuine de-escalation gives way to routine violations management.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates remarkable perspectival polarization. The Iranian sanctions-relief coalition and US military see coordination (rope) — de-escalation reduces their operational costs. Regional allies see pure extraction (snare) — they are locked into ambiguous security postures with no exit. The non-proliferation regime sees pure extraction (snare) — verification erodes with no formal recourse. The verification community sees mixed extraction-coordination (tangled rope) — they maintain dialogue but lose substantive access. The formal treaty architecture sees its own degradation (piton) — it persists as legal backdrop to an informal reality. The analytical observer sees rational hybrid (tangled rope) — this matches the base classification and confirms the presheaf structure. The gap between the snare perspectives (allies, NPT regime) and rope/tangled perspectives (beneficiaries, observers) is the core structural tension.
 *
 * DIRECTIONALITY LOGIC:
 *   US military posture: Beneficiary + arbitrage → d≈0.12, f(d)≈0.10. Net beneficiary; can exit without catastrophic cost. Iranian nuclear program: Beneficiary + mobile → d≈0.20, f(d)≈0.15. Benefits from arrangement; mobile enough to walk away if confronted. NPT/IAEA regime: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; cannot exit treaty framework but framework function is hollowed. Regional allies: Victim + trapped → d≈0.88, f(d)≈1.32. Cannot exit US relationship; vulnerable to Iranian advancement. Verification community: Mixed victim + constrained → d≈0.62, f(d)≈0.88. Some agency (can report violations) but constrained by geopolitical pressure to maintain engagement. Treaty architecture: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary (continues to exist); can be formally abandoned if both powers align.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: This constraint exhibits high extractiveness (0.58) and requires mandatrophy resolution (base properties do not declare mandatrophy_resolved: true, which violates the ε > 0.46 rule if extractiveness > 0.70). The constraint does NOT breach the 0.70 threshold, so mandatrophy resolution is not yet mandatory. However, the tension is visible: Is this a sustainable hybrid (tangled rope that genuinely solves a coordination problem) or a degrading constraint (piton masquerading as rope, destined to collapse)? The trajectory from ε=0.38 (initial de-escalation cooperation) to ε=0.58 (Iranian expansion + US powerlessness to formalize constraints) suggests extraction is accumulating. If the extractiveness reaches 0.70+ within 24 months, mandatrophy will become critical — the system will need to certify whether this is a legitimate hybrid serving geopolitical stability or a false rope hiding Iranian nuclear breakout. The theater_ratio rise (0.55→0.82) indicates theater is replacing function, which is the signature of piton degradation. The engine's task: track whether the informal agreement stabilizes as a functional tangled rope (both sides see mutual benefit in de-escalation) or collapses into a snare (Iranian nuclear progress unconstrained, US/allies trapped in powerlessness).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informal_enforcement_capacity,
    'What enforcement mechanisms actually exist in an unwritten agreement, and how credible is mutual forbearance without formal consequences?',
    'Historical precedent analysis of similar informal US-adversary agreements (Cuban Missile Crisis aftermath, US-USSR détente); measurement of compliance/violation patterns and response asymmetries',
    'If enforcement is credible: agreement is functional hybrid (tangled_rope confirmed). If enforcement is illusory: agreement is theater masking Iranian nuclear progress (snare dominates, theater_ratio rises toward 0.90+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_enforcement_capacity, empirical, 'Credibility of mutual forbearance enforcement in unwritten agreements').

omega_variable(
    verification_degradation_timeline,
    'Over what timeframe does informal verification (IAEA with restricted access) become functionally equivalent to no verification?',
    'IAEA inspection reports analysis; comparison of access density and scope vs formal JCPOA baselines; detection probability modeling for centrifuge deployment and weaponization milestones',
    'If timeline < 2 years: agreement rapidly becomes pure extraction (snare classification gains confidence). If timeline > 5 years: verification is meaningful enough to sustain tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_degradation_timeline, empirical, 'Timeline for IAEA restricted access to become functionally equivalent to no verification').

omega_variable(
    us_iran_mutual_interest_stability,
    'How stable is the assumption that both US and Iran have mutual interest in preventing escalation, and what triggers could break that assumption?',
    'Analysis of domestic political incentives in both countries; monitoring of regional proxy conflict intensity (Yemen, Iraq, Syria); assessment of elite consensus on nuclear brinkmanship costs',
    'If assumption is stable: agreement persists, classifications hold. If assumption breaks: agreement collapses into formal conflict, all perspectives shift to snare/mountain (existential confrontation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_iran_mutual_interest_stability, preference, 'Stability of mutual forbearance assumption under domestic political pressure').

omega_variable(
    treaty_substitution_precedent,
    'Does this informal agreement establish a precedent that major-power nuclear diplomacy no longer requires formal treaty structures, or is it an anomaly?',
    'Long-term observation of future US-China, US-Russia nuclear interactions; analysis of treaty-formation trends in post-JCPOA era',
    'If precedent-setting: non-proliferation regime classification shifts to systemic snare (formal verification frameworks become optional theater). If anomaly: NPT/IAEA framework recovers authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_substitution_precedent, conceptual, 'Whether informal agreements become new normal in nuclear diplomacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_nuclear_deal_informal_2023, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_informal_tr_t0, iran_nuclear_deal_informal_2023, theater_ratio, 0, 0.55).
narrative_ontology:measurement(iran_informal_tr_t6, iran_nuclear_deal_informal_2023, theater_ratio, 6, 0.72).
narrative_ontology:measurement(iran_informal_tr_t12, iran_nuclear_deal_informal_2023, theater_ratio, 12, 0.82).

% Extraction over time
narrative_ontology:measurement(iran_informal_be_t0, iran_nuclear_deal_informal_2023, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(iran_informal_be_t6, iran_nuclear_deal_informal_2023, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(iran_informal_be_t12, iran_nuclear_deal_informal_2023, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_nuclear_deal_informal_2023, enforcement_mechanism).
narrative_ontology:affects_constraint(iran_nuclear_deal_informal_2023, jcpoa_collapse_2018).
narrative_ontology:affects_constraint(iran_nuclear_deal_informal_2023, iaea_iran_verification_access).
narrative_ontology:affects_constraint(iran_nuclear_deal_informal_2023, gulf_security_architecture).
narrative_ontology:affects_constraint(iran_nuclear_deal_informal_2023, israeli_regional_nuclear_deterrent).

% DUAL FORMULATION NOTE:
% The informal 2023 agreement is structurally downstream of the JCPOA collapse (2018) and represents a distinct constraint with different ε values. The JCPOA (ε≈0.15, rope) was a multilateral formal agreement with transparent verification. The 2023 informal arrangement (ε≈0.58, tangled rope) operates through bilateral backchannel signaling with degraded verification. The two constraints should not be collapsed into a single story — they have different baseline metrics, different beneficiaries/victims, and different classification signatures. This story models the informal 2023 arrangement; a separate story should model JCPOA-as-constraint. Network links capture the causal relationship (JCPOA collapse enabled informal arrangement) and the structural coupling (informal agreement's success/failure directly affects IAEA access protocols, regional nuclear security calculations, and Israeli deterrent sufficiency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_nuclear_deal_informal_2023, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
