% ============================================================================
% CONSTRAINT STORY: iran_nuclear_deal_informal_2023
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: geopolitical/nuclear_diplomacy
 *
 * SUMMARY:
 *   The informal US-Iran nuclear de-escalation agreement of 2023 represents a
 *   constrained diplomatic arrangement where both principal parties (US and
 *   Iran) benefit from reduced tensions and sanctions relief pathways, but
 *   the informality creates structural asymmetries: excluded regional allies
 *   are trapped, the global nonproliferation regime loses institutional
 *   control, and verification depends on back-channel signaling rather than
 *   transparent inspection. The constraint exhibits high suppression (68%)
 *   because alternatives are actively foreclosed — formal renegotiation is
 *   politically impossible in both capitals, regional allies cannot veto, and
 *   the nonproliferation regime has no enforcement lever. Theater ratio (65%)
 *   reflects that public diplomatic engagement and IAEA inspections continue
 *   as performative apparatus while real coordination happens through
 *   informal channels. The agreement is neither pure extraction (Snare) nor
 *   pure coordination (Rope) — it is a hybrid where the US and Iranian
 *   administrations coordinate mutual restraint (coordination function) while
 *   extracting terms that exclude other stakeholders and bypass institutional
 *   verification (extraction function). This makes it a diagnostic
 *   tangled_rope: it solves the immediate US-Iran crisis (coordination
 *   benefit) while accumulating verification deficits and excluding affected
 *   parties (asymmetric extraction).
 *
 * KEY AGENTS:
 *   - United States Administration: Primary beneficiary (organized/constrained) — gains de-escalation narrative and sanctions relief leverage without formal treaty constraints
 *   - Iran: Primary beneficiary (powerful/constrained) — gains sanctions relief pathway and legitimacy; constrained by verification burdens and internal faction management
 *   - Regional Allies (Israel, Gulf States): Primary victims (powerless/trapped) — excluded from negotiation, dependent on US security umbrella, face binding de-escalation they did not consent to
 *   - Global Nonproliferation Regime: Structural victim (powerless/trapped) — IAEA and NPT frameworks bypassed; verification integrity compromised by great-power politics
 *   - Formal Treaty Infrastructure (JCPOA/UN): Institutional actor (institutional/arbitrage) — formal apparatus persists but functional role has atrophied; sees verification displaced by informal channels
 *   - Russian and Chinese Strategic Interests: Tertiary beneficiaries (institutional/arbitrage) — benefit from reduced US-Iran conflict without being pulled into crisis management
 *   - Analytical Observer: External perspective (analytical/analytical) — sees the arrangement as temporary scaffold with implicit sunset toward either formalization or breakdown
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_nuclear_deal_informal_2023, 0.52).
domain_priors:suppression_score(iran_nuclear_deal_informal_2023, 0.68).
domain_priors:theater_ratio(iran_nuclear_deal_informal_2023, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, extractiveness, 0.52).
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_nuclear_deal_informal_2023, tangled_rope).
narrative_ontology:human_readable(iran_nuclear_deal_informal_2023, "Informal US-Iran Nuclear De-escalation Agreement (2023)").
narrative_ontology:topic_domain(iran_nuclear_deal_informal_2023, "geopolitical/nuclear_diplomacy").

domain_priors:requires_active_enforcement(iran_nuclear_deal_informal_2023).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, us_regional_military_interests).
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, iranian_sanctions_relief_constituency).
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, global_nonproliferation_regime).
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, treaty_verification_integrity).
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, regional_allies_excluded).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL ALLIES (SNARE) — Israel and Gulf Arab states face binding commitment to accept a de-escalation framework they did not negotiate and cannot modify. Trapped by geographic proximity and dependence on US security umbrella. The informal agreement constrains their options without representation. Maximum extraction: high suppression (cannot veto), high asymmetry (excluded from negotiations), no arbitrage path.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NONPROLIFERATION REGIME (SNARE) — The NPT framework and IAEA verification architecture cannot exit an informal arrangement that bypasses their institutional mechanisms. The regime is trapped by great-power politics. Informal deals undermine the institutional integrity required for verification. Cannot organize or enforce. Trapped and victimized.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: IRAN (TANGLED ROPE) — Powerful enough to demand recognition and negotiate terms, but constrained by sanctions vulnerability and internal factional pressures. Benefits from de-escalation (sanctions relief pathway, legitimacy) but bears verification burdens and constrained by ability to credibly signal compliance without formal transparency. Mixed experience: coordination mechanism (mutual restraint) plus extraction (asymmetric disclosure requirements).
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: US ADMINISTRATION (TANGLED ROPE) — Organized power with primary beneficiary status. Gains diplomatic victory and de-escalation narrative. But constrained by domestic political opposition, Congressional skepticism, and need for plausible deniability if informal arrangement breaks down. Active enforcement required (signaling compliance, restraining hardliners) but cannot formalize without Senate approval. Extraction runs through: informal status shields US from treaty constraints while appearing cooperative.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RUSSIAN/CHINESE STRATEGIC INTERESTS (ROPE) — Benefit from US-Iran de-escalation as coordination mechanism reducing their own Middle East exposure and conflict risk. Can arbitrage between formal/informal arrangements. See the constraint as pure coordination: US and Iran managing mutual threat reduces great-power intervention scenarios. Low extraction experienced because both benefit from reduced conflict.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FORMAL TREATY INFRASTRUCTURE (PITON) — The JCPOA and UN inspection apparatus persist as institutional structures, but their functional verification role has been displaced by an informal arrangement they cannot access. The formal apparatus continues performing verification theater (IAEA inspections continue, reporting continues) but the real coordination happens off-stage. Piton because function has atrophied (verification now depends on back-channel signaling) while institutional forms persist through inertia.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SCAFFOLD) — From an analytical/generational perspective, the informal arrangement functions as a temporary coordination mechanism with an implicit sunset: it exists because formal negotiation is politically impossible in 2023, but creates pressure toward either (a) formalization into a new treaty, or (b) breakdown into crisis when political conditions shift. Theater ratio high (0.65) because performative diplomatic engagement masks underlying verification gaps. Sunset logic: incompleteness of informal arrangement drives pressure toward resolution.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
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
 *   Extractiveness (0.52): Moderate-high. The US administration and Iran each extract benefits (de-escalation, sanctions relief, legitimacy) from the coordination function, but this is legitimate mutual benefit rather than pure extraction. However, the informality enables asymmetric extraction: US avoids Senate ratification burdens while appearing cooperative; Iran must accept verification asymmetries and informal compliance burdens. The growth from 0.44 to 0.52 reflects expanding compliance verification requirements over the interval. Suppression (0.68): High. Significant structural suppression: regional allies have zero input on the arrangement, the formal nonproliferation regime cannot participate, and alternative pathways (Congressional renegotiation, multilateral approaches) are actively foreclosed by political constraints. Senate opposition makes formalization impossible in the US; Iranian hardline factions constrain Iran's flexibility. Theater ratio (0.65): Moderate-high. Public diplomatic statements, IAEA inspections, and UN reporting continue as performative apparatus while substantive verification happens through back-channel intelligence sharing. The performative content is high because the public claim (de-escalation through transparency) masks the reality (informal arrangement relies on trust and intelligence sharing, not institutional verification). Theater has increased from 0.50 to 0.65 as the gap widens between public posture and informal reality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Regional allies and the nonproliferation regime experience pure extraction (Snare) — they are trapped, excluded, and bear costs without benefits. The US and Iran experience mixed coordination and extraction (Tangled Rope) — they both benefit from mutual de-escalation (coordination) while structuring the arrangement to extract favorable terms (extraction). Russia and China experience pure coordination (Rope) — they benefit from reduced conflict risk and can arbitrage between formal/informal arrangements. The formal treaty apparatus experiences degradation (Piton) — its institutional role persists but functional verification has been displaced. The analytical observer sees a temporary arrangement (Scaffold) with an implicit sunset — informality creates pressure toward either formalization (once US politics change) or breakdown (if violations emerge). The perspectival gaps arise from structural asymmetries: who was included in negotiations determines their experience. Beneficiaries see coordination; victims see extraction; observers see temporality and theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the extraction flow. The US and Iran, as primary beneficiaries with power to negotiate, experience low d values (beneficiary + organized/powerful → d ≈ 0.35-0.45) despite constrained exit — they structured the arrangement to benefit themselves. Regional allies, trapped with no negotiating capacity, experience high d values (victim + powerless/trapped → d ≈ 0.95). The nonproliferation regime, bypassed entirely, experiences high d (victim + powerless → d ≈ 0.95). Russia and China, who can arbitrage between formal and informal arrangements, experience low d (beneficiary + institutional/arbitrage → d ≈ 0.15). The formal treaty infrastructure persists but its verification function is displaced, creating ambiguous directionality — it nominally benefits from continued institutional role (IAEA inspections continue) but actually becomes vestigial (real decisions happen off-stage). This directionality distribution (majority of perspectives showing high d or low d) indicates significant perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CASE: This constraint avoids the trap of mislabeling pure extraction (snare) as coordination (rope). From some perspectives it is legitimately a snare (regional allies, nonproliferation regime), from others legitimately tangled rope (US and Iran), from others rope (Russia/China). The mandatrophy is resolved by recognizing that the same constraint is GENUINELY different structural objects from different observation positions. The US administration's experience (tangled rope: both coordinate and extract) is not the same constraint as Israel's experience (snare: pure exclusion and extraction). The perspectival gap is not measurement error — it is structural reality. The informality itself is the resolution mechanism: by keeping the arrangement informal, the US and Iran avoid the formal commitment that would force the nonproliferation regime or Congress into the negotiation, which would reduce their extracted benefits. Informality = structural device to maximize extraction from parties with no veto power while maintaining coordination with the peer power (Iran). The theater ratio (0.65) reflects that this extraction mechanism depends on plausible deniability: public messaging emphasizes cooperation and de-escalation (coordination frame) while actual terms emphasize exclusion and verification asymmetry (extraction frame). The arrangement survives because affected parties either benefit (US, Iran) or are structurally unable to mobilize (regional allies, nonproliferation regime).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informal_vs_binding,
    'Does ''informal'' create a structurally different constraint from a formal treaty, or is it the same constraint with different theater ratio?',
    'Empirical test: Compare compliance behavior under formal JCPOA (2015-2018) to informal arrangement (2023-present). If behavioral patterns are similar, it is the same constraint with higher theater. If compliance mechanisms fundamentally differ, it is a different constraint.',
    'If same constraint: the ε value should remain ~0.52 and classification is stable tangled_rope. If different constraint: informal arrangement may have lower ε (pure coordination) or higher ε (pure extraction/snare) depending on verification sufficiency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informal_vs_binding, empirical, 'Whether informality creates structurally different constraint').

omega_variable(
    verification_sufficiency,
    'Can Iran''s compliance be verified through informal signaling and intelligence channels, or does effective verification require the IAEA institutional apparatus?',
    'Post-2023 case analysis: Document specific verification events (breakout scenarios, facility access, enrichment alerts). Compare detection speed and confidence under informal vs formal JCPOA arrangements. Cross-reference with IAEA technical capacity assessments.',
    'If informal verification is sufficient: suppression score should be lower (~0.45), classification may shift to rope. If insufficient: suppression should increase (~0.80), classification may become snare. This is the most consequential omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_sufficiency, empirical, 'Whether informal verification mechanisms suffice').

omega_variable(
    us_domestic_constraint_asymmetry,
    'Does the US domestic political constraint (Senate opposition to formal treaty) represent a genuine structural limit of the constraint, or is it a temporary political artifact?',
    'Monitor US Congressional positions on Iran nuclear diplomacy. Track whether future administrations could formalize the arrangement. Assess whether Senate dynamics are structural or cyclical.',
    'If structural: the informality is not temporary, and the constraint is a stable tangled_rope or piton. If temporary: the constraint is a genuine scaffold with a real sunset toward formalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_domestic_constraint_asymmetry, preference, 'Whether US domestic constraint is structural or temporary').

omega_variable(
    regional_ally_coalition_capacity,
    'Can excluded regional allies (Israel, Gulf states) credibly threaten to exit the implied coordination framework, or are they structurally trapped?',
    'Analyze military action scenarios (Israeli strikes, Gulf arms race) and their relationship to informal arrangement. Track whether allies develop independent verification or deterrence pathways.',
    'If capable of credible threat: regional allies'' exit options improve from trapped to constrained, reducing their experienced extraction. If powerless: they remain snares from their perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_ally_coalition_capacity, empirical, 'Whether regional allies can credibly exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_nuclear_deal_informal_2023, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_informal_tr_t0, iran_nuclear_deal_informal_2023, theater_ratio, 0, 0.5).
narrative_ontology:measurement(iran_informal_tr_t6, iran_nuclear_deal_informal_2023, theater_ratio, 6, 0.58).
narrative_ontology:measurement(iran_informal_tr_t12, iran_nuclear_deal_informal_2023, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(iran_informal_be_t0, iran_nuclear_deal_informal_2023, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(iran_informal_be_t6, iran_nuclear_deal_informal_2023, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(iran_informal_be_t12, iran_nuclear_deal_informal_2023, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_nuclear_deal_informal_2023, enforcement_mechanism).
narrative_ontology:affects_constraint(iran_nuclear_deal_informal_2023, saudi_iran_rapprochement_2023).
narrative_ontology:affects_constraint(iran_nuclear_deal_informal_2023, iaea_verification_asymmetry).
narrative_ontology:affects_constraint(iran_nuclear_deal_informal_2023, regional_ally_security_dependency).

% DUAL FORMULATION NOTE:
% The informal arrangement decomposes into two structurally distinct constraints: (1) US-Iran mutual restraint (ε ≈ 0.35, tangled_rope) — genuine coordination with asymmetric extraction; (2) Regional exclusion and nonproliferation regime bypass (ε ≈ 0.70, snare) — pure extraction from trapped actors. These are linked: the informal status of constraint (1) enables the extraction in constraint (2). Downstream constraints show how the informality propagates: Saudi-Iran rapprochement requires parallel exclusion logic; IAEA verification becomes asymmetric; regional allies deepen security dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_nuclear_deal_informal_2023, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
