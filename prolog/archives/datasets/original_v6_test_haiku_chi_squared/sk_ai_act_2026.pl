% ============================================================================
% CONSTRAINT STORY: sk_ai_act_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sk_ai_act_2026, []).

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
 *   constraint_id: sk_ai_act_2026
 *   human_readable: South Korea's Proposed AI Industry Promotion Act
 *   domain: technological/political
 *
 * SUMMARY:
 *   South Korea's proposed AI Industry Promotion Act creates a legal
 *   framework granting AI developers broad exemptions from privacy and
 *   copyright law, framed as temporary industrial policy to accelerate
 *   national AI competitiveness. The act exemplifies a structural tension
 *   between national technological development incentives and global data
 *   protection norms. The same regulatory architecture exhibits all major
 *   constraint types depending on observer position: pure extraction for data
 *   subjects (snare), mixed coordination-extraction for the domestic AI
 *   industry (tangled rope), pure coordination for the government (rope),
 *   mixed extraction with organized resistance for international firms
 *   (tangled rope), temporary crisis framework for advocates (scaffold), and
 *   degraded norm violation for the global privacy regime (piton). The
 *   constraint's extractiveness has risen from 0.35 (initial proposal) to
 *   0.58 (current draft) as exemptions have expanded beyond anonymized
 *   research data to include commercial model training. Theater ratio (0.52)
 *   reflects mixed function: the exemption does coordinate industry around a
 *   common data pipeline, but also requires performative compliance
 *   mechanisms (ethics review boards, vague 'research purpose'
 *   certifications) that operate as theater masking the extraction.
 *
 * KEY AGENTS:
 *   - Data Subjects (citizens): Primary victims (powerless/trapped) — personal data becomes non-consensual training material; no exit option within Korean jurisdiction
 *   - Copyright Holders (international): Secondary victims (moderate/constrained) — creative works extracted without compensation; exit constrained by enforcement slowness and foreign jurisdiction
 *   - Domestic AI Companies: Primary beneficiaries (organized/constrained) — gain coordinated data access and competitive advantage; constrained by potential revocation or international retaliation
 *   - Technology Ministry & Government: Institutional beneficiary (institutional/arbitrage) — shapes policy directly; arbitrage exit to alternative industrial policies; net coordinator
 *   - International Tech Companies & WIPO Coalition: Organized victims (organized/mobile) — face extraction but have mobility through trade pressure, WTO disputes, reciprocal exemptions
 *   - Privacy Advocates & Civil Society: Secondary agents (powerful/mobile) — build alternatives (differential privacy, federated learning); frame exemption as temporary scaffold
 *   - Global Privacy Regulatory Regime: Institutional observer (institutional/arbitrage) — GDPR/CCPA represent international epistemic standard; South Korean exemption degrades this norm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_ai_act_2026, 0.58).
domain_priors:suppression_score(sk_ai_act_2026, 0.68).
domain_priors:theater_ratio(sk_ai_act_2026, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_ai_act_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(sk_ai_act_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sk_ai_act_2026, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sk_ai_act_2026, tangled_rope).
narrative_ontology:human_readable(sk_ai_act_2026, "South Korea's Proposed AI Industry Promotion Act").
narrative_ontology:topic_domain(sk_ai_act_2026, "technological/political").

domain_priors:requires_active_enforcement(sk_ai_act_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sk_ai_act_2026, domestic_ai_companies).
narrative_ontology:constraint_beneficiary(sk_ai_act_2026, technology_ministry).
narrative_ontology:constraint_victim(sk_ai_act_2026, data_subjects).
narrative_ontology:constraint_victim(sk_ai_act_2026, copyright_holders).
narrative_ontology:constraint_victim(sk_ai_act_2026, competing_jurisdictions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Citizens whose personal data becomes trainable material without consent. No exit option within South Korean jurisdiction; cannot opt out of data collection sweeps. Suppression is explicit (exemption from privacy law). d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. Full extraction mechanism.
constraint_indexing:constraint_classification(sk_ai_act_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERNATIONAL COPYRIGHT HOLDER (SNARE) — Publishers, authors, creative industries outside South Korea face material extraction of works for training data. Exit option is constrained: legal challenge requires engagement with foreign jurisdiction, expensive and slow. Suppression is structural (exemption applies only within Korean territory, creating asymmetric extraction). d≈0.85, f(d)≈1.20, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(sk_ai_act_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMESTIC AI INDUSTRY COALITION (TANGLED ROPE) — South Korean AI companies benefit from coordinated data access (coordination function) AND asymmetric extraction from data subjects and foreign creators. Benefits from reduced compliance overhead, competitive advantage during training window, and state backing. Constrained because exemptions could be revoked or reciprocated by trading partners. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.20. Low effective extraction from industry perspective — the law reads as pure coordination benefit.
constraint_indexing:constraint_classification(sk_ai_act_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TECHNOLOGY MINISTRY & GOVERNMENT (ROPE) — Primary institutional beneficiary. Exercises direct policy control with maximum flexibility. Can revise exemptions, grant safe harbors selectively, or sunset the act if international pressure mounts. Arbitrage exit: can shift to alternative industrial policy (semiconductor subsidies, chip foundry investment) without constraint. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary. Experiences constraint as pure coordination mechanism (coordinating industry around a common policy direction).
constraint_indexing:constraint_classification(sk_ai_act_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL TECH COMPANIES & WIPO COALITION (TANGLED ROPE) — US, EU, and international publishers organized through WIPO and trade associations. Face extraction (uncompensated use of works for Korean AI training). But also have mobility: can lobby trade negotiations, threaten IP sanctions, pursue WTO dispute, or themselves adopt analogous extraction policies in their home markets (race-to-the-bottom logic). Constrained because retaliation is slow and uncertain. d≈0.58, f(d)≈0.80, σ=1.1 → χ≈0.51. Moderate extraction; some mitigation through exit options (mobility, organized pressure).
constraint_indexing:constraint_classification(sk_ai_act_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PRIVACY ADVOCATES & CIVIL SOCIETY (SCAFFOLD) — Korean and international NGOs frame the exemption as temporary, contingent on demonstrating measurable AI sector gains. The scaffold logic: sunset clause after 7-10 years if alternative funding/training mechanisms emerge (federated learning, synthetic data, licensed datasets). Theater_ratio low in this perspective because advocates are building alternative technical pathways (differential privacy, on-device training) that bypass the exemption entirely. d≈0.62, f(d)≈0.88, σ=1.0 → χ≈0.30. Moderate extraction but declining trajectory via technical alternatives.
constraint_indexing:constraint_classification(sk_ai_act_2026, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: GLOBAL PRIVACY REGULATORY REGIME (PITON) — GDPR, CCPA, and emerging regional privacy norms have become the global epistemic standard for data protection. South Korea's exemption is experienced by the international regime as degraded — a rollback of established norms maintained through nationalist industrial policy, not through demonstration that the exemption serves coordination. Theater_ratio=0.52 reflects mixed function: the exemption does coordinate industry BUT also creates visible norm violation. The global privacy regime sees this as institutional theater: the exemption persists via state power and competitive pressure, not because it's functionally superior to privacy-preserving alternatives.
constraint_indexing:constraint_classification(sk_ai_act_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN?) — Risk of naturalizing the act as inevitable response to 'the law of AI development' — that global AI competitiveness necessarily requires unrestricted data access. This view sees exemptions as immutable feature of any nation pursuing AI leadership. However, base properties contradict mountain: ε=0.58 (too high for natural law), suppression=0.68 (explicit coercion, not natural scarcity), theater=0.52 (mixed function, not inherent limit). The engine's false summit detector will flag this perspective as a misclassification — the 'naturalness' is aspirational framing by beneficiaries, not structural inevitability.
constraint_indexing:constraint_classification(sk_ai_act_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_ai_act_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sk_ai_act_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sk_ai_act_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sk_ai_act_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sk_ai_act_2026, TR),
    TR >= 0.70.

:- end_tests(sk_ai_act_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The act grants unrestricted collection and processing of personal data without consent for AI training — a significant extraction from data subjects. However, it falls short of maximal extraction (0.70+) because: (1) the exemption is territorially bounded (applies only within South Korea), limiting total extractable data; (2) international copyright holders retain legal recourse in foreign courts, though expensive; (3) the act includes nominal ethics review procedures (theater, but reducing bare extraction); (4) competitive pressure and international responses will constrain duration. Suppression (0.68): High. Suppression is explicit and multifaceted: privacy law is suspended by statute, copyright law is exempted, consent requirements are waived, and no data subject notification is mandated. Suppression is not maximal (0.70+) because data subjects retain some informal exit options (VPN, pseudonymization, alternative identity use) and privacy advocates retain international recourse. Theater ratio (0.52): Mixed. The ethics review procedures (Research Ethics Boards) are performative theater — they lack enforcement power, clear standards, or meaningful review authority. However, the core function (coordinating industry around a data pipeline) is genuinely structural, not purely theatrical. Unlike a piton, the core coordination function remains intact; unlike a rope, significant extraction remains visible. The trajectory over time shows theater increasing (more review procedures added) while core extraction also increases (scope expanded), suggesting the act is accumulating legitimacy theater while actual extraction grows.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by directionality. Data subjects and copyright holders perceive snare — they are trapped and harvested. The domestic AI industry perceives rope — pure coordination benefit with maximum exit options (arbitrage to other policies). International firms perceive tangled rope — they face real extraction but have organized power to push back. Privacy advocates perceive scaffold — a temporary problem being solved by technical alternatives (privacy-preserving ML). The global privacy regime perceives piton — the exemption violates established norms but persists through state power, not through demonstrated superiority. The analytical observer risks mountain — naturalizing extraction as inevitable law of AI development. This perspectival spread (snare → rope → tangled rope → scaffold → piton → mountain) is maximal, indicating the constraint has not yet stabilized into a clear type. The instability is driven by the omega variables: if technical alternatives succeed, scaffold → rope; if international retaliation escalates, rope → snare (for beneficiaries); if courts strike the exemption, all perspectives collapse. The constraint is a pure case of contested institutional architecture with unstable equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects: Victim + trapped within national boundaries → d≈0.92, f(d)≈1.40. Cannot exit, cannot organize, cannot legally object. Maximum d derives from structural powerlessness combined with full victimhood (data extraction without compensation). Copyright holders: Victim + constrained (can litigate but slowly, expensively, in foreign courts) → d≈0.85, f(d)≈1.20. High d because extraction is uncompensated and escape routes are blocked by jurisdiction and cost. Domestic AI companies: Beneficiary + constrained (could lose exemptions via retaliation or domestic political shift) → d≈0.35, f(d)≈0.35. Low d because they are net beneficiaries of the structure; constraint is experienced as coordinating opportunity, not extraction. Technology ministry: Beneficiary + arbitrage (can shift to other policies without constraint) → d≈0.08, f(d)≈-0.10. Minimal d; net beneficiary with maximum exit flexibility. International tech coalition: Mixed (both beneficiaries of reciprocal exemptions globally AND victims of this specific exemption) + organized + mobile → d≈0.58, f(d)≈0.80. Mid-range d reflecting that they can push back through coordinated action but face real extraction costs during the act's duration. Global privacy regime: Institutional observer + institutional power but constrained by state sovereignty → d≈0.65, f(d)≈0.95 (not explicitly computed but implied piton observation). The regime sees itself as victim of norm erosion but lacks direct enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is UNRESOLVED. The constraint contains genuine coordinating function (aligning industry around shared data infrastructure, enabling Korean AI sector coordination) AND genuine extraction (uncompensated data use, copyright violation, consent waiver). The challenge is decomposing whether the extraction is NECESSARY to the coordination or merely EXPLOITATIVE. (1) NECESSARY EXTRACTION pathway: Unrestricted data access is technically required for competitive model training. Privacy-preserving alternatives (federated learning, differential privacy, synthetic data) cannot match performance. In this case, the exemption is a justified temporary measure — tangled rope with legitimate asymmetry. The sunset clause omega (#1, data compensation) determines whether this legitimacy is acknowledged. (2) PURELY EXPLOITATIVE pathway: Privacy-preserving methods are technically sufficient, but exemption is cheaper and faster — extraction serves rent-seeking, not coordination. In this case, the exemption is a snare masquerading as industrial policy. Omega #2 (technical necessity) will resolve this. (3) RACE-TO-THE-BOTTOM pathway: South Korea's exemption triggers reciprocal exemptions in US, EU, China, and India. Global convergence to mutual extraction (all countries exempting copyright and privacy for their AI sectors) becomes normalized as 'the cost of AI competitiveness.' In this case, all perspectives shift: what appears as snare to Korean data subjects appears as rope to a global AI industry that has collectively exempted itself. Omega #3 (reciprocal liability) resolves this. CURRENT STATUS: High extractiveness (0.58 > 0.46) triggers mandatrophy requirement. The commentary declares the constraint TANGLED ROPE with the explicit caveat that this is contingent on omegas #2 and #3 resolving toward 'technical necessity' and 'reciprocal legitimation.' If omegas resolve opposite — technical alternatives exist AND retaliation escalates — the constraint reclassifies to SNARE and mandatrophy is violated (false tangled rope). The act's actual classification will be determined by 3-5 years of empirical data: Do Korean AI models outperform privacy-trained alternatives? Do other jurisdictions retaliate or reciprocate? The framework's role is to make this instability explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_subject_compensation_sufficiency,
    'Would mandatory compensation for data subjects (micropayments, benefit-sharing) convert the snare into a tangled rope, or would it remain extraction despite token payments?',
    'Comparative analysis of South Korean public opinion on acceptable compensation levels; international precedent (EU data dividend proposals, California CPRA buyouts); economic modeling of training data value extraction',
    'If compensation ≥10% of AI model value generation: reclassifies snare perspective as tangled rope (coordination + asymmetric but acknowledged extraction). If compensation <2% of value: snare classification confirmed; compensation is performative theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_subject_compensation_sufficiency, empirical, 'Whether compensation could legitimize data extraction').

omega_variable(
    technical_training_necessity,
    'Is unrestricted data access technically necessary for competitive AI training, or are privacy-preserving alternatives (federated learning, synthetic data, differential privacy) sufficient for the state-of-the-art?',
    'Benchmark performance of Korean AI models trained under exemption vs. international models trained on privacy-preserving methods; technical assessment by independent ML researchers; follow-up performance metrics over 3-5 years',
    'If privacy-preserving methods prove sufficient: exemption is purely extractive, not coordination-driven; reclassifies from tangled rope toward pure snare. If unrestricted data proves materially superior: strengthens tangled rope / scaffold narratives (legitimate competitive need for temporary exemption).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_training_necessity, empirical, 'Whether unrestricted data access is technically necessary for competitive AI').

omega_variable(
    international_reciprocal_liability,
    'Will exemptions in other countries (US domestic foundation model training, EU data adequacy decisions, China''s training data policies) converge into mutual legitimation or escalate into bilateral sanctions?',
    'Tracking of US, EU, and China exemption policies; analysis of trade dispute filings and IP litigation patterns; monitoring of technology transfer restrictions and sanctions threats',
    'If reciprocal exemptions emerge: globally normalized extraction (all perspectives see rope/tangled rope). If retaliation escalates: snare perspective strengthens for Korean companies (trapped by trade war), scaffold collapse (international sunset pressure evaporates), and global regime pitonification (privacy norms become vestigial theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_reciprocal_liability, empirical, 'Whether exemptions will be reciprocated or trigger retaliation').

omega_variable(
    domestic_legal_challenge_route,
    'Will Korean constitutional courts, administrative courts, or international human rights bodies sustain the privacy exemption against data subject challenges?',
    'Monitoring of filed lawsuits; analysis of Korean constitutional law precedent (right to privacy, informational self-determination); international human rights committee positions; analysis of sunset clause triggers',
    'If domestic courts strike exemption: constraint dissolves (snare becomes moot, scaffold accelerates). If courts sustain: data subject victimhood is formally legitimized (snare perspective confirmed in law). If international human rights body finds violation: global regime pressure mounts (piton degradation accelerates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_legal_challenge_route, empirical, 'Legal sustainability of exemption under domestic and international law').

omega_variable(
    sunset_clause_enforceability,
    'If a sunset clause is written into the act (scaffold logic), will politicians actually allow it to expire, or will competitive pressure force indefinite renewal?',
    'Analysis of prior industrial policy sunsets in South Korea (semiconductor export restrictions, shipbuilding subsidies); monitoring of industry lobbying intensity as sunset deadline approaches; international pressure measurement (trade disputes, IP litigation volume)',
    'If sunsets enforced: scaffold classification confirmed; institutional commitment to transition to privacy-preserving alternatives. If renewed indefinitely: scaffold is aspirational theater; reclassifies piton (performative sunset threat maintained but not executed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_enforceability, preference, 'Political will to enforce sunset clauses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sk_ai_act_2026, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sk_ai_tr_t0, sk_ai_act_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sk_ai_tr_t3, sk_ai_act_2026, theater_ratio, 3, 0.45).
narrative_ontology:measurement(sk_ai_tr_t7, sk_ai_act_2026, theater_ratio, 7, 0.52).

% Extraction over time
narrative_ontology:measurement(sk_ai_be_t0, sk_ai_act_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sk_ai_be_t3, sk_ai_act_2026, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sk_ai_be_t7, sk_ai_act_2026, base_extractiveness, 7, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sk_ai_act_2026, resource_allocation).
narrative_ontology:affects_constraint(sk_ai_act_2026, global_ai_data_governance).
narrative_ontology:affects_constraint(sk_ai_act_2026, gdpr_extraterritorial_scope).
narrative_ontology:affects_constraint(sk_ai_act_2026, copyright_ai_training_liability).

% DUAL FORMULATION NOTE:
% This constraint is downstream of two distinct upstream pressures: (1) technical claims about AI training data requirements (epsilon ≈ 0.42, contested); (2) geopolitical race dynamics in semiconductor/AI leadership (epsilon ≈ 0.35, structural). The 0.58 extractiveness of the act itself represents a mixed response to both pressures. Decomposition: global_ai_data_governance (ε=0.35, rope/scaffold) establishes the international coordination problem; sk_ai_act_2026 (ε=0.58, tangled rope) is a unilateral response that transforms coordination into extraction. The act also feeds back into copyright_ai_training_liability (ε=0.48, tangled rope) by establishing precedent for statutory exemption rather than statutory liability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sk_ai_act_2026, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
