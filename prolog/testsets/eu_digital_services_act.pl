% ============================================================================
% CONSTRAINT STORY: eu_digital_services_act
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_digital_services_act, []).

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
 *   constraint_id: eu_digital_services_act
 *   human_readable: EU Digital Services Act (DSA)
 *   domain: technological/political
 *
 * SUMMARY:
 *   The EU Digital Services Act (DSA), which entered into force in 2024 and
 *   became fully operational in 2025, represents a watershed moment in global
 *   digital platform regulation. It attempts to solve a genuine coordination
 *   problem (fragmented EU regulatory regimes for online platforms,
 *   inconsistent content moderation, information asymmetries between
 *   platforms and regulators) while simultaneously extracting costs from
 *   multiple actors. The constraint exhibits the full six-type perspectival
 *   spread: non-EU platforms experience pure extraction (Snare); EU
 *   regulatory institutions experience genuine coordination with
 *   institutional expansion (Tangled Rope); compliance vendors experience
 *   market opportunity with minimal extraction (Rope); EU users experience
 *   continued data extraction masked by transparency theater (Snare);
 *   startups face entry barriers that consolidate incumbent dominance
 *   (Tangled Rope); digital rights advocates see a temporary framework with
 *   sunset potential (Scaffold); national content courts provide performative
 *   due process masking algorithmic extraction (Piton); and the
 *   civilizational analytical view risks naturalizing contingent political
 *   choices as inevitable law (false Mountain). The constraint's evolution
 *   from 2024 implementation through 2025 shows extractiveness rising (0.38 →
 *   0.52) and theater increasing (0.52 → 0.64), suggesting platforms are
 *   investing heavily in compliance infrastructure while maintaining
 *   extraction mechanisms, and regulators are accumulating enforcement
 *   apparatus that depends on continued DSA authority for legitimacy.
 *
 * KEY AGENTS:
 *   - EU Regulatory Institutions (Digital Services Coordinators, European Commission): Institutional beneficiaries — gain new enforcement authority, funding, regulatory power over global platforms. Exit options: arbitrage (can modify or repeal DSA).
 *   - Large Global Platforms (Meta, Google, Amazon, TikTok): Primary targets — bear compliance costs, legal risk, ongoing compliance infrastructure investment. Exit options: constrained (must serve 400M+ EU market; exit costs exceed compliance costs for all major platforms).
 *   - Non-EU Technology Companies: Secondary targets — face compliance costs for EU service provision; experience extraction as entry tax to European market. Exit options: trapped (cannot ignore EU market) or arbitrage (accept compliance costs as necessary operating expense).
 *   - EU Users (400M+ population): Nominal beneficiaries but structurally victim-adjacent — gain transparency rights and content moderation recourse, but lose (or maintain) behavioral data autonomy. Exit options: constrained (network effects lock users into major platforms; alternative platforms face DSA barriers).
 *   - Small EU/Global Tech Startups: Victims — face high compliance costs that prevent market entry. Exit options: mobile (can serve non-EU markets, slower growth path) or trapped (cannot launch in EU market).
 *   - Compliance Infrastructure Vendors (legal tech, content moderation services): Beneficiaries — gain large TAM from platform compliance demand. Exit options: arbitrage (can develop adjacent services in non-DSA markets).
 *   - Digital Rights Advocates (civil society, privacy organizations): Secondary beneficiaries — DSA provides enforcement mechanism for user rights; view regulation as temporary step toward stronger digital autonomy. Exit options: constrained (have agency through consultation and advocacy, can propose modifications).
 *   - National Content Moderation Courts: Institutional actors maintaining degraded process — provide judicial review theater while platforms maintain algorithmic extraction. Exit options: arbitrage (can modify procedures, but embedded in national legal systems).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_digital_services_act, 0.52).
domain_priors:suppression_score(eu_digital_services_act, 0.58).
domain_priors:theater_ratio(eu_digital_services_act, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_digital_services_act, extractiveness, 0.52).
narrative_ontology:constraint_metric(eu_digital_services_act, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(eu_digital_services_act, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_digital_services_act, tangled_rope).
narrative_ontology:human_readable(eu_digital_services_act, "EU Digital Services Act (DSA)").
narrative_ontology:topic_domain(eu_digital_services_act, "technological/political").

domain_priors:requires_active_enforcement(eu_digital_services_act).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_digital_services_act, eu_regulatory_institutions).
narrative_ontology:constraint_beneficiary(eu_digital_services_act, content_moderation_workers).
narrative_ontology:constraint_beneficiary(eu_digital_services_act, platform_compliance_infrastructure_vendors).
narrative_ontology:constraint_victim(eu_digital_services_act, eu_users_data_autonomy).
narrative_ontology:constraint_victim(eu_digital_services_act, non_eu_platforms).
narrative_ontology:constraint_victim(eu_digital_services_act, small_tech_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-EU PLATFORM TRAPPED COMPLIANCE — Global platforms operating in EU must comply with DSA requirements (content labeling, algorithmic transparency, user data protection) without meaningful participation in rule-making. Exit: cannot serve 400M+ EU market without compliance. Extraction: operating cost increases 15-40% due to specialized compliance infrastructure, legal analysis, and content moderation scaling. Suppression: high — alternative pathways (lobbying, legal challenge, graduated compliance) are expensive and slow.
constraint_indexing:constraint_classification(eu_digital_services_act, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EU REGULATORY INSTITUTIONS (TANGLED ROPE) — DSA creates new enforcement apparatus (Digital Services Coordinators in each member state) with power to investigate, impose fines, and audit platforms. Benefits: institutional expansion, funding, regulatory authority, policy implementation capability. Costs: coordination overhead across 27 member states, technical expertise gaps, political pressure to show enforcement results. Extraction: enforced by institutional actors with exit option (can modify DSA) but constrained by inter-member agreement. Coordination function: genuine — DSA coordinates digital market rules across fragmented EU regulatory landscape.
constraint_indexing:constraint_classification(eu_digital_services_act, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: COMPLIANCE INFRASTRUCTURE VENDORS (ROPE) — DSA creates immediate demand for compliance-as-a-service: automated content moderation, algorithmic transparency tools, user data portal infrastructure, regulatory reporting systems. Vendors benefit from market expansion and new revenue streams. Suppression: low — vendors have exit options (develop non-EU services, pivot to adjacent markets). Extraction: minimal — coordination solves a real platform problem (fragmented EU regulations). Pre-DSA, platforms faced 27 different regulatory regimes; DSA harmonizes requirements, reducing vendor coordination costs.
constraint_indexing:constraint_classification(eu_digital_services_act, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EU USERS' DATA AUTONOMY (SNARE) — DSA mandates algorithmic transparency and user data rights, but enforcement is weak. Platforms remain incentivized to maximize data extraction (tracking, profiling, behavioral prediction) within DSA compliance boundaries. Theater: significant — mandatory transparency reports fulfill DSA letter without addressing underlying business model dependency on behavioral data. Users cannot exit (network effects lock them into major platforms) and cannot audit algorithmic systems (transparency obligations provide summary-level reporting, not source-code access or real-time verification). Extraction: platforms maintain full behavioral data pipeline while nominally complying with transparency rules.
constraint_indexing:constraint_classification(eu_digital_services_act, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: SMALL TECH STARTUPS (TANGLED ROPE) — DSA compliance costs create high barriers to market entry. Startups must implement content moderation, algorithmic documentation, user reporting systems before launch. Benefits: DSA eliminates larger competitors from low-compliance markets (e.g., startups can differentiate on privacy/transparency). Costs: compliance infrastructure ($2-10M annually for viable platform) favors well-capitalized entrants, killing bootstrapped competitors. Exit options: constrained — cannot avoid DSA compliance if serving EU users, but can choose to serve non-EU markets first (slower growth, smaller addressable market).
constraint_indexing:constraint_classification(eu_digital_services_act, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITAL RIGHTS ADVOCACY COALITION (SCAFFOLD) — Civil society organizations, privacy advocates, and small-platform coalitions experience DSA as temporary regulation with sunset potential. Coordination function: genuine — DSA provides enforcement mechanism for user rights and content safety that market competition alone would not deliver. Suppression: moderate and declining — organized advocates have agency (formal consultation rights, legal standing, media access) and can propose modifications. Exit path: structured — advocacy organizations envision DSA as stepping stone to stronger user-rights architecture (interoperability mandates, data portability, algorithmic auditing). Sunset implicit: DSA viewed as transitional framework pending more radical digital platform restructuring.
constraint_indexing:constraint_classification(eu_digital_services_act, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: NATIONAL CONTENT MODERATION COURTS (PITON) — DSA mandates expedited judicial review of platform content removal decisions. Theater: high (0.68+). The judicial process provides legitimate due process appearance but operates with severe information asymmetry: judges assess content policy consistency, not empirical harms or platform algorithmic impact. Process persists because it satisfies formal procedural requirements (right to appeal, judicial review) without addressing underlying problem: platforms can be simultaneously compliant with all DSA judicial rulings while maintaining extractive algorithmic systems. Theater ratio reflects that procedural compliance masks substantive failure to regulate platform incentive structures.
constraint_indexing:constraint_classification(eu_digital_services_act, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE UNIVERSALITY VIEW (MOUNTAIN) — From a civilizational perspective, digital platform regulation is sometimes framed as a natural law: network effects create inevitable monopoly concentration, data extraction is inherent to algorithmic optimization, and regulatory fragmentation is an immutable feature of multi-jurisdictional governance. This perspective sees DSA as merely acknowledging structural inevitability. HOWEVER: this classification fails the mountain gates. DSA is not naturally emerging (requires active EU institutional enforcement), has significant resistance (platforms contest rules), and accessibility is not collapsed (alternatives exist: interoperability, public platforms, cooperative networks). The 'natural law' framing naturalizes what is actually a contingent political choice. Engine's false summit detector will flag this perspective as naturalization.
constraint_indexing:constraint_classification(eu_digital_services_act, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_digital_services_act_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_digital_services_act, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_digital_services_act, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_digital_services_act, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_digital_services_act, TR),
    TR >= 0.70.

:- end_tests(eu_digital_services_act_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising. DSA imposes measurable costs on non-EU platforms: compliance infrastructure ($2-10M annually), legal analysis, content moderation scaling, algorithmic documentation, and ongoing regulatory uncertainty. Non-EU platforms cannot exit without losing EU market access, establishing high f(d) and large χ for this agent class. However, extractiveness is not 0.70+ because: (1) compliance costs are manageable for largest platforms (absorbed as operating expense), (2) some platforms benefit from DSA (smaller EU competitors face equal barriers), and (3) no permanent extraction mechanism (DSA can be modified). Rising trajectory reflects increasing enforcement intensity and expanding compliance scope (originally targeting VLOPs; scope broadened to smaller platforms over implementation period). Suppression (0.58): Moderate-high. Barriers to non-compliance or reduced extraction are substantial: legal risk (fines up to 6% of global revenue), reputational cost, operational risk (service restrictions). But suppression is not total — platforms can challenge enforcement decisions through courts, negotiate compliance pathways, and lobby for rule modifications (slow, expensive, but possible). Theater ratio (0.64): High, rising. Algorithmic transparency mandates create significant theater: platforms produce compliance documentation (system cards, ranking factor explanations) that provide limited operational insight into actual algorithmic behavior. Content moderation expedited appeals provide judicial review theater without addressing underlying incentive structures. Compliance infrastructure investment is partially genuine (real moderation scaling) and partially theater (documentation that enables limited third-party verification). Rising trajectory reflects that theater intensity increases as platforms mature their compliance processes: initial compliance is operational; ongoing compliance becomes increasingly performative (maintaining appearance of compliance with minimal algorithmic change).
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives is substantial because DSA distributes costs and benefits asymmetrically across agent types. EU institutions (Tangled Rope, Rope) experience genuine coordination gains and minimal extraction; non-EU platforms (Snare) experience pure extraction with no coordination benefit; users (Snare) appear to benefit but structurally remain locked-in victims; startups (Tangled Rope) face barriers that reinforce incumbent dominance; advocates (Scaffold) experience temporary constraint with exit pathway. The widest gap is between institutional beneficiaries (who designed DSA to solve coordination problems and gain regulatory authority) and trapped victims (non-EU platforms and users with network lock-in), whose views of the same constraint are nearly opposite. The piton and false-mountain perspectives reveal that theater and naturalization are significant risks: describing DSA as inevitable (mountain) or merely procedural (piton) obscures the contingent political choices embedded in rule design. The scaffold perspective offers a structural corrective: DSA is not immutable but explicitly temporary, contingent on development of more radical alternatives (interoperability, data portability, algorithmic auditing). This perspectival heterogeneity is diagnostic of a hybrid constraint (Tangled Rope) where genuine coordination and genuine extraction coexist.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position: beneficiary vs victim status, combined with exit options and power level. Non-EU platforms are victims (bear extraction costs) with trapped exit (cannot serve EU without compliance) → high d → high f(d) → high χ, producing Snare classification. EU regulatory institutions are beneficiaries (gain enforcement authority) with arbitrage exit (can modify DSA) → low d → low/negative f(d) → moderate χ with coordination function, producing Tangled Rope. Compliance vendors are beneficiaries (new market) with arbitrage exit (serve non-EU markets) → low d → negative f(d) → low χ, producing Rope. EU users are nominally beneficiaries (transparency rights) but structurally victims (data extraction continues, network lock-in) with constrained exit (cannot choose alternative platforms due to network effects) → moderate-to-high d → moderate-to-high f(d) → high χ, producing Snare. Startups are victims (face entry barriers) with mobile exit (non-EU markets) but constrained by growth velocity → moderate d → moderate f(d) → moderate χ, producing Tangled Rope. Digital rights advocates are beneficiaries (DSA enforcement tool) with constrained exit (embedded in advocacy system) but genuine agency (can propose modifications) → low-to-moderate d → low-to-moderate f(d) → low χ, producing Scaffold. National courts are institutional actors with arbitrage exit but degraded function → moderate d → moderate f(d) with high theater ratio, producing Piton. The perspectival gap emerges because the same DSA structural parameters produce radically different d values depending on agent position: institutional beneficiaries with exit see low extraction; powerless agents with network lock-in see high extraction; organized agents with agency see temporary extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED (extractiveness 0.52 > 0.46 but < 0.70, so mandatrophy not yet required by thresholds, but present analytically). The core mandatrophy question is: Is DSA a coordination mechanism with incidental extraction, or an extraction mechanism with coordination theater? The answer is BOTH, distributed across agent types and evolving over time. For EU institutions and compliance vendors, DSA is genuine coordination that happens to extract from non-EU platforms (they experience Rope or Tangled Rope Rope with clear coordination value). For non-EU platforms, DSA is pure extraction with no coordination benefit (Snare). For users, DSA provides nominal coordination (transparent algorithms, content appeals) but masks persistent extraction (behavioral data, algorithmic ranking for engagement). For startups, DSA creates coordination failure: the rule that should enable fair platform competition instead consolidates incumbent dominance (Tangled Rope with extraction ≥ coordination). The mandatrophy would be RESOLVED (extractiveness → 0.70+) if any of three conditions hold: (1) platforms successfully exit EU market (extractiveness becomes existential threat, rising to snare-pure levels), (2) enforcement coordination across member states collapses (extractiveness fragments into 27 different regimes, rising to snare-via-chaos), or (3) DSA's theater ratio exceeds 0.80 (algorithmic transparency and content appeals are purely performative, masking continued extraction). Current trajectory shows extractiveness rising (0.38 → 0.52) and theater rising (0.52 → 0.64), suggesting mandatrophy may resolve within 2-3 years if these trends continue. Structural recommendation: monitor platforms' strategic compliance investments (genuine algorithmic change vs compliance infrastructure theater) as the key early indicator of whether DSA remains Tangled Rope or degrades toward Snare (if theater > 0.75 or platform behavioral extraction increases post-compliance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_sufficiency,
    'Does DSA-mandated algorithmic transparency (system documentation, ranking factor explanation) provide meaningful user or regulator oversight, or is it performative theater that obscures rather than clarifies platform decision-making?',
    'Empirical audit: compare algorithmic documentation quality across compliant platforms; measure user comprehension of disclosed ranking factors; track enforcement actions based on algorithmic transparency violations (high enforcement rate = documentation is actionable; low enforcement = theater)',
    'If sufficient: DSA provides genuine user-rights mechanism (Rope or Tangled Rope classification holds). If performative: theater ratio increases substantially (→ Piton risk) and user data autonomy remains Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_transparency_sufficiency, empirical, 'Whether algorithmic transparency mandates are effective or performative').

omega_variable(
    compliance_cost_barrier_height,
    'What is the actual compliance cost threshold that prevents startup market entry? Are $2M compliance costs sufficient to block 95% of new entrants, or do some succeed despite barriers?',
    'Longitudinal startup funding data post-DSA implementation (2024-2026); correlation between venture capital allocation to EU tech startups and compliance cost estimates; measurement of new platform launches in EU vs non-EU markets',
    'If high barrier ($5M+): Tangled Rope classification for startups holds; DSA consolidates large-platform dominance. If moderate barrier ($1-2M): barrier effect is real but non-determinative; some startups still launch; Tangled Rope remains but with lower extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_barrier_height, empirical, 'Height of DSA compliance cost barriers to market entry').

omega_variable(
    inter_member_state_enforcement_coherence,
    'Do the 27 national Digital Services Coordinators enforce DSA rules with sufficient coherence that platforms face a unified regulatory environment, or do divergent interpretations create new forms of regulatory fragmentation?',
    'Analysis of DSA enforcement decisions (2024-2026): audit frequency distributions, fine amounts, content removal ratios across member states; measure variance in interpretation of ''illegal content'' and ''systemic risk'' definitions',
    'If coherent: DSA''s coordination function is genuine (Rope/Tangled Rope holds). If divergent: fragmentation persists under new name (DSA becomes Piton — theater of unified regulation masking continued member-state competition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_member_state_enforcement_coherence, empirical, 'Coherence of DSA enforcement across EU member states').

omega_variable(
    platform_strategic_compliance_vs_substantive_change,
    'Do platforms implement DSA compliance as genuine operational changes to ranking algorithms, content moderation, and data practices, or as strategic theater (compliance infrastructure that leaves extraction mechanisms intact)?',
    'Comparative analysis: measure behavioral changes in platform algorithms post-DSA (engagement metrics, recommendation diversity, data collection rates) against pre-DSA baselines; audit compliance infrastructure investment vs algorithmic redesign investment',
    'If substantive: DSA delivers real user-rights gains (Rope/Tangled Rope). If theater: theater ratio is higher than 0.64; extraction mechanisms persist (Snare classification from user perspective is stronger).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_strategic_compliance_vs_substantive_change, empirical, 'Whether platform DSA compliance is genuine operational change or strategic theater').

omega_variable(
    non_eu_platform_exit_probability,
    'Will significant platforms (Meta, Google, TikTok) exit the EU market rather than comply with DSA costs, or will compliance costs be absorbed as cost of doing business?',
    'Platform financial disclosures (2024-2026); statements regarding EU market strategic importance; measurement of platform service reduction (feature withdrawal, user account restrictions, geographic service limitations)',
    'If major exit occurs: DSA extractiveness increases (victims are vindicated, non-EU platforms see enforcement as existential threat). If absorbed: extractiveness remains moderate (costs are real but manageable); compliant equilibrium reached.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_eu_platform_exit_probability, empirical, 'Likelihood of platform market exit vs compliance absorption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_digital_services_act, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsa_tr_t0, eu_digital_services_act, theater_ratio, 0, 0.52).
narrative_ontology:measurement(dsa_tr_t1, eu_digital_services_act, theater_ratio, 1, 0.58).
narrative_ontology:measurement(dsa_tr_t2, eu_digital_services_act, theater_ratio, 2, 0.64).

% Extraction over time
narrative_ontology:measurement(dsa_be_t0, eu_digital_services_act, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dsa_be_t1, eu_digital_services_act, base_extractiveness, 1, 0.45).
narrative_ontology:measurement(dsa_be_t2, eu_digital_services_act, base_extractiveness, 2, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_digital_services_act, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_digital_services_act, eidas_digital_identity).
narrative_ontology:affects_constraint(eu_digital_services_act, platform_algorithmic_transparency).
narrative_ontology:affects_constraint(eu_digital_services_act, content_moderation_liability).
narrative_ontology:affects_constraint(eu_digital_services_act, cross_border_data_flows).

% DUAL FORMULATION NOTE:
% DSA is a high-level enforcement framework that operates alongside several structurally distinct constraints. EIDAS (digital identity verification) is upstream — it provides the technical infrastructure for user identity that DSA transparency rules reference. Platform algorithmic transparency is a sub-constraint (one DSA obligation) that can be analyzed separately with its own epsilon and perspectives. Content moderation liability is an upstream constraint (European intermediary immunity rules, DMCA-adjacent) that DSA refines but does not replace. Cross-border data flows (GDPR, adequacy decisions) intersect with DSA but operate under different extractiveness models. DSA's extractiveness (0.52) reflects the aggregate of these sub-constraints; individual components may have different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_digital_services_act, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
