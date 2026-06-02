% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article17_erasure_right__competitive_moat_reading
 *   human_readable: Article 17 Right to Erasure as Competitive Moat via Compliance Cost Asymmetry
 *   domain: technology_governance/data_protection/competition_policy
 *
 * SUMMARY:
 *   Article 17 of the GDPR establishes a right to erasure, framed normatively
 *   as a fundamental privacy protection. This constraint instantiates ONE
 *   READING of that kernel: the competitive_moat_reading. Under this reading,
 *   Article 17 functions primarily as an incumbent protection mechanism via
 *   compliance cost asymmetry and technical infrastructure requirements, not
 *   as a privacy protection mechanism. The constraint exhibits tangled rope
 *   structure: genuine coordination benefit (data subjects gain erasure
 *   rights; privacy-by-design incentives are real) coexists with asymmetric
 *   extraction (compliance costs that scale with data retention complexity,
 *   not linearly with data volume, creating barriers to new entrants that
 *   incumbents have already amortized). The primary beneficiaries are
 *   incumbent platform operators (Meta, Google, Microsoft, Apple) whose
 *   erasure infrastructure costs are negligible relative to user bases; the
 *   primary victims are new market entrants (startups, regional platforms,
 *   specialized services) who face per-user compliance costs that incumbents
 *   do not. The constraint's extractiveness has risen over the interval (0.35
 *   → 0.61) as enforcement actions have accumulated and technical
 *   interpretation has solidified, clarifying the compliance burden. Theater
 *   ratio (0.45) reflects that much Article 17 compliance activity is
 *   performative: logical deletion (de-indexing from search) rather than
 *   cryptographic destruction; deletion from primary systems but not from
 *   logs, backups, or third-party processors; formal acknowledgment of
 *   deletion requests without meaningful remediation. The constraint differs
 *   structurally from its sibling readings: privacy_fundamental_reading
 *   treats Article 17 as advancing genuine data protection (ε lower,
 *   suppression lower, beneficiary is data subjects);
 *   censorship_mechanism_reading treats it as enabling state erasure requests
 *   (ε different, different victim set, authoritarian government as primary
 *   beneficiary through back-channel enforcement).
 *
 * KEY AGENTS:
 *   - Incumbent Platform Operators (Meta, Google, Microsoft, Apple): Institutional/arbitrage position — primary beneficiaries. Erasure infrastructure already built and amortized; compliance is coordination, not extraction. Capital barriers protect competitive position.
 *   - New Market Entrants (startups, regional platforms, specialized data services): Powerless/trapped or moderate/constrained position — primary victims. Face per-user compliance costs that prohibit profitable market entry. Trapped by regulatory requirement; no technical workaround available.
 *   - Data Protection Authorities (CNIL, ICO, EDPB): Institutional/constrained position — see constraint as transitional coordination problem. Architecture-aware enforcement trajectory suggests scaffold logic (sunset as privacy-by-design matures).
 *   - Data Subjects (Rights Holders): Moderate/constrained — nominal beneficiaries but actual erasure effectiveness is partial due to replication, vendor dependencies, and implementation gaps.
 *   - Compliance-Industrial Complex (law firms, consultancies, audit vendors): Institutional/arbitrage — benefit from theater maintenance; constituency dependent on constraint persistence.
 *   - Analytical Observer: Sees risk of false-summit mountain classification if technical intractability of data erasure is naturalized rather than recognized as capital-contingent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.58).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.62).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "Article 17 Right to Erasure as Competitive Moat via Compliance Cost Asymmetry").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technology_governance/data_protection/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, '90192699-4e7e-4aeb-bd57-6de29761ded2').
narrative_ontology:cs_kernel_codification('90192699-4e7e-4aeb-bd57-6de29761ded2', formalized).
narrative_ontology:cs_authority_grounding('90192699-4e7e-4aeb-bd57-6de29761ded2', extraction).
narrative_ontology:cs_interpretation_layer_present('90192699-4e7e-4aeb-bd57-6de29761ded2').
narrative_ontology:cs_reading_relation('90192699-4e7e-4aeb-bd57-6de29761ded2', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('90192699-4e7e-4aeb-bd57-6de29761ded2', article17_erasure_right__censorship_mechanism_reading, influences).
narrative_ontology:cs_axiom('90192699-4e7e-4aeb-bd57-6de29761ded2', foundational, data_controller_bears_proportional_erasure_burden).
narrative_ontology:cs_axiom_status(data_controller_bears_proportional_erasure_burden, overridden).
narrative_ontology:cs_axiom_grounding('90192699-4e7e-4aeb-bd57-6de29761ded2', data_controller_bears_proportional_erasure_burden, empirically_contingent).
narrative_ontology:cs_axiom('90192699-4e7e-4aeb-bd57-6de29761ded2', foundational, erasure_technically_achievable_at_scale).
narrative_ontology:cs_axiom_status(erasure_technically_achievable_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('90192699-4e7e-4aeb-bd57-6de29761ded2', erasure_technically_achievable_at_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('90192699-4e7e-4aeb-bd57-6de29761ded2', data_protection_fundamental_right).
narrative_ontology:cs_drift_state('90192699-4e7e-4aeb-bd57-6de29761ded2', contemporary_market_concentration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('90192699-4e7e-4aeb-bd57-6de29761ded2', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, incumbent_platform_operators).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, established_data_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, new_entrants_and_challengers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, data_subject_effective_erasure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW MARKET ENTRANT (SNARE) — A startup or alternative platform attempting to compete in data services faces erasure compliance costs that incumbents have already amortized across user bases. No exit option: operating in EU means Article 17 compliance; non-compliance means enforced shutdown. Trapped by regulatory structure, not by contract. Experiences maximum extraction: compliance burden does not reflect platform scale, making per-user cost prohibitive for small competitors. Organizational power insufficient to absorb cost or negotiate exemption.
constraint_indexing:constraint_classification(article17_erasure_right__competitive_moat_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-MARKET DATA CONTROLLER (TANGLED ROPE) — A regional platform or data-intensive business has legitimate coordination needs (user privacy rights) aligned with Article 17's intent, but bears asymmetric compliance costs. Can survive compliance but at significant operational burden. Exit is costly (relocate outside EU, restructure business model, lobby for exemption) but theoretically possible. Coordination benefit genuine (users genuinely need erasure capability); extraction overlay significant (compliance infrastructure cost favors scale). Moderate power allows limited negotiation or technical innovation to reduce burden.
constraint_indexing:constraint_classification(article17_erasure_right__competitive_moat_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PLATFORM OPERATOR (ROPE) — Meta, Google, Microsoft have already built erasure infrastructure at scale; Article 17 merely formalizes their operational necessity. Compliance cost is amortized across billions of users. For these actors, the constraint functions as pure coordination: centralizing erasure requirements across EU eliminates fragmented national compliance regimes, and the regulatory clarity enables arbitrage (competing on privacy instead of haggling with fragmented regulators). Institutional power and arbitrage optionality (can influence enforcement interpretation, negotiate compliance standards) mean extraction flows toward them, not away.
constraint_indexing:constraint_classification(article17_erasure_right__competitive_moat_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DATA PROTECTION AUTHORITY (SCAFFOLD) — GDPR enforcement regime sees Article 17 as a temporary coordination mechanism for a generational transition: data minimization practices (collecting less sensitive data) and encryption-based architectures (where erasure becomes cryptographic key rotation, not record deletion) are making the constraint obsolete. As privacy-by-design and federated data architectures mature (estimated 15-20 years), the compliance burden asymmetry disappears because the underlying data retention itself becomes architecturally implausible. Sunset logic is structural: the constraint solves a transitional problem (data hoarding) that better architecture prevents.
constraint_indexing:constraint_classification(article17_erasure_right__competitive_moat_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: COMPLIANCE-INDUSTRIAL COMPLEX (PITON) — Legal and compliance consultancies, forensic data specialists, and erasure-verification vendors have built business models around Article 17 compliance. The constraint persists partly due to institutional inertia: the complexity has generated a constituency (lawyers, compliance officers, auditors) whose professional identity depends on the constraint existing. Measured functional benefit of Article 17 (actual data erasure) is moderate (theater ratio 0.45: many 'erasure' events are logical deletion, not cryptographic destruction or physical media erasure). But the compliance theater persists and generates revenue. Theater ratio reflects that compliance activity is only partially tied to actual user erasure outcomes.
constraint_indexing:constraint_classification(article17_erasure_right__competitive_moat_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DATA SUBJECT (TANGLED ROPE) — The nominal beneficiary (data subject with right to erasure) experiences coordination benefit (genuine ability to request deletion) but partial extraction (erasure is often slow, incomplete, or ineffectual due to data replication, vendor dependencies, or technical obstacles). Suppression is moderate: legally guaranteed right (low suppression from direct barriers) but technical and administrative friction (medium suppression from implementation gaps) means exercise is costly (time, legal expense, non-response from controllers). Benefits from coordination; bears cost of asymmetric implementation quality. Constrained exit: right is legally guaranteed but enforcement is difficult.
constraint_indexing:constraint_classification(article17_erasure_right__competitive_moat_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scope, data erasure is a structural problem with no perfect solution: once data replicates across systems, logs, backups, and third-party processors, true erasure becomes computationally intractable at scale. This perspective sees Article 17 compliance burden as inherent to the problem (natural law of information systems), not as contingent institutional arrangement. However, structural data contradicts this mountain classification: the burden asymmetry (negligible for incumbents with amortized infrastructure, prohibitive for challengers) is not inherent to data architecture but to capital requirements. False summit risk: naturalizing capital barriers as technical inevitability.
constraint_indexing:constraint_classification(article17_erasure_right__competitive_moat_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__competitive_moat_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article17_erasure_right__competitive_moat_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article17_erasure_right__competitive_moat_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, TR),
    TR >= 0.70.

:- end_tests(article17_erasure_right__competitive_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Compliance costs for erasure infrastructure are substantial and scale with data retention complexity (full history, backups, vendor dependencies, jurisdictional fragments) not with user count. Incumbents have amortized these costs across billions of users (per-user cost → negligible); new entrants face fixed costs regardless of scale. The extraction is not maximal (snare-level 0.66+) because: (a) genuine coordination benefit exists (user privacy rights genuinely matter and Article 17 secures them); (b) escape routes exist (relocate outside EU, adopt privacy-by-design, outsource to specialized vendors); (c) mid-market actors can survive compliance. But extraction is substantial enough to function as barrier-to-entry. Suppression (0.62): Moderate-high. Regulatory requirement (non-compliant services banned in EU market); technical intractability (no technical workaround exists); compliance costs scale unfavorably for challengers. Not total suppression (actors can pay to comply) but significant. Theater ratio (0.45): Moderate-low. Compliance activity is partly performative: many 'erasure' operations are logical deletion (de-indexing) rather than cryptographic destruction; deletion from primary systems but not from replicated data, logs, or vendor systems; formal acknowledgment without meaningful remediation. But the theater is not dominant (piton-level 0.70+) because genuine erasure coordination benefits exist and enforcement actions do compel some technical improvements. The rising trajectory (0.38 → 0.47) reflects that enforcement has clarified interpretations, making compliance less theater and more substantive, but residual gaps remain.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces perspectival divergence on the fundamental nature of Article 17. Incumbent platforms see coordination (Rope) — erasure is already their operational necessity; the constraint merely standardizes it across jurisdictions. New entrants see extraction (Snare) — they face binary choice (comply at unsustainable cost or exit market). Mid-market actors see hybrid (Tangled Rope) — genuine coordination benefit but asymmetric burden. Data subjects see partial realization (Tangled Rope) — nominal rights exist but actual erasure effectiveness is contingent on vendor cooperation and technical complexity. Data protection authorities see transitional problem with architectural solution (Scaffold) — privacy-by-design reduces the constraint to irrelevance over 15-20 years. Compliance vendors see persistent function (Piton) — theater ratio 0.45 means 55% of compliance activity is substantive, justifying continued institutional dependency. The analytical observer risks false-summit classification (Mountain) if data erasure impossibility is naturalized as immutable law rather than recognized as contingent on centralized data retention. The perspectival gap reveals that Article 17 is not primarily about privacy (that would be privacy_fundamental_reading) but about infrastructure gatekeeping (competitive_moat_reading): the right itself is real and protective, but the compliance mechanism creates an incidental competitive barrier.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural position relative to compliance costs. Incumbents (beneficiary + arbitrage) experience d ≈ 0.10 (net beneficiary, structural d derives to near-zero, f(d) → negative, extraction flows toward them). New entrants (victim + trapped) experience d ≈ 0.92 (net target, structural d near-unity, f(d) → high, extraction flows away from them). Mid-market actors (victim + constrained) experience d ≈ 0.65 (moderate target, f(d) moderate), experiencing the tangled rope authentically: genuine coordination benefit, but asymmetric burden. Data subjects (beneficiary in nominal right, victim in actual erasure quality) experience d ≈ 0.55 (symmetric or slightly victim-favoring depending on enforcement effectiveness). The analytical observer (d ≈ 0.73) sees the structure without being embedded in it. Scope modifier σ(S): European scope (continental → 1.1) amplifies effective extractiveness relative to local constraint; barrier-to-entry effects are strongest at EU scale where market is most valuable.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    erasure_technical_feasibility_threshold,
    'What proportion of user data must be actually deleted (not merely de-indexed or logically marked) to constitute legitimate Article 17 compliance?',
    'Forensic audit of major platforms'' ''erasure'' procedures; measurement of residual data recovery capability after reported deletion; cryptographic audit of whether deletion is cryptographic destruction or logical removal',
    'If threshold is logical deletion: compliance cost is low and asymmetry disappears (encryption-key rotation suffices). If threshold is physical/cryptographic destruction: compliance cost is high and asymmetry persists (replication across systems makes destruction intractable without coordinated vendor action).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(erasure_technical_feasibility_threshold, empirical, 'Technical threshold for what constitutes actual vs. performative erasure').

omega_variable(
    compliance_cost_attribution_boundary,
    'Should Article 17 compliance costs be attributed to the primary platform, to all downstream processors and vendors, or shared between them?',
    'Analysis of GDPR enforcement actions and ICO/CNIL guidance on processor liability; measurement of actual compliance cost by organization size and type; comparison of vendor vs. controller expenditure on erasure infrastructure',
    'If costs are borne by controllers alone: competitive asymmetry is severe (moat effect). If costs are shared with processors: asymmetry is mitigated (challengers can outsource erasure to specialized vendors). If burden is on processors: processor consolidation occurs (creating different bottleneck structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_attribution_boundary, conceptual, 'Where legal and economic burden of Article 17 compliance falls').

omega_variable(
    alternative_architecture_feasibility,
    'Can privacy-by-design architectures (federated data, client-side processing, deletion-native storage) reduce Article 17 compliance costs sufficiently to eliminate the competitive asymmetry?',
    'Technical audit of privacy-by-design architectures deployed in real services; measurement of infrastructure cost reduction relative to centralized platforms; timeline projection for industry-wide adoption',
    'If feasible and rapid: scaffold perspective confirmed — constraint has genuine sunset logic. If technically impossible or requiring 30+ years: barrier persists indefinitely and competitive moat becomes structural rather than transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_architecture_feasibility, empirical, 'Whether architectural alternatives can obsolete Article 17 asymmetry').

omega_variable(
    reading_classification_ambiguity,
    'Is Article 17 fundamentally a privacy protection mechanism (privacy_fundamental_reading) or a data deletion infrastructure requirement (competitive_moat_reading) or a censorship-enabling tool (censorship_mechanism_reading)?',
    'Comparative analysis of empirical outcomes across the three readings: Does data protection authority enforcement prioritize user erasure rights (privacy) or platform compliance (moat) or government erasure requests (censorship)? Measure enforcement action distribution across these dimensions.',
    'This reading instantiates the competitive_moat_reading: Article 17 creates infrastructure capabilities that incidentally protect privacy but primarily function as barrier-to-entry. If empirical enforcement shows privacy priority (90%+ of actions granted to individuals), reading shifts toward privacy_fundamental_reading. If empirical enforcement shows government erasure requests dominating, reading shifts toward censorship_mechanism_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_classification_ambiguity, empirical, 'Which kernel reading (privacy vs moat vs censorship) is supported by actual enforcement patterns').

omega_variable(
    axiom_proportionality_status,
    'Is the axiom ''data_controller_bears_proportional_compliance_burden'' still holdable given empirical evidence of order-of-magnitude cost asymmetries, or has European jurisprudence overridden it?',
    'Review of CJEU and national court decisions on Article 17 proportionality; survey of regulatory guidance on scale-adjusted compliance expectations; measurement of actual compliance cost per data subject by platform size',
    'If holdable: competitive asymmetry is a policy failure within the current framework, not a structural feature of Article 17 itself. If overridden: the framework acknowledges asymmetry and may trigger cascade effects on other data protection provisions. If foreclosed: would require legislative amendment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_proportionality_status, empirical, 'Whether proportionality axiom in Article 17 remains operative or has been overridden by jurisprudence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a17_moat_tr_t0, article17_erasure_right__competitive_moat_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(a17_moat_tr_t3, article17_erasure_right__competitive_moat_reading, theater_ratio, 3, 0.41).
narrative_ontology:measurement(a17_moat_tr_t6, article17_erasure_right__competitive_moat_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(a17_moat_tr_t9, article17_erasure_right__competitive_moat_reading, theater_ratio, 9, 0.47).

% Extraction over time
narrative_ontology:measurement(a17_moat_be_t0, article17_erasure_right__competitive_moat_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(a17_moat_be_t3, article17_erasure_right__competitive_moat_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(a17_moat_be_t6, article17_erasure_right__competitive_moat_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(a17_moat_be_t9, article17_erasure_right__competitive_moat_reading, base_extractiveness, 9, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(a17_moat_su_t0, article17_erasure_right__competitive_moat_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(a17_moat_su_t6, article17_erasure_right__competitive_moat_reading, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__competitive_moat_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, gdpr_data_subject_rights__market_concentration).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, compliance_cost_moat__fintech_access).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, data_minimization_incentives__architectural_lock_in).

% DUAL FORMULATION NOTE:
% Article 17 is part of a constraint family in data protection law. The privacy_fundamental_reading would have lower ε (genuine coordination benefit without extraction overlay) and different beneficiaries (data subjects primary). The censorship_mechanism_reading would have different directionality (government as beneficiary, speech as victim). These are structurally distinct constraints sharing the same legal text. Each story in the family should link to its siblings via network.affects_constraints and document the reading decomposition in commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__competitive_moat_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
