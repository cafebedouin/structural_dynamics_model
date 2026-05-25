% ============================================================================
% CONSTRAINT STORY: liability_shield_beta_label
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_shield_beta_label, []).

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
 *   constraint_id: liability_shield_beta_label
 *   human_readable: Liability Shield via Beta Label Disclaimer
 *   domain: legal/product_governance
 *
 * SUMMARY:
 *   The beta label operates as a legal and institutional mechanism that
 *   shields product developers from liability while deploying products to
 *   real users who bear the risks of unvalidated systems. This constraint
 *   exhibits structural characteristics of both coordination (enabling staged
 *   testing and rapid iteration) and extraction (asymmetric risk allocation
 *   with minimal user agency). The beta label is simultaneously justified as
 *   necessary for innovation, used as a permanent escape hatch by developers,
 *   and experienced by users as involuntary assumption of risk. The
 *   constraint's extractiveness has increased over the measurement interval
 *   as the label has become standardized practice across industries (consumer
 *   software, cloud services, cryptocurrency, autonomous systems, medical
 *   devices in research contexts), while theater ratio has increased as the
 *   legal fiction of 'informed beta participation' has become more
 *   performative and less connected to actual user understanding or
 *   meaningful consent.
 *
 * KEY AGENTS:
 *   - Product Developer: Primary beneficiary (institutional/arbitrage) — captures coordination benefits (rapid testing, liability shield, deployment speed) with full exit flexibility via transition to general availability
 *   - Early Adopter User: Primary victim (powerless/trapped) — bears full risk of failures; lacks meaningful understanding of specific hazards; cannot exit without abandoning service access
 *   - Regulatory Oversight Body: Secondary actor (moderate/constrained) — experiences mixed coordination (manages innovation-stability tradeoff) and extraction (liability waived without verification); politically constrained against blocking innovation
 *   - Consumer Protection Movement: Organized actor (organized/constrained) — advocates for graduated liability, safety-by-design standards, and mandatory disclosures; building sunset pathways
 *   - Enterprise Adopter: Secondary beneficiary (powerful/mobile) — negotiates custom terms, accesses pre-release through direct relationships; benefits from coordination while maintaining agency
 *   - Caveat Emptor Doctrine: Institutional substrate (institutional/arbitrage) — persists through inertia; primary mechanism for legal shield despite erosion by modern consumer protection law
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as necessary feature of technological innovation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_shield_beta_label, 0.58).
domain_priors:suppression_score(liability_shield_beta_label, 0.68).
domain_priors:theater_ratio(liability_shield_beta_label, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_shield_beta_label, extractiveness, 0.58).
narrative_ontology:constraint_metric(liability_shield_beta_label, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(liability_shield_beta_label, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_shield_beta_label, tangled_rope).
narrative_ontology:human_readable(liability_shield_beta_label, "Liability Shield via Beta Label Disclaimer").
narrative_ontology:topic_domain(liability_shield_beta_label, "legal/product_governance").

domain_priors:requires_active_enforcement(liability_shield_beta_label).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_shield_beta_label, product_developer).
narrative_ontology:constraint_beneficiary(liability_shield_beta_label, service_provider).
narrative_ontology:constraint_victim(liability_shield_beta_label, early_adopter_users).
narrative_ontology:constraint_victim(liability_shield_beta_label, regulatory_oversight_bodies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY ADOPTER USER (SNARE) — Trapped by information asymmetry and lack of meaningful alternatives. The 'beta' label provides no actionable insight; they cannot exit without abandoning access to the service entirely. They bear the full risk of undisclosed defects, data loss, privacy violations, and safety failures. Maximum experienced extraction with minimal escape routes.
constraint_indexing:constraint_classification(liability_shield_beta_label, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATORY OVERSIGHT BODY (TANGLED ROPE) — Experiences genuine coordination (need for controlled deployment of novel technologies) alongside asymmetric extraction (liability waived without verification). The beta label creates a coordination mechanism for staged rollout but enables developers to bypass safety validation. Constrained by political pressure to avoid stifling innovation while bearing institutional accountability for harms.
constraint_indexing:constraint_classification(liability_shield_beta_label, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRODUCT DEVELOPER (ROPE) — The beneficiary. Experiences the constraint as pure coordination: the beta label solves the collective action problem of testing at scale without legal exposure. They can exit the beta label relationship by transitioning to general availability at will. Net beneficiary with high structural flexibility.
constraint_indexing:constraint_classification(liability_shield_beta_label, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER PROTECTION MOVEMENT (SCAFFOLD) — Organized agents (consumer advocates, legislative bodies, tort reform coalitions) perceive the beta label as a temporary institutional accommodation with sunset logic. Safety-by-design regulation, compulsory liability insurance for beta features, and mandatory disclosure standards create exit pathways. The constraint is reframed from 'shield against all liability' to 'graduated liability matching risk level.' Sunset trajectory: 5-10 years as regulatory frameworks mature.
constraint_indexing:constraint_classification(liability_shield_beta_label, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CAVEAT EMPTOR DOCTRINE (PITON) — The underlying legal principle (buyer beware) persists through institutional inertia despite reduced functional application. Modern consumer protection law has eroded caveat emptor's coverage, yet the beta label operates as though the doctrine remains dominant. The theatrical invocation of 'beta' maintains the legal fiction that users are informed market participants making rational trade-offs. Theater ratio high because the label's function is largely performative — it signals legal status without meaningfully changing user knowledge or actual risk allocation.
constraint_indexing:constraint_classification(liability_shield_beta_label, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENTERPRISE ADOPTER (TANGLED ROPE) — Large organizations with legal resources and mobility can negotiate custom liability agreements, access pre-release products through direct relationships, and exit the standard 'beta user' constraint. However, they also benefit from the constraint's coordination function — graduated deployment reduces costs and enables parallel testing across organizational infrastructure. Mixed extraction: they bear some risk but have agency and meaningful alternatives.
constraint_indexing:constraint_classification(liability_shield_beta_label, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — Risks classifying the beta label as a natural law: 'All new technology requires a testing phase where some users bear risk.' This naturalizes the institutional arrangement (liability shield via label) as an immutable feature of innovation. The analytical view is that some verification lag is necessary, but the structural data contradicts mountain classification — the extraction mechanisms (information asymmetry, legal shield, regulatory capture) are contingent institutions, not laws of nature. Engine identifies as false summit.
constraint_indexing:constraint_classification(liability_shield_beta_label, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_shield_beta_label_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liability_shield_beta_label, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liability_shield_beta_label, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_shield_beta_label, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liability_shield_beta_label, TR),
    TR >= 0.70.

:- end_tests(liability_shield_beta_label_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting the asymmetric risk allocation that increases over time as beta deployment scales. The value reflects that developers capture significant value (liability shield, rapid iteration, market entry) while users bear material risks. The trajectory from 0.35 to 0.58 over 15 years shows increasing normalization of the practice — earlier betas were genuinely experimental and temporary; modern betas often represent permanent product strategy (see: social media platforms, enterprise software, mobile OS releases). Suppression (0.68): High. Users face multiple barriers to exit: information asymmetry (cannot assess actual risks from a 'beta' label), switching costs (invested in the service, data, workflows), psychological entrenchment (early adopters identify as 'beta testers'), and lack of functionally equivalent alternatives for novel capabilities. Theater ratio (0.65): Moderate-high and increasing. The 'beta' label is substantially performative — it signals legal status without conveying actual risk information. Regulators accept it as satisfying disclosure requirements despite evidence that users do not understand what 'beta' means in practice. As betas have proliferated, the label's informational content has declined while its legal function has strengthened.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap between developer (rope/coordination) and user (snare/extraction) is fundamental and represents a failure of the constraint to operate symmetrically. The developer genuinely solves a coordination problem — how to test at scale without building verification burden into the development cycle. The user genuinely experiences extraction — they bear risks they did not choose and cannot evaluate. Neither perspective is wrong; both are structurally accurate from their position. The gap reveals that the constraint is not solving a pure coordination problem but rather shifting a problem — the developer's testing burden — onto users. The regulatory perspective (tangled_rope) represents an institutional actor trying to balance innovation and user protection, experiencing the beta shield as both enabling necessary staged rollout (coordination) and enabling indefinite liability avoidance (extraction). The consumer protection perspective (scaffold) sees a temporary institutional accommodation with genuine sunset logic — as regulatory frameworks mature (safety-by-design mandates, compulsory liability insurance, mandatory incident reporting), the beta label's extraction function can be constrained while preserving its coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d differs sharply across agent types. The developer experiences low d (0.10-0.15) — they are beneficiary with arbitrage exit, so their effective extraction f(d) is near-negative. They perceive the constraint as enabling coordination. The powerless early adopter experiences high d (0.92) — trapped victim with no exit; their f(d) yields maximum experienced extraction. The regulatory body, as a secondary victim constrained to cooperate, derives moderate-high d (0.65-0.75), producing moderate-high experienced extraction despite institutional power. The enterprise adopter, as a mobile beneficiary, derives moderate d (0.45-0.55), experiencing mixed coordination and extraction. The analytical observer, examining the structure from outside all these positions, derives d around 0.72 (the canonical analytical value), perceiving the constraint as neither pure coordination nor pure extraction but as a naturalizing frame that masks institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the coordination function (staged testing, rapid iteration, innovation incentives) from the extraction mechanism (liability shield enabling indefinite deferral of safety validation). The constraint can remain a rope (pure coordination) only if the beta label is genuinely temporary and transitions to accountability. It becomes tangled_rope when both coordination and extraction persist simultaneously — developers benefit from both the testing infrastructure and the liability shield. It becomes snare when the extraction dominates (liability shield becomes permanent, user risk becomes baseline assumption). The analytical observer's risk of false mountain classification — 'testing requires beta users' — naturalizes the institutional choice to allocate risk to users rather than developers. Alternatives exist: closed-beta testing with staff or recruited participants, staged rollout with mandatory liability insurance, safety-by-design regulation requiring pre-release validation. The fact that the industry chose the user-risk model reveals this is extraction, not natural law. Mandatrophy resolves by showing that the classification difference between rope and snare depends on empirical facts (is beta temporary or permanent? do users materially understand risks? can alternatives work?) rather than on framework ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informed_consent_threshold,
    'What level of disclosure constitutes genuine informed consent for users bearing beta product risk?',
    'Empirical testing: comprehension studies on beta disclaimers; correlation between user understanding and actual risk exposure; comparison with informed consent standards in medical/research contexts',
    'If current beta labels meet threshold: constraint operates as rope/coordination. If disclosure insufficient: constraint is extraction mechanism (snare/tangled_rope), shifting classification burden to developers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_threshold, empirical, 'Whether beta labels provide sufficient informed consent').

omega_variable(
    hidden_risk_materialization,
    'What fraction of beta product failures cause material harm (data loss, privacy violation, financial loss, safety incident) that would trigger liability in non-beta products?',
    'Incident tracking: categorize beta product failures by severity; compare liability exposure if same product were labeled general availability; construct counterfactual compensation claims',
    'If materialization rate > 15%: extraction is severe, snare classification appropriate. If < 5%: constraint operates closer to coordination. If concentrated on powerless users: disparity amplifies snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hidden_risk_materialization, empirical, 'Rate of material harm from beta products').

omega_variable(
    alternative_testing_pathway_feasibility,
    'Could developers conduct equivalent-quality testing using closed beta pools, staged rollout with liability insurance, or mandatory incident reporting rather than open unshielded beta deployments?',
    'Compare testing timelines, defect detection rates, and cost structures across beta strategies; assess regulatory acceptance of alternatives; survey developer preference drivers',
    'If feasible alternatives exist: beta shield becomes extractive preference rather than coordination necessity, shifting classification toward snare. If alternatives significantly more costly: shield may be legitimate coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_testing_pathway_feasibility, empirical, 'Feasibility of alternative testing pathways').

omega_variable(
    user_population_asymmetry,
    'Are early beta adopters systematically more sophisticated (able to understand risks, implement workarounds, exit if necessary) or systematically more vulnerable (price-sensitive, dependent on service, lower technical literacy)?',
    'Demographic analysis of beta user populations; skill-level assessment; dependency mapping (how critical is the service to the user''s function?); exit option availability by user class',
    'If sophisticated: powerless classification is incorrect, moderate classification appropriate. If vulnerable: extraction is asymmetric by design, snare classification strengthened. If mixed: segmentation creates distinct constraints per user class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_population_asymmetry, empirical, 'Whether beta adopters are sophisticated or vulnerable').

omega_variable(
    regulatory_capture_vs_innovation_lag,
    'Is the beta shield justified by genuine regulatory inability to assess novel technologies, or by developer capture of regulatory bodies to block alternative accountability mechanisms?',
    'Examine regulatory comment periods, expert testimony patterns, and legislative history; assess whether regulators request data that developers refuse to provide; compare jurisdictions with stricter beta liability to innovation timelines',
    'If regulatory lag: constraint is coordination (scaffold sunset toward stronger regulation). If capture: constraint is extraction (snare with institutional complicity). Classification may differ between jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_vs_innovation_lag, empirical, 'Whether regulatory asymmetry reflects lag or capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_shield_beta_label, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_shield_beta_label, theater_ratio, 0, 0.48).
narrative_ontology:measurement(liab_tr_t5, liability_shield_beta_label, theater_ratio, 5, 0.58).
narrative_ontology:measurement(liab_tr_t10, liability_shield_beta_label, theater_ratio, 10, 0.65).
narrative_ontology:measurement(liab_tr_t15, liability_shield_beta_label, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_shield_beta_label, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(liab_be_t5, liability_shield_beta_label, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(liab_be_t10, liability_shield_beta_label, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(liab_be_t15, liability_shield_beta_label, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_shield_beta_label, resource_allocation).
narrative_ontology:affects_constraint(liability_shield_beta_label, product_liability_waiver).
narrative_ontology:affects_constraint(liability_shield_beta_label, regulatory_arbitrage_testing).
narrative_ontology:affects_constraint(liability_shield_beta_label, user_data_risk_asymmetry).

% DUAL FORMULATION NOTE:
% The beta label constraint is upstream of specific product liability constraints (cryptocurrency exchange beta, autonomous vehicle testing, medical device research use). Each specific instantiation has its own extractiveness reflecting domain-specific factors, but all share the common structure: liability shield via preliminary status label. The network links capture how regulatory capture or tightening in one domain affects the viability of beta shields in adjacent domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_shield_beta_label, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
