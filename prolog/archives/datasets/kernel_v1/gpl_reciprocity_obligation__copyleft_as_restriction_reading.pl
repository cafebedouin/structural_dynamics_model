% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity as Business Model Restriction (Copyleft-as-Restriction Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL reciprocity obligation presents a contested institutional choice:
 *   whether mandatory source code disclosure for derivative works is a
 *   liberation mechanism protecting the commons, a commons-building
 *   coordination tool aligning vendor incentives with shared sustainability,
 *   or a restriction preventing proprietary integration and innovation. This
 *   constraint is ONE READING of the contested kernel
 *   gpl_reciprocity_obligation. The reading instantiated here is
 *   copyleft_as_restriction_reading, which characterizes GPL reciprocity as a
 *   snare: a mechanism that benefits commons maintainers
 *   (institutional/arbitrage) while constraining proprietary vendors
 *   (powerful/constrained) through legal suppression of exit options. Sibling
 *   readings (copyleft_as_freedom_reading, copyleft_as_commons_reading)
 *   present structurally different beneficiary/victim relationships and claim
 *   different coordination functions. This reading does not deny that
 *   coordination benefits exist; it asserts that those benefits accrue
 *   asymmetrically to the commons at vendor expense, making the constraint
 *   extractive rather than purely coordinative. The measurement trajectory
 *   shows rising extractiveness (0.35→0.62) and rising suppression
 *   (0.55→0.68) over the interval, tracking the maturation of GPL enforcement
 *   infrastructure (legal precedents, license-scanning tools, compliance
 *   frameworks) and the hardening of vendor workaround costs (dual-licensing
 *   overhead, closed-source layer maintenance). Theater ratio remains low
 *   (0.38) because the suppression mechanism (copyright law and license
 *   agreements) is legible and directly enforced, not ritualized. The
 *   constraint violates the false-summit gates: GPL reciprocity is not
 *   natural law (it is a specific licensing choice), not universally
 *   immutable (other licensing regimes exist and work), not enforced through
 *   physical impossibility (only through legal consequence). From the
 *   analytical observer's perspective, presenting GPL as natural law would be
 *   a false summit — the engine should identify this during compilation.
 *
 * KEY AGENTS:
 *   - GPL Commons Maintainers: Primary beneficiary (institutional/arbitrage) — receive mandatory source contributions from vendors integrating GPL code; coordinate vendor behavior toward commons sustainability
 *   - Proprietary Software Vendors: Primary victim (powerful/constrained) — face binary choice (reciprocate or rewrite); high cost of circumvention or exit; legal suppression prevents proprietary integration
 *   - Closed-Source Derivative Creators: Secondary victim (powerful/constrained) — specifically prohibited from creating proprietary versions; bear full suppression of licensing alternatives
 *   - Vendor Developer Community: Secondary actor (organized/mobile) — building alternative business models (dual licensing, permissive licensed alternatives, source-available models) that circumvent GPL constraints
 *   - Corporate Legal Compliance Functions: Institutional actor (institutional/arbitrage) — maintain ritualized GPL compliance monitoring that degrades into theater as organizations build proprietary workarounds
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing GPL as immutable law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.62).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.68).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity as Business Model Restriction (Copyleft-as-Restriction Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '38415316-f75b-44f9-9d1c-9810d06a9363').
narrative_ontology:cs_kernel_codification('38415316-f75b-44f9-9d1c-9810d06a9363', formalized).
narrative_ontology:cs_authority_grounding('38415316-f75b-44f9-9d1c-9810d06a9363', extraction).
narrative_ontology:cs_interpretation_layer_present('38415316-f75b-44f9-9d1c-9810d06a9363').
narrative_ontology:cs_reading_relation('38415316-f75b-44f9-9d1c-9810d06a9363', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('38415316-f75b-44f9-9d1c-9810d06a9363', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('38415316-f75b-44f9-9d1c-9810d06a9363', foundational, proprietary_model_legitimate_extraction_target).
narrative_ontology:cs_axiom_status(proprietary_model_legitimate_extraction_target, holdable).
narrative_ontology:cs_axiom_grounding('38415316-f75b-44f9-9d1c-9810d06a9363', proprietary_model_legitimate_extraction_target, deontological).
narrative_ontology:cs_axiom('38415316-f75b-44f9-9d1c-9810d06a9363', foundational, commons_benefit_asymmetric_to_vendor_cost).
narrative_ontology:cs_axiom_status(commons_benefit_asymmetric_to_vendor_cost, holdable).
narrative_ontology:cs_axiom_grounding('38415316-f75b-44f9-9d1c-9810d06a9363', commons_benefit_asymmetric_to_vendor_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('38415316-f75b-44f9-9d1c-9810d06a9363', proprietary_vendor_business_model_autonomy).
narrative_ontology:cs_drift_state('38415316-f75b-44f9-9d1c-9810d06a9363', contemporary_open_source_infrastructure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('38415316-f75b-44f9-9d1c-9810d06a9363', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_commons_maintainers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, closed_source_derivative_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPRIETARY VENDOR (SNARE) — Faces binary choice: either use GPL code and forfeit proprietary differentiation through mandatory reciprocity, or avoid GPL entirely and duplicate engineering effort. The license enforces exit cost so high that many vendors are functionally trapped between rewriting or abandoning product lines. High suppression: no legal avenue to benefit from GPL code while maintaining closed source; vendor has power but license removes exit options. Effective extraction: GPL code is valuable, but reciprocity obligation extracts the vendor's proprietary model as the price of access.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: GPL COMMONS MAINTAINERS (ROPE) — Benefit from mandatory reciprocity: when vendors integrate GPL code, the commons gains access to improvements. The obligation aligns vendor incentives with commons sustainability. From the commons' perspective, the mechanism solves a coordination problem: how to prevent free-riding on contributions. Low effective extraction from the commons' position — the mechanism funds commons maintenance through vendor contribution. Arbitrage exit: commons can fork if unhappy; license is enforced through copyright, not external force.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_restriction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: VENDOR DEVELOPER COMMUNITY (SCAFFOLD) — Organized agents (companies investing in permissive-licensed alternatives, dual-licensing services, proprietary middleware layers) see GPL reciprocity as a temporary institutional constraint that is being circumvented through alternative business models (dual licensing, tiered licensing, source-available models). Low effective extraction because these agents perceive and execute exit strategies. Theater is moderate: the constraint persists through copyright law, not through market force; alternatives make the GPL restriction functionally obsolete for vendors willing to pay licensing fees or build closed-source layers atop open components.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_restriction_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal view, GPL reciprocity is presented as an immutable property of copyright law itself: if you use copyrighted code, you cannot legally impose different licenses. This perspective naturalizes the constraint as an inherent feature of intellectual property law, not a contingent institutional choice. However, the structural data contradicts this: GPL is one among many license choices; vendors are not physically prevented from using other licenses; reciprocity is enforced through legal action (contingent), not physical law (immutable). The engine will identify this as a false summit, revealing how copyright naturalism masks a deliberate choice to enable the commons at vendor expense.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_restriction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: CORPORATE LEGAL COMPLIANCE (PITON) — Within large organizations, GPL compliance functions have become ritualized: license scanning tools, compliance audits, and approved-software lists persist as institutional theater even when business units actively work around them through closed-source subsidiaries, licensing fees, or proprietary middleware. The compliance function is performative — it produces the appearance of GPL avoidance while the organization continues integrating GPL code through various circumvention pathways. Theater ratio is high; functional verification of GPL compliance is rare. The constraint degrades from snare (functionally binding) to piton (institutionally inert) at the organizational level because the enforcement mechanism (legal liability) is disconnected from actual business practice.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_restriction_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: SMALL OPEN SOURCE-FRIENDLY VENDOR (TANGLED ROPE) — Companies that embrace rather than resist GPL reciprocity (Red Hat, Canonical, etc.) experience the constraint as mixed coordination and extraction. GPL reciprocity solves coordination among vendors who want to contribute improvements (rope function) while simultaneously extracting their proprietary service model (support, hosting, integration services rather than licensed code). They have constrained mobility — they could close source, but it would damage their brand and community relationships. Effective extraction is moderate: the license creates asymmetric benefit (the vendor's brand builds on commons trust while they monetize services), but genuine coordination emerges (vendor contributions improve commons).
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, TR),
    TR >= 0.70.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high. GPL reciprocity mandates source disclosure, preventing proprietary differentiation on integrated code. However, the extraction is not total (0.72+) because vendors can (with cost) circumvent through rewrites, dual licensing, proprietary wrappers, or alternative license selection. The rising trajectory (0.35→0.62) reflects strengthening enforcement infrastructure and rising vendor awareness that circumvention is costly. Suppression (0.68): High. Vendors face legal prohibition (not mere economic cost) on proprietary integration. The mechanism is copyright law + license enforcement, not market pressure or negotiated constraint. Rising trajectory reflects maturation of GPL enforcement (legal precedents, SPDX standards, automated license scanning). Theater (0.38): Low-to-moderate. GPL compliance is legible (source code is either available or not) and directly enforced (through copyright litigation), not hidden or ritualized. Low theater is appropriate for legal/contractual constraints, even when disputed. The slight rise (0.25→0.38) reflects increasing corporate ritualization (compliance functions) without corresponding increase in functional verification.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces maximal perspectival divergence: the same GPL reciprocity obligation appears as a snare to proprietary vendors (extraction, suppression, no exit), a rope to the commons (coordination, mutual benefit, low extraction), a mountain to the analytical observer (presented as natural law but structurally contingent), a piton to corporate compliance functions (ritualized without functional verification), a scaffold to vendor communities building alternatives (temporary, solvable), and tangled rope to open-source-friendly vendors (mixed coordination and extraction with constrained mobility). The perspectival gap reveals that 'GPL reciprocity' does not have a univocal classification — it is structurally different depending on who bears the suppression and who benefits from the coordination. This reading privileges the vendor-constraint perspective, naming GPL as a snare and the commons as the beneficiary. Sibling readings would privilege the commons-sustainability or freedom-maximization perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (gpl_commons_maintainers, institutional/arbitrage exit) experiences low effective extraction χ because their structural position gives them exit options (fork, relicense, negotiate) and benefits from the mechanism (mandatory contributions). Directionality d is low (~0.15-0.20), f(d) is negative or near-zero, yielding χ ≈ ε × f(d) × σ(S) with small absolute value. The victims (proprietary_software_vendors, powerful/constrained exit) experience high effective extraction because they bear suppression (copyright law prevents circumvention) and lack arbitrage exits (constrained rather than mobile). Directionality d is high (~0.75-0.85), f(d) is strongly positive (1.1-1.3), yielding χ ≈ 0.62 × 1.2 × 1.0 ≈ 0.74 at global scope. The powerful agent (vendors have market power and technical capability) is materially constrained by law (exit_options: constrained, not trapped) — they could rewrite or challenge in court, but the cost is high. This accounts for why the vendor perspective is snare rather than mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading RESOLVES mandatrophy by naming the constraint as a snare from the vendor perspective and a rope from the commons perspective. The classification depends on the reading: copyleft_as_restriction_reading frames the commons benefit as asymmetric extraction (snare+rope duality), while copyleft_as_freedom_reading would frame the commons benefit as enabling vendor freedom to innovate cooperatively (rope or scaffold), and copyleft_as_commons_reading would frame it as commons preservation (tangled_rope with justified asymmetry). Mandatrophy is resolved by acknowledging the multiple readings are incommensurable at the axiom level: they disagree on what 'freedom,' 'benefit,' and 'fairness' mean. The engine should report all three readings simultaneously as a presheaf over the kernel rather than attempting to select one 'correct' classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copyright_vs_natural_constraint,
    'Is GPL reciprocity an inherent immutable property of copyright law, or a contingent institutional choice to enable the commons?',
    'Historical analysis of copyright law alternatives (public domain, permissive licenses, source-available models, patent licensing regimes) demonstrating that reciprocity is one among many enforceable choices. Patent systems achieve different distribution of benefits through different licensing structures (e.g., copyleft patents vs. grant-back clauses vs. royalty-free pools) with equivalent legal force.',
    'If immutable: GPL is a mountain (beneficiary declarations are spurious, false summit detection should not fire). If contingent: GPL is a snare from the proprietary vendor perspective; beneficiary declarations are structural and FSM evaluation applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyright_vs_natural_constraint, empirical, 'Whether GPL reciprocity obligation is immutable law or contingent institutional choice').

omega_variable(
    derivative_work_boundary_contestation,
    'What constitutes a ''derivative work'' under GPL semantics? Is dynamically linked software, proprietary middleware, or closed-source wrapper code a derivative subject to reciprocity?',
    'Litigation outcomes (GPL enforcement actions), legal scholarship consensus on linking standards, SPDX definition authority, and empirical analysis of actual GPL enforcement patterns (which works are pursued, which are ignored). Comparison across jurisdictions (EU, US, China) showing that ''derivative'' is legally contestable.',
    'If derivative boundary is strict (narrow): GPL reciprocity is highly effective (snare suppression remains high). If boundary is porous (broad): vendors circumvent reciprocity through middleware layers (snare effectiveness drops, scaffold perspective strengthens). The engine''s suppression metric encodes what vendors perceive as enforceable — if the boundary is contested, suppression should be lower than 0.68.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_boundary_contestation, empirical, 'Definition and enforceability of ''derivative work'' under GPL').

omega_variable(
    commons_maintenance_counterfactual,
    'Without GPL reciprocity, would the commons (Linux kernel, GNU tools, etc.) have achieved comparable sustainability and vendor contribution levels?',
    'Comparative analysis of non-reciprocal open-source projects (BSD, Apache) vs. reciprocal projects (Linux, GNU) across cohorts matched on funding, initial contributor base, and commercial interest. Causal inference from projects that switched license (XFree86 → X.Org, from reciprocal to non-reciprocal; Harmony DB → commons, from proprietary to reciprocal) showing difference in vendor contribution patterns.',
    'If GPL reciprocity is necessary for commons sustainability: the constraint solves a genuine coordination problem (rope functional identification is correct; snare classification from vendor perspective is appropriate asymmetry, not unjust extraction). If sustainability is achievable without reciprocity: GPL reciprocity is extracting vendor value for purposes unrelated to commons health (snare classification is strengthened, rope functional identification is weaker).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_maintenance_counterfactual, empirical, 'Whether GPL reciprocity is necessary for commons maintenance and vendor contribution').

omega_variable(
    reading_contest_epistemic_status,
    'What is the epistemic and normative status of the competing readings (copyleft_as_freedom, copyleft_as_commons, copyleft_as_restriction)? Are they genuinely incommensurable, or can evidence resolve which is correct?',
    'Conceptual analysis of what constitutes ''freedom'' in software licensing (freedom for whom? from what constraints?). Historical analysis of GPL author intent (Stallman''s conception of freedom). Comparative framing analysis across reading communities (Free Software Foundation, Open Source Initiative, Proprietary Software Alliance) identifying what axioms each treats as foundational vs. negotiable.',
    'If readings are conceptually incommensurable (different axioms about what ''freedom'' or ''commons'' means): all three readings are hold-able simultaneously; they coexist_with each other. If one reading''s axioms are empirically falsifiable or conceptually incoherent: that reading may be foreclosed by evidence or logic. This omega determines the reading_relations field in cs_structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_epistemic_status, conceptual, 'Epistemic and normative status of competing GPL readings; whether evidence can resolve or readings are incommensurable').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.68) primarily legal (vendors cannot legally circumvent GPL) or practical (vendors lack economic incentive to challenge GPL in courts)?',
    'Analysis of GPL enforcement patterns: how many license violations are prosecuted vs. how many are technically possible but not pursued? Survey of vendor decision-making: do vendors avoid GPL code because legal risk is unacceptable, or because costs of legal defense or workarounds are lower than integration benefits? Comparison of GPL enforcement rates across jurisdictions (countries with weak IP enforcement show similar vendor behavior patterns, suggesting practical barriers dominate legal ones).',
    'If suppression is primarily legal: vendors are genuinely trapped (snare classification accurate). If suppression is primarily practical/economic: vendors have de facto exit options (constrained rather than trapped); the snare classification should be downgraded to tangled_rope; effective extraction χ declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of GPL circumvention is legal or practical/economic in nature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_restr_tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gpl_restr_tr_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(gpl_restr_tr_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(gpl_restr_be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gpl_restr_be_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(gpl_restr_be_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gpl_restr_su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gpl_restr_su_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(gpl_restr_su_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_license_enforcement).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, software_patent_licensing_regimes).

% DUAL FORMULATION NOTE:
% The GPL reciprocity obligation constraint exists as a kernel with three main readings: copyleft_as_restriction (this file), copyleft_as_freedom (sibling), and copyleft_as_commons (sibling). Each reading instantiates different beneficiary/victim structures and different ε values. This reading (copyleft_as_restriction) characterizes the constraint as a snare (ε=0.62) with the commons as beneficiary and vendors as victims. Sibling readings will have different ε values reflecting different observables (vendor innovation rate, commons contributor motivation, user freedom metrics). These are NOT three observations of the same constraint — they are three structurally distinct constraints sharing a kernel commitment. Link them as network.affects_constraints to enable the engine to recognize the family and route analyses accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
