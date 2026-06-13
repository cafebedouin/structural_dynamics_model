% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right and Commercial Legitimacy
 *   domain: political_economy/intellectual_property/software_engineering
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'software_control_legitimacy': the property-rights reading, which grounds
 *   software control in intellectual property doctrine and commercial
 *   legitimacy. Under this reading, creators have legitimate authority to
 *   restrict use, modification, and distribution to protect investment and
 *   enable commercial sustainability. The alternative readings (commons,
 *   freedom-imperative, pragmatic-openness) offer competing frames with
 *   different ε values and victim/beneficiary structures. This story models
 *   ONLY the property-rights reading as a structurally coherent constraint —
 *   generated independently per the ε-invariance principle, with its own
 *   metrics, stakeholders, and type classification. The sibling readings are
 *   other constraint stories, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - commercial_software_vendors: institutional beneficiary and agenda-setter (set and enforce IP terms, collect license revenue; arbitrage exit options)
 *   - venture_capital_investors: institutional beneficiary (IP protection enables exit strategies and valuations; no direct enforcement role)
 *   - foss_advocates: moderate-power payer/excluded (denied return on investment they seek; restricted from setting legitimacy frame; constrained exit)
 *   - downstream_integrators: powerful-power payer (pay license fees, operate under restrictions; constrained exit)
 *   - security_researchers_under_restriction: moderate-power payer with identity-locked exit (professional identity in security research bound to vendor-controlled constraints)
 *   - end_users: organized-power beneficiary/payer (receive professional software + support; bear licensing costs and lock-in)
 *   - intellectual_property_regime: non-agent beneficiary (the legal/institutional doctrine this reading vindicates and depends on)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.62).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.71).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right and Commercial Legitimacy").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "political_economy/intellectual_property/software_engineering").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'd38ea962-f092-43d2-a2ca-6734896c2044').
narrative_ontology:cs_kernel_codification('d38ea962-f092-43d2-a2ca-6734896c2044', fixed_text).
narrative_ontology:cs_authority_grounding('d38ea962-f092-43d2-a2ca-6734896c2044', extraction).
narrative_ontology:cs_interpretation_layer_present('d38ea962-f092-43d2-a2ca-6734896c2044').
narrative_ontology:cs_reading_relation('d38ea962-f092-43d2-a2ca-6734896c2044', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('d38ea962-f092-43d2-a2ca-6734896c2044', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_reading_relation('d38ea962-f092-43d2-a2ca-6734896c2044', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_axiom('d38ea962-f092-43d2-a2ca-6734896c2044', foundational, software_is_intellectual_property).
narrative_ontology:cs_axiom_status(software_is_intellectual_property, holdable).
narrative_ontology:cs_axiom_grounding('d38ea962-f092-43d2-a2ca-6734896c2044', software_is_intellectual_property, conventional).
narrative_ontology:cs_axiom('d38ea962-f092-43d2-a2ca-6734896c2044', foundational, vendor_authority_legitimate_through_property_rights).
narrative_ontology:cs_axiom_status(vendor_authority_legitimate_through_property_rights, holdable).
narrative_ontology:cs_axiom_grounding('d38ea962-f092-43d2-a2ca-6734896c2044', vendor_authority_legitimate_through_property_rights, deontological).
narrative_ontology:cs_axiom('d38ea962-f092-43d2-a2ca-6734896c2044', secondary, restriction_of_use_justified_by_investment_protection).
narrative_ontology:cs_axiom_status(restriction_of_use_justified_by_investment_protection, holdable).
narrative_ontology:cs_axiom_grounding('d38ea962-f092-43d2-a2ca-6734896c2044', restriction_of_use_justified_by_investment_protection, instrumental).
narrative_ontology:cs_reference_frame('d38ea962-f092-43d2-a2ca-6734896c2044', intellectual_property_doctrine_software_as_property).
narrative_ontology:cs_drift_state('d38ea962-f092-43d2-a2ca-6734896c2044', contemporary_open_source_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d38ea962-f092-43d2-a2ca-6734896c2044', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, commercial_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, venture_capital_investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, downstream_integrators).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, security_researchers_under_restriction).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.62) because the constraint solves a genuine problem (funding professional development) but does so by restricting freedoms that have measurable social value (code reuse, security auditing, modification for local needs). The measurement series shows extractiveness rising from 0.45 to 0.62 across the interval, driven by: (1) software becoming essential infrastructure where lock-in costs rise, (2) patent thickets in software increasing barriers to derivative work, (3) subscription models replacing one-time purchases, making the revenue extraction continuous. Suppression is higher (0.71) because enforcement requires active restriction of code access, reverse-engineering, interoperability, and license-term alternatives — vendors must actively police boundaries to maintain property claims. Theater is low-moderate (0.28) because the security/quality justification is real but an increasing share of enforcement activity defends market position rather than software quality. Accessibility-collapse (0.58) reflects that alternatives (open-source, forking, reimplementation) remain available but require substantial effort, so most users are functionally locked in even if not legally trapped. Resistance (0.72) is high because open-source communities actively contest the legitimacy of proprietary software; the constraint persists not because resistance is absent but because institutional and financial power outweighs it.
 *
 * PERSPECTIVAL GAP:
 *   The vendor/investor seats and the FOSS/researcher seats compute radically different types from the same structural data. Vendors see coordination (funding professional development, enabling liability, paying for security) and modest extraction (license fees are the cost of access to professional expertise). FOSS advocates see pure extraction (restricting freedoms that should be universal, denying return to decentralized developers, locking in users). Downstream integrators see Tangled Rope (they coordinate on standards and library ecosystems but pay asymmetrically high license fees for dependent software). Security researchers see a snare (denied access to audit code, forced to accept vendor definitions of acceptable vulnerability research, trapped by identity fusion). The engine computes per-seat classifications from the power, exit, and beneficiary/victim declarations; the perspectival gap emerges naturally from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial vendors are structural beneficiaries (d ≈ 0.05-0.15: collect license revenue, control supply, arbitrage exit means they can move resources freely). Venture investors are beneficiaries (d ≈ 0.10: benefit from IP protection, arbitrage exit, but don't directly enforce). End-users are symmetric-to-slight-beneficiary (d ≈ 0.45-0.55: receive professional software and support, but also bear lock-in and licensing costs). FOSS advocates are targets (d ≈ 0.75-0.85: denied the return model they seek, restricted from modifying, constrained exit because the constraint controls industry legitimacy). Downstream integrators are targets (d ≈ 0.65-0.75: pay license fees, operate under restrictions, constrained exit because major software stacks are proprietary). Security researchers are near-full targets (d ≈ 0.85: denied access, identity-locked so they cannot simply leave the profession, constrained by legal restrictions on disclosure). These directionalities derive directly from the beneficiary/victim declarations: vendors and investors are beneficiaries, FOSS advocates and researchers are victims; exit options vary by seat's institutional position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint sits exactly at the boundary where mandatrophy becomes contestable. The founding problem (funding professional software development) is partially but not completely solved: open-source professionals exist and build enterprise-grade software (Linux, Kubernetes, Apache), but venture-capital-funded proprietary models remain the industry's default path and generate the most developer investment. The measurement series shows extractiveness plateauing around 0.62 after t=30, suggesting the constraint has reached equilibrium: license restrictions are not intensifying further, but neither are they declining despite decades of open-source alternatives proving viable. Theater is staying low (0.28), which argues AGAINST mandatrophy on the piton mechanism (theater would need to approach 0.5-0.7 to signal atrophied function maintained theatrically). The constraint persists because: (1) the funding coordination problem is real and vendors solve it, (2) venture capital and IP law are mutually reinforcing institutions, (3) institutional beneficiaries have power to maintain it. The constraint would satisfy a mandatrophy verdict only if extractiveness kept rising (rent-seeking) or if the founding problem became demonstrably solved (which it hasn't — professional software still requires large capital). Current state: live constraint with contested mandate, not mandatrophic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_decomposition,
    'Is the software property-rights constraint primarily a solution to a genuine coordination problem (funding professional development) with extraction as a side-effect, or is the coordination function a cover story for extraction that would proceed regardless?',
    'Empirical investigation: compare venture-funded proprietary software outcomes (R&D investment, support, security quality) with professional open-source outcomes (Linux kernel development, Apache ecosystem, Kubernetes) for equivalent functionality and market maturity. If open-source achieves equivalent or superior outcomes at lower extraction cost, the coordination function is separable from the extraction mechanism.',
    'If coordination and extraction are entangled, the constraint is a genuine Tangled Rope (both functions necessary). If separable, the constraint transitions toward Snare (extraction mechanism riding on a solved coordination problem). This omega directly addresses whether the constraint persists because it solves a live problem or because institutional power maintains it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_decomposition, empirical, 'Whether IP-based coordination is necessary for professional software development or whether it is a contingent institutional choice.').

omega_variable(
    property_concept_applicability_to_software,
    'Does the property-rights metaphor accurately describe software, or does software''s copyability-without-loss and essential-infrastructure role require a different conceptual frame?',
    'Philosophical and economic analysis: compare software to physical property (where scarcity is natural) and to information/knowledge (where scarcity is created and maintained). Examine whether patent doctrine (invented to describe physical mechanisms) adequately captures software''s dynamics.',
    'If software is categorically different from physical property (no natural scarcity, economies of scale in copying), the property-rights legitimacy claim rests on a category mistake, and the constraint''s justification collapses into pure institutional choice (which grounds the constraint''s contestation across all sibling readings). If property framing is adequate, the framework stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_concept_applicability_to_software, conceptual, 'Whether property-rights doctrine is the appropriate conceptual frame for software control.').

omega_variable(
    vendor_exit_options_and_alternatives,
    'If software vendors'' property rights were removed or severely restricted, would software development funding mechanisms exist (open-source foundations, government research, cooperative models), and if so, would they sustain comparable or superior outcomes?',
    'Natural experiments from jurisdictions with mandatory interoperability (EU), enforced right-to-repair (France), or reduced patent scope (Australia). Observe: do alternative funding models emerge; is development quality sustained; does innovation rate change; do users benefit or suffer.',
    'Evidence of viable alternatives weakens the ''property rights are necessary'' justification and suggests the constraint persists through institutional power rather than necessity. Evidence that alternatives produce inferior outcomes strengthens the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_exit_options_and_alternatives, empirical, 'Whether software property rights are necessary for professional development or contingent on institutional choices.').

omega_variable(
    security_researcher_suppression_mechanism,
    'Is the measured suppression (0.71) primarily structural (legal barriers like CFAA, license terms) or internalized (researchers accept vendor definitions of acceptable research as legitimate)?',
    'Post-restriction analysis: if researchers adopt open-source security auditing after restrictive vendor regimes relax or are bypassed, suppression was primarily structural. If researchers continue to defer to vendors even after legal barriers are removed, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — researchers carry the suppression with them even when barriers are removed. This would argue for higher suppression values and greater entrenchment of the constraint''s power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_researcher_suppression_mechanism, empirical, 'Structural vs. internalized suppression of security research in proprietary software regimes.').

omega_variable(
    kernel_reading_contention_locus,
    'Where exactly does the contention between property-rights and freedom-imperative readings reside? At the level of axioms (software IS/IS NOT property), at the level of authority (vendors DO/DO NOT have legitimate authority), or at the level of value (restricting software access is GOOD/BAD)?',
    'Discourse analysis of debate between property-rights advocates and freedom-imperative advocates: locate points of claimed disagreement. Identify what would settle each: empirical evidence (software development outcomes), axiomatic premises (category membership), or value commitments (freedom weight).',
    'If disagreement is primarily axiomatic (category membership), only a conceptual/philosophical resolution path exists. If primarily empirical, data can resolve it. If primarily value-based, disagreement is structural and persists regardless of evidence. This omega routes the kernel''s contestation mechanics into visible structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention_locus, conceptual, 'The locus of contention between property-rights and freedom-imperative kernel readings.').

omega_variable(
    downstream_integrator_exit_feasibility,
    'For downstream integrators (companies building on proprietary software stacks), are alternative platforms and libraries truly available as functional exit options, or is exit only theoretical due to network effects and user lock-in?',
    'Case study analysis: pick downstream-integrator companies in mature vendor-dependent positions (e.g., companies built on Microsoft .NET, AWS, Salesforce platforms). Assess actual costs and barriers to migrating to alternative stacks or building proprietary reimplementations.',
    'If exit is theoretical but practically blocked, integrators are trapped targets and directionality is high (d ≈ 0.75+), strengthening Snare classification. If exit is costly but feasible, directionality is moderate (d ≈ 0.55-0.65), supporting Tangled Rope. If exit is easy, the constraint''s extraction is dampened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_integrator_exit_feasibility, empirical, 'Whether downstream integrators have genuine exit options or are functionally trapped.').

omega_variable(
    commons_reading_coexistence_stability,
    'Can property-rights and commons readings genuinely coexist long-term in separate institutional spheres, or does the dominance of proprietary software in venture capital and institutional procurement eventually exclude commons-based alternatives?',
    'Longitudinal analysis: track market share, developer investment, and institutional legitimacy of proprietary vs. commons software over 20-30 year periods. Observe whether commons alternatives remain viable as primary development models or become relegated to niche or volunteer sectors.',
    'If commons are structurally excluded from major investment flows and institutional procurement despite technical parity, the readings do not coexist — the property-rights reading dominates. This suggests a foreclosure dynamic rather than coexistence, requiring the cs_structure.reading_relations to shift from ''coexists_with'' to ''influences'' or ''forecloses''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_reading_coexistence_stability, empirical, 'Whether property-rights and commons readings can sustainably coexist or whether one reading eventually dominates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__property_rights_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__property_rights_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__property_rights_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__property_rights_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__property_rights_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__property_rights_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__property_rights_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__property_rights_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__property_rights_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__property_rights_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__property_rights_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__property_rights_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__property_rights_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__property_rights_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__property_rights_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__property_rights_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__property_rights_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__property_rights_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__property_rights_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__property_rights_reading, 0.18).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (software_control_legitimacy). The four sibling readings decompose the kernel's legitimacy grounds: property-rights (this story) grounds authority in IP doctrine; freedom-imperative denies proprietary software legitimacy; commons grounds authority in collective governance; pragmatic-openness treats control as a methodology choice. Each reading instantiates a distinct constraint with distinct ε, distinct victim/beneficiary structures, and distinct political positions. They are not perspectives on one constraint — they are separate constraints that share a kernel. The property-rights reading's ε is moderate (0.62) because it solves a coordination problem but does so through extraction. The freedom-imperative reading's ε would be higher (extracting user freedoms). The commons reading's ε would be lower (solving coordination through negotiated management with lower extraction). The pragmatic reading treats open-source as better engineering, not a legitimacy question, so its ε depends on empirical outcomes, not on grounds of authority. Sibling readings are linked via network.affects_constraints from each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__property_rights_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
