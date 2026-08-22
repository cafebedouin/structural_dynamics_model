% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Section 2(b) Strong Copyleft Reading: Derivative Work Boundary
 *   domain: intellectual_property/software_licensing/open_source_governance
 *
 * SUMMARY:
 *   This story instantiates the strong copyleft reading of GPL Section 2(b):
 *   the derivative-work boundary is interpreted to extend to all forms of
 *   code coupling (dynamic linking, plugin architectures, runtime
 *   composition), not merely direct derivative works under traditional
 *   copyright doctrine. Under this reading, proprietary software vendors are
 *   structurally excluded from GPL component integration without releasing
 *   their own source under GPL. The free-software community gains a credible
 *   enforcement mechanism against enclosure. The constraint is a snare:
 *   extraction is high (0.82), suppression is substantial (0.71), and the
 *   payer seats face collapsed exit options (either avoid GPL code entirely
 *   or release source). The measurement trajectory shows extractiveness
 *   rising from 0.71 to 0.82 over the interval as the strong reading gains
 *   enforcement credibility through case law accumulation and community
 *   norm-hardening.
 *
 * KEY AGENTS:
 *   - free_software_community: Beneficiary — maintains GPL codebases, sets the strong copyleft interpretation, enforces it selectively against violators.
 *   - proprietary_software_vendors: Primary victim — structurally excluded from GPL integration without source release; face binary choice (reimplements vs. GPL violation vs. license negotiation).
 *   - closed_source_incorporators: Secondary victim — mid-market firms using GPL code in closed products; constrained exit options force costly rewrites or litigation risk.
 *   - FSF_and_aligned_projects: Agenda setter — stewards the GPL license text, interprets Section 2(b) broadly, maintains enforcement credibility through litigation threats.
 *   - plugin_architecture_designers: Excluded — would design GPL/proprietary boundary mechanisms if strong copyleft reading did not classify dynamic linking as derivative work.
 *   - courts_and_legislatures: Observer — lack definitive precedent; retain power to narrow or broaden the derivative-work boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.82).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.71).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) Strong Copyleft Reading: Derivative Work Boundary").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "intellectual_property/software_licensing/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '612369c7-e5be-46bc-b076-e04a7c5536c0').
narrative_ontology:cs_kernel_codification('612369c7-e5be-46bc-b076-e04a7c5536c0', fixed_text).
narrative_ontology:cs_authority_grounding('612369c7-e5be-46bc-b076-e04a7c5536c0', lineage).
narrative_ontology:cs_interpretation_layer_present('612369c7-e5be-46bc-b076-e04a7c5536c0').
narrative_ontology:cs_reading_relation('612369c7-e5be-46bc-b076-e04a7c5536c0', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('612369c7-e5be-46bc-b076-e04a7c5536c0', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('612369c7-e5be-46bc-b076-e04a7c5536c0', foundational, dynamic_linking_is_derivative_work).
narrative_ontology:cs_axiom_status(dynamic_linking_is_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('612369c7-e5be-46bc-b076-e04a7c5536c0', dynamic_linking_is_derivative_work, deontological).
narrative_ontology:cs_axiom('612369c7-e5be-46bc-b076-e04a7c5536c0', secondary, copyleft_scope_extends_to_all_coupling).
narrative_ontology:cs_axiom_status(copyleft_scope_extends_to_all_coupling, holdable).
narrative_ontology:cs_axiom_grounding('612369c7-e5be-46bc-b076-e04a7c5536c0', copyleft_scope_extends_to_all_coupling, instrumental).
narrative_ontology:cs_reference_frame('612369c7-e5be-46bc-b076-e04a7c5536c0', gpl_text_maximalist_freedom_interpretation).
narrative_ontology:cs_drift_state('612369c7-e5be-46bc-b076-e04a7c5536c0', contemporary_proprietary_integration_attempts, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('612369c7-e5be-46bc-b076-e04a7c5536c0', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_community).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, closed_source_incorporators).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, software_freedoms_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, copyleft_cascade_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and extends GPL-licensed codebases under the strong copyleft reading. Benefits from the structural guarantee that any derivative or dynamically linked work must remain GPL-licensed, ensuring code availability cascades to downstream integrators. Interprets Section 2(b) to cover dynamic linking, plugin architectures, and runtime composition as forms of code coupling. Their enforcement strategy (selective litigation, license-grant threats, attribution under GPLv3 tiered disclosure) creates credible constraints on proprietary reuse.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_community, beneficiary,
    organized, generational, mobile, global).

% Face binary choice under strong copyleft reading: either avoid GPL components entirely (forcing reimplementation, acquiring alternatives, or accepting competitive disadvantage in speed-to-market), or release source under GPL (contradicting their business model of proprietary source as competitive moat). Their exit options are constrained by the practical necessity of using high-value GPL libraries (kernel modules, compression, cryptography). They bear the cost of exclusion from GPL ecosystem or loss of proprietary status if they integrate.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Mid-size software firms that use GPL code in closed products without releasing source. Under strong copyleft reading, they are in technical violation if their linking pattern (dynamic or static) is interpreted as derivative work creation. They cannot easily exit: reimplementation is cost-prohibitive; publicly releasing source kills their product differentiation; license violations expose them to enforcement actions. Their options compress: maintain current code base with litigation risk, negotiate dual licensing at cost, or rewrite.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, closed_source_incorporators, payer,
    moderate, biographical, constrained, global).

% Sets and enforces the strong copyleft interpretation of GPL Section 2(b) through license language, enforcement actions (e.g., FSF v. Cisco), COPYING file documentation, and community norm-setting. Maintains the GPL license text, interprets ambiguous coupling cases in the direction of maximum copyleft scope, and reserves the right to revoke licenses or pursue legal action against violators. Their interpretive authority derives from GPLv2/v3 stewardship and a track record of selective enforcement that makes the threat credible.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, fsf_and_aligned_projects, agenda_setter,
    institutional, generational, arbitrage, global).

% Would architect software systems with clear GPL/proprietary boundaries through plugin interfaces, if the strong copyleft reading did not classify dynamic linking as derivative work. They are excluded from the design-choice set: the strong reading makes plugin architecture unviable for proprietary extensions. They have no seat in the GPL interpretation debate but would argue for functional separation of GPL core from proprietary overlay.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, plugin_architecture_designers, excluded,
    moderate, biographical, constrained, global).

% Monitor GPL copyleft scope for intersection with their patent portfolios. If GPL Section 2(b) is read broadly enough to reach dynamic linking and plugin patterns, GPL codebases become harder for patent holders to license defensively or weaponize; conversely, if narrowly read, GPL community loses leverage to prevent patent enclosure. They are observers of the scope debate but stand to benefit from narrower readings that leave more flexibility in licensing strategies.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, software_patent_holders, observer,
    powerful, biographical, arbitrage, global).

% Have not yet definitively ruled on the derivative-work boundary under GPL Section 2(b). The strong copyleft reading is defended through enforcement threats and community norm-setting, but lacks judicial precedent establishing its scope. Courts retain the power to narrow or broaden the derivative-work boundary through interpretation of the GPL text and copyright law principles. Their seat is analytical: they observe the dispute but have not yet adjudicated it.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, free_software_community).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a guarantee of code availability and forward propagation of freedoms: any software that incorporates or dynamically links a GPL component must either remain closed to other integrators (making integration impractical) or release its source under GPL (enforcing the propagation of the freedom commitment to all downstream users).
% TRANSFER_FUNCTION: Transfers the right to distribute proprietary derivatives from proprietary software vendors to the free-software community. Vendors pay by forgoing proprietary ownership of any work that incorporates GPL code, or by accepting the cost and risk of license violation. The community collects by gaining automatic access to all downstream derivatives and a credible enforcement mechanism against enclosure.
% ABSENT_VOICES: Proprietary software vendors would argue for narrower derivative-work boundaries (functional rather than technical coupling) and for explicit permission to dynamically link without triggering copyleft. Plugin architects would argue for clear separation mechanisms. Courts (absent judicial precedent) and patent holders (observing the scope) are structurally excluded from the norm-setting process that affirms the strong reading.
% DISAPPEARANCE_RATIONALE: If the strong copyleft reading disappeared and were replaced by a narrow-scope or narrow-linking reading, proprietary vendors would integrate GPL components without releasing source; plugin architectures would separate GPL cores from proprietary extensions through dynamic linking; software markets would reorganize around permissive-licensed components and narrow GPL components; the free-software community would lose the structural guarantee of cascading freedom and the enforcement credibility that sustains it.
% FOUNDING_PROBLEM: Early GPL community confronted a risk of proprietary enclosure: vendors could redistribute GPL-licensed binaries while withholding source, or link against GPL libraries without acknowledging the copyleft obligation, effectively converting free software into proprietary products. The strong copyleft reading was articulated to prevent this conversion by extending the derivative-work boundary to all forms of technical coupling, making proprietary reuse impossible without releasing source.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and GPL community attest the founding problem remains live: vendors continue to test the boundaries of GPL interpretation through dynamic linking, plugin architectures, and aggregate licensing. Proprietary vendors attest the founding problem was solved or overstated: source-release requirements are already clear for direct derivatives, and extending copyleft to dynamic linking overreaches copyright law. No court has yet ruled definitively; the corroboration is community assertion without judicial vindication.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the strong copyleft reading structurally forces proprietary vendors to choose between high-cost exits: reimplementation (cost-prohibitive for complex libraries), full source release (business-model destroying), dual licensing (expensive), or license violation (legal risk). Suppression is substantial (0.71) because enforcement is maintained through FSF litigation threats, community norm-setting, and the threat of license revocation — the suppression is active and credible. Theater ratio is moderate (0.28): the stated purpose (guarantee freedom propagation) is genuine, but a growing share of enforcement effort defends the scope boundary itself against narrowing interpretations rather than defending the coordination function. The measurement series show extractiveness rising over time as proprietary integration attempts increase and the strong reading hardens through case-law and community practice; suppression also rises as enforcement infrastructure (litigation, DMCA takedown coordination, vendor compliance programs) becomes more established.
 *
 * PERSPECTIVAL GAP:
 *   From the free-software community's seat, the strong copyleft reading is a justified coordinate: it prevents proprietary enclosure and ensures freedoms propagate. From the proprietary vendor's seat, the same structure operates as a licensing trap: they cannot use high-value GPL components without violating their business model. From the plugin architect's seat, it is a design constraint that forecloses an entire category of architecture. The engine computes these divergences from the structural data: beneficiary with mobile exit options experiences low d (low/negative χ); victim with constrained exit experiences high d (high χ). The perspectival gap is structural, not a difference of opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   Free-software community: organized power, mobile exit (can fork, create alternatives, maintain parallel ecosystems), generational horizon. Despite beneficiary role, their d is near 0.2–0.35 because they have leverage to exit the GPL ecosystem itself and their mobility is genuine. Proprietary vendors: powerful but not organized (competing individually), constrained exit (cannot escape GPL code without massive reimplementation cost or business-model change), biographical horizon. Payer role + constrained exit puts d near 0.75–0.90. Closed-source incorporators: moderate power, constrained exit (same reimplementation cost, less litigation resources), biographical horizon. Payer role + moderate power + constrained exit puts d near 0.80–0.88. FSF/aligned projects: institutional power, arbitrage exit (can shift interpretation or create new license if GPL fails), generational horizon. Agenda-setter role + institutional power + arbitrage exit puts d near 0.05–0.15. Courts: institutional power, analytical exit, generational horizon. Observer role puts d near 0.5 (no direct benefit or cost; they maintain the leverage to change the game).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary enclosure of GPL code) is contested because the strong copyleft reading's scope extension (dynamic linking as derivative work) goes beyond the GPL text's plain language and beyond traditional copyright doctrine. Some proprietary vendors argue the founding problem was solved by requiring source release for direct derivatives, and the strong reading overreaches. The free-software community argues the founding problem remains live because vendors continue to find loopholes. The mandatrophy question: Does the constraint still solve the problem it was built for, or has its function shifted to rent collection from proprietary software? The strong reading's function has shifted from preventing enclosure to enforcing a bright-line rule that proprietary vendors must exit the GPL ecosystem entirely — that is extraction, not coordination. A narrow reading would restore the founding-problem focus (preventing source hiding on direct derivatives) while leaving dynamic linking ambiguous. The strong reading transforms the constraint from a coordination mechanism into a snare by making the boundary so expansive that exit becomes prohibitively expensive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_ambiguity,
    'Is dynamic linking a form of derivative work under copyright law, or is it merely aggregation/linking of independent works?',
    'Judicial precedent (U.S. courts have not definitively ruled on this specific question as of the knowledge cutoff; it remains a live interpretive question under the DMCA, the GPL, and copyright doctrine). Resolution requires either a court ruling on the GPL''s scope or legislative clarification of derivative-work boundaries in software.',
    'If dynamic linking is classified as derivative work, the strong copyleft reading stands as the correct interpretation of GPL Section 2(b), and proprietary vendors face the binary choice (GPL release vs. exit). If it is not, the narrow reading prevails, and proprietary vendors can use GPL components dynamically without triggering copyleft obligations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, empirical, 'Whether dynamic linking constitutes derivative-work creation under copyright law.').

omega_variable(
    fsf_enforcement_credibility,
    'Does the FSF''s selective enforcement strategy (choosing which violations to litigate) establish a credible threat or undermine the universality of the copyleft commitment?',
    'Empirical observation of enforcement patterns: does litigation follow consistent principles (e.g., all proprietary kernel modules) or appear arbitrary (selective enforcement against some vendors but not others)? Post-litigation settlements and public statements about enforcement priorities.',
    'If FSF enforcement is perceived as credible and consistent, the suppression required to maintain the strong copyleft reading remains high (~0.71); if enforcement appears arbitrary or weakens, the suppression required drops and the constraint risks drift toward the narrow reading or enforcement vacuum reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsf_enforcement_credibility, empirical, 'Whether FSF enforcement strategy sustains the credible threat that underpins the strong reading.').

omega_variable(
    functional_vs_technical_coupling_interpretation,
    'Does GPL Section 2(b) create an obligation based on functional coupling (does the code call or depend on GPL code?) or technical coupling (is the code statically or dynamically linked)?',
    'Close reading of GPL text (Section 2(b) uses ''derivative work'', which copyright law defines via substantial copying and creative transformation, not via linking mechanism); review of FSF''s official interpretations (FSF FAQ, COPYING files in GPL projects); case-law development if courts rule on the question.',
    'If the interpretation is functional, then plugin architectures and protocol-based integration can escape copyleft without releasing source (narrow reading); if technical, even loosely coupled runtime composition triggers copyleft (strong reading). The strong reading asserts a technical interpretation; the narrow reading asserts functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_technical_coupling_interpretation, conceptual, 'Whether the GPL''s derivative-work obligation is grounded in functional or technical coupling.').

omega_variable(
    reading_foreclosure_risk,
    'Does the strong copyleft reading logically foreclose the narrow reading, or can they coexist as different interpretive communities'' commitments?',
    'Structural analysis: if both readings claim to be faithful to GPL Section 2(b)''s text and copyright law, and the text is ambiguous, then both can coexist (coexists_with relation). If the strong reading''s technical-coupling interpretation directly contradicts the narrow reading''s functional-coupling interpretation such that no single framework could hold both, then the strong reading forecloses the narrow reading (forecloses relation). The engine resolves this via axiom contradiction detection.',
    'If the readings coexist, the constraint landscape remains plural and enforcement-dependent; if the strong reading forecloses, the narrow reading becomes untenable within the GPL community and proprietary vendors must exit. This omega documents the uncertainty about whether the kernel admits two live readings or one winner.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_risk, conceptual, 'Whether the strong copyleft reading logically forecloses narrower interpretations or merely competes with them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 5, 0.74).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 10, 0.77).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(gpl__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(gpl__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__strong_copyleft_reading, 0.18).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, proprietary_licensing_constraints).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, open_source_market_dynamics).

% DUAL FORMULATION NOTE:
% The gpl_copyleft_scope kernel decomposes into three readings: strong_copyleft_reading (this story — high-epsilon snare with expansive derivative-work boundary), narrow_scope_reading (lower epsilon, narrower boundary following traditional copyright doctrine), and enforcement_vacuum_reading (intermediate epsilon, ambiguity maintained by lack of judicial precedent). The three readings are structurally distinct constraints because their ε values differ substantially (strong reading: 0.82, narrow reading: ~0.35–0.45, enforcement vacuum: ~0.55–0.65) and their stakeholder structures differ (strong reading benefits free-software community and extracts from proprietary vendors; narrow reading reduces extraction; enforcement vacuum reading maintains ambiguity). Each reading should be authored as a separate story with its own six-questions interview, measurements, and omegas. This story links to its siblings via network.affects_constraints. The three readings are not perspectives on one constraint; they are three materially different constraints arising from interpreting the same GPL text differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
