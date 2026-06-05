% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__severity_carve_out_reading, []).

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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Doctrine — Severity Carve-Out Reading
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   The beta designation doctrine instantiates a contested kernel: what role
 *   should provisional liability allocation (beta status) play in markets
 *   with irreducible safety constraints? This story documents ONE reading of
 *   that kernel — the severity carve-out reading — which holds that harm
 *   severity categorically constrains contractual freedom: beta designation
 *   is structurally unavailable in life-safety, financial, or other critical
 *   domains regardless of testing status or disclosure. The carve-out reading
 *   is grounded in a natural-duty principle: the severity of potential harm
 *   determines the minimum level of care and liability responsibility, not
 *   the vendor's preferred risk allocation. This reading coexists with two
 *   siblings: the expansive shield reading (beta is available whenever tested
 *   and disclosed, regardless of domain) and the narrow warning reading (beta
 *   is available with domain-specific restrictions, but not categorically
 *   prohibited). The severity carve-out differs structurally from both: it
 *   posits an irreducible ceiling on permissible contractual escape via
 *   domain identity, not via testing or disclosure mechanics. The constraint
 *   produces a mixed coordination-extraction hybrid (tangled rope) because it
 *   coordinates safety expectations while simultaneously extracting from
 *   developers in restricted domains who bear full compliance cost without
 *   ability to exit or reprice risk.
 *
 * KEY AGENTS:
 *   - Life-Safety-System Developers (Trapped/Powerless): Medical device vendors, automotive control engineers, aviation software companies — face categorical prohibition, high compliance cost, no meaningful exit from critical markets
 *   - Regulators and Safety Bodies (Beneficiary/Institutional): FDA, NHTSA, aviation authorities, pharmaceutical regulators — benefit from carve-out as enforcement tool; coordinates sector-wide safety rigor without specific mandates
 *   - General Software Vendors (Constrained/Moderate): Consumer software, business software, non-critical systems — benefit from beta permission in non-critical domains; constrained by domain-boundary uncertainty and liability cascade risk
 *   - Large Safety-Critical Vendors (Powerful/Arbitrage): Established medical software, aerospace contractors, financial infrastructure vendors — absorb compliance cost through economies of scale; use carve-out as market-gatekeeping mechanism
 *   - Patient and Consumer Advocates (Organized/Constrained): Consumer safety organizations, patient advocacy groups — benefit from carve-out as signal of minimum rigor; constrained from opposing by safety alignment
 *   - Analytical Observer (Civilizational): Meta-level view of liability doctrine — risks naturalizing contingent regulatory choice as irreducible principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.35).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.48).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Doctrine — Severity Carve-Out Reading").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, 'reading_severity_carve_out_v1').
narrative_ontology:cs_kernel_codification('reading_severity_carve_out_v1', formalized).
narrative_ontology:cs_authority_grounding('reading_severity_carve_out_v1', extraction).
narrative_ontology:cs_interpretation_layer_present('reading_severity_carve_out_v1').
narrative_ontology:cs_reading_relation('reading_severity_carve_out_v1', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('reading_severity_carve_out_v1', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('reading_severity_carve_out_v1', foundational, harm_severity_irreducible_ceiling).
narrative_ontology:cs_axiom_status(harm_severity_irreducible_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('reading_severity_carve_out_v1', harm_severity_irreducible_ceiling, deontological).
narrative_ontology:cs_axiom('reading_severity_carve_out_v1', foundational, domain_identity_gates_beta_availability).
narrative_ontology:cs_axiom_status(domain_identity_gates_beta_availability, holdable).
narrative_ontology:cs_axiom_grounding('reading_severity_carve_out_v1', domain_identity_gates_beta_availability, deontological).
narrative_ontology:cs_reference_frame('reading_severity_carve_out_v1', irreducible_duty_of_care_ceiling).
narrative_ontology:cs_drift_state('reading_severity_carve_out_v1', contemporary_ai_autonomy_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('reading_severity_carve_out_v1', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, safety_critical_sector_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, public_health_infrastructure).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_vendors_in_restricted_domains).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, developers_constrained_by_carve_out).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Developers of medical devices, automotive control systems, and industrial safety controls face categorical prohibition on beta designation with no meaningful exit: they cannot operate in these markets without full compliance. The constraint extracts from them through compliance cost (testing, liability insurance, documentation) while providing no coordination benefit — only exclusion. High d → high χ → snare from this position.
constraint_indexing:constraint_classification(beta_designation_doctrine__severity_carve_out_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Regulators and safety bodies (FDA, NHTSA, aviation authorities) benefit from the carve-out as an enforcement mechanism: it allows them to mandate full development rigor in critical domains without writing specific testing requirements. The constraint coordinates sector-wide safety standards while benefiting the regulator's authority. Low d → negative χ → rope from this position.
constraint_indexing:constraint_classification(beta_designation_doctrine__severity_carve_out_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Non-safety-critical software vendors experience the carve-out as a mixed constraint. They benefit from being able to use beta designation for non-critical systems (coordination function: reduces pressure to perfect non-critical features before release). They are constrained by domain-boundary uncertainty (what counts as 'life-safety'?) and by downstream liability exposure if their software integrates with safety-critical systems. Moderate d → moderate χ → tangled rope.
constraint_indexing:constraint_classification(beta_designation_doctrine__severity_carve_out_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Established vendors of medical, automotive, and aerospace software can absorb carve-out costs through compliance economies of scale and pass them to customers. They experience the constraint as a coordination mechanism (predictable liability rules) paired with extraction (smaller competitors cannot enter the market). High power + arbitrage exit → lower d → rope-like effective extraction, but the coordination function is mixed with market gatekeeping.
constraint_indexing:constraint_classification(beta_designation_doctrine__severity_carve_out_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Consumer safety organizations benefit from the carve-out as a signal of minimum development rigor — beta prohibition in life-safety domains functions as a coordination mechanism signaling that these systems are held to higher standards. The constraint reduces information asymmetry and provides legal recourse clarity. Organized power + constrained exit (cannot lobby for removal without opposing safety) → rope.
constraint_indexing:constraint_classification(beta_designation_doctrine__severity_carve_out_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From a civilizational/universal perspective, this constraint appears as an irreducible natural law: harm severity categorically cannot permit provisional liability allocation. The constraint follows from the logical principle that critical functions require irreducible duty of care, not from contingent institutional design. However, structural data contradicts mountain classification — the carve-out is a doctrine (constructed) not a law of physics/logic. False summit risk: naturalizing a contingent regulatory choice as inevitable.
constraint_indexing:constraint_classification(beta_designation_doctrine__severity_carve_out_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beta_designation_doctrine__severity_carve_out_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beta_designation_doctrine__severity_carve_out_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The carve-out imposes real compliance costs on safety-critical vendors (testing, liability insurance, documentation, slower release cycles) and constrains market entry. However, the extraction is not severe because (1) the beneficiaries (regulators, safety-focused consumers) derive genuine public-health value, (2) large vendors can absorb costs and treat them as competitive barriers rather than pure extraction, (3) the doctrine is explicitly justified by harm-severity reasoning, not by rent-seeking. The trajectory (0.22 → 0.35 over interval) reflects increasing vendor sophistication in absorbing and passing through compliance costs, plus gradual domain expansion as AI and autonomous systems become life-safety-critical in new contexts. Suppression (0.48): Moderate-high. Barriers to challenging the carve-out include: (1) genuine public-health concerns that make arguments against carve-out appear callous, (2) regulatory capture (incumbents support the carve-out), (3) legal precedent and doctrinal entrenchment, (4) international harmonization pressure. Suppression is not total because (1) vendors can litigate domain boundaries, (2) market competition still occurs within permitted domains, (3) disclosure and testing can modulate extraction at the margins. Theater ratio (0.52): Moderate. Some theater in the doctrine: compliance documentation exceeds actual risk reduction, certification processes contain performative elements, and domain boundaries are sometimes applied more as ritual than as functional thresholds. However, the underlying safety coordination is genuine — not purely performative. The ratio increases over time as vendors develop more sophisticated compliance theater (check-box testing, safety-theater process compliance) while actual development approaches remain flexible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a classic carve-out structure: the same structural phenomenon (beta designation availability) appears as snare, rope, and tangled rope from different perspectives. Trapped developers see extraction with no benefit. Beneficiary regulators see pure coordination. Constrained moderate vendors see mixed benefit and cost. Large vendors see a gatekeeping mechanism they can exploit. The mountain perspective risks naturalizing the carve-out as an inevitable law of safety duty rather than a contingent regulatory choice. The perspectival gap exposes the doctrine's internal contradiction: if beta is truly incompatible with life-safety duty, the incompatibility should follow from logic or physics (mountain). But the carve-out is a doctrine — a human-constructed rule — which means it is chosen, not inevitable. The choice benefits some parties (regulators, large vendors, safety advocates) at cost to others (new vendors, developers in restricted domains). No single perspective captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position. Trapped developers: caught between market demand (beta adoption pressure) and categorical prohibition (no exit route); victimhood from suppression + vendor powerlessness → high d ≈ 0.92 → high f(d) ≈ 1.35 → snare. Beneficiary regulators: benefit from enforcement mechanism, low cost to implementation, arbitrage exit (can rewrite doctrine unilaterally); beneficiary + arbitrage → low d ≈ 0.12 → negative f(d) ≈ -0.08 → rope. Constrained general vendors: mixed position — benefit from beta permission in non-critical domains, constrained by domain-boundary rules and liability cascade; balanced position → d ≈ 0.50 → moderate f(d) ≈ 0.65 → tangled rope. Large vendors: powerful position with arbitrage (can lobby for exemptions or clarifications), benefit from market gatekeeping, but also comply with doctrine; organized + arbitrage → low-moderate d ≈ 0.28 → f(d) ≈ 0.16 → rope-to-tangled-rope boundary. Organized advocates: organized power, constrained by ideological alignment with safety, benefit from carve-out as safety signal; organized + constrained → moderate d ≈ 0.38 → f(d) ≈ 0.35 → rope. Analytical observer: detached position, witnesses the doctrine as constructed; analytical exit → d ≈ 0.73 → f(d) ≈ 1.14 → mountain classification, but false summit risk because doctrine is not natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying the distinction between harm-severity principle and enforcement mechanism. The PRINCIPLE — that irreducible duty of care constrains contractual liability escape — is non-contingent and grounded in logic/ethics (mountain-adjacent). The MECHANISM — categorical prohibition on beta designation — is contingent and could be replaced by alternative enforcement approaches (tangled rope, piton, or rope depending on alternative). This story focuses on the mechanism (the doctrine), not the principle. The mechanism coordinates safety expectations while extracting from developers. The mandate is stable only if the benefit (safety coordination) exceeds the cost (developer extraction + market inefficiency). As alternatives emerge (disclosure-based frameworks, tiered certification, domain-specific testing standards), the carve-out mechanism loses its monopoly on satisfying the underlying principle. The measurement trajectory shows increasing theater (vendors developing check-box compliance) paired with increasing extractiveness (more domains classified as critical), suggesting the mechanism is degrading relative to the principle. Long-term stability depends on whether alternative enforcement mechanisms emerge that satisfy the principle more efficiently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_ambiguity,
    'What empirical or logical criteria define ''life-safety'' domain boundaries? Is software life-safety-critical by function, by consequence, or by design intent?',
    'Systematic case analysis: software classified as safety-critical vs. non-critical in regulatory decisions; correlation with actual harm outcomes; comparison of liability standards applied',
    'If functional definition: beta prohibition cascades into adjacent domains (fitness tracking, telehealth analytics). If consequence-based: prohibition is outcome-dependent, retroactively applied. If intent-based: developers control classification through framing. Each resolution changes the constraint''s scope and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_ambiguity, empirical, 'Definition of life-safety domain boundaries and classification criteria').

omega_variable(
    carve_out_vs_absolute_prohibition,
    'Is the beta prohibition a carve-out from a general-permissibility rule (contrast to expansive shield where beta IS permitted), or is it an absolute prohibition? Does the doctrine permit exceptions for disclosure, testing, or consent?',
    'Doctrinal analysis: regulatory text, case law, enforcement guidance; historical evolution of the rule; comparison across jurisdictions',
    'If carve-out: the severity principle is the dominant framework, and exceptions must be justified. If absolute: the doctrine is inflexible but clearer. This reading assumes carve-out status; if reframed as absolute, sibling relations shift (more likely forecloses than influences).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carve_out_vs_absolute_prohibition, conceptual, 'Whether the prohibition is a carve-out from permissibility or an absolute rule').

omega_variable(
    dual_constraint_family_decomposition,
    'Is the beta designation doctrine ONE constraint viewed from different perspectives, or multiple structurally distinct constraints (one per domain: life-safety carve-out, general-software beta shield, liability allocation baseline)?',
    'ε-invariance test: Can the same constraint description yield different ε values when observed through different measurement methodologies? If domain-specific observation shifts ε by >0.25, decompose into separate stories per domain.',
    'If one constraint: this story captures the full doctrine. If multiple: this story represents only the life-safety carve-out reading; siblings exist for expansive and narrow readings with different ε values and different beneficiary/victim sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_constraint_family_decomposition, conceptual, 'Whether beta designation doctrine is one constraint or a family of domain-specific constraints').

omega_variable(
    regulatory_capture_of_carve_out,
    'Does the carve-out doctrine benefit genuine public health/safety, or does it function primarily to entrench large vendors and regulatory capture by excluding new entrants?',
    'Market analysis: vendor concentration in safety-critical sectors over time; cost barriers to entry; enforcement disparities; comparative outcomes in jurisdictions with and without carve-out',
    'If genuine benefit: carve-out is coordination + legitimate extraction (vendors absorb real cost). If capture: extractiveness should be uprated; beneficiary designation shifts from regulators to incumbent vendors; sibling relations with expansive shield reading shift from coexists_with to forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_carve_out, empirical, 'Whether the carve-out serves public safety or vendor capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_sev_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(beta_sev_tr_t5, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(beta_sev_tr_t10, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(beta_sev_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(beta_sev_be_t5, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(beta_sev_be_t10, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(beta_sev_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(beta_sev_su_t5, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(beta_sev_su_t10, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__severity_carve_out_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, medical_device_software_liability_standard).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, autonomous_vehicle_liability_allocation).

% DUAL FORMULATION NOTE:
% The beta_designation_doctrine kernel spawns multiple constraint stories corresponding to different readings. This story (severity carve-out) documents ONE reading with ε=0.35. The expansive shield reading has higher ε (vendor-friendly framework) and differs in beneficiary/victim structure. The narrow warning reading has intermediate ε and domain-specific gradation. These are not the same constraint viewed differently — they are structurally distinct claims about what permissive role beta status should play. Link via network.affects_constraints to track doctrinal family relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__severity_carve_out_reading, institutional, 0.08).
constraint_indexing:directionality_override(beta_designation_doctrine__severity_carve_out_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
