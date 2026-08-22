% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Boundary — Coordination Reading (Transformative Use Exempt)
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the coordination reading of the
 *   derivative_work_statutory_boundary kernel. The reading holds that only
 *   fixed recastings substantially incorporating original expression
 *   constitute derivative works requiring authorization; transformative uses
 *   (including ML training on copyrighted corpora, style transfer, and
 *   generative synthesis that does not output substantially similar
 *   expression) and intermediate copies made in the course of non-infringing
 *   transformation are non-infringing. This reading functions as a
 *   coordination scaffold for generative technologies — it creates a
 *   predictable, low-extraction boundary that enables innovation without
 *   ex-ante licensing friction. The claimed_type is rope: a genuine
 *   coordination mechanism with minimal coercive overhead, where participants
 *   (developers, researchers, platforms, creators) are net beneficiaries and
 *   alternatives (licensing regimes, opt-out systems) are not suppressed. The
 *   kernel is contested: the enclosure reading claims any use of copyrighted
 *   expression in creating new work prepares a derivative work; the hybrid
 *   carveout reading conditions the boundary on commercial exploitation. This
 *   story authors ONLY the coordination reading as a clean, ε-invariant
 *   constraint per Rule 1.
 *
 * KEY AGENTS:
 *   - generative_ai_developers: Primary beneficiary (institutional/arbitrage) — build models on copyrighted corpora without licensing; coordinate at scale
 *   - transformative_creators: Primary beneficiary (organized/mobile) — remix, critique, parody, and synthesize without clearance
 *   - research_institutions: Primary beneficiary (institutional/analytical) — conduct text/data mining and computational analysis
 *   - platform_operators: Secondary beneficiary (institutional/arbitrage) — host generative tools and user-generated transformative content
 *   - copyright_holders: Excluded/payer (powerful/constrained) — bear uncompensated use of works in training; would license if enclosure reading prevailed
 *   - collective_management_organizations: Excluded (organized/constrained) — lose licensing revenue streams under coordination reading
 *   - courts_legislatures: Agenda setters (institutional/generational) — adjudicate and amend the kernel's boundary
 *   - analytical_observers: Observers (analytical/analytical) — track structural dynamics across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.12).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.08).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary — Coordination Reading (Transformative Use Exempt)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property/technology_governance/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '24394578-820c-4e80-87d2-5450332ce36a').
narrative_ontology:cs_kernel_codification('24394578-820c-4e80-87d2-5450332ce36a', formalized).
narrative_ontology:cs_authority_grounding('24394578-820c-4e80-87d2-5450332ce36a', lineage).
narrative_ontology:cs_interpretation_layer_present('24394578-820c-4e80-87d2-5450332ce36a').
narrative_ontology:cs_reading_relation('24394578-820c-4e80-87d2-5450332ce36a', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('24394578-820c-4e80-87d2-5450332ce36a', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('24394578-820c-4e80-87d2-5450332ce36a', foundational, transformative_use_not_derivative_work_preparation).
narrative_ontology:cs_axiom_status(transformative_use_not_derivative_work_preparation, holdable).
narrative_ontology:cs_axiom_grounding('24394578-820c-4e80-87d2-5450332ce36a', transformative_use_not_derivative_work_preparation, deontological).
narrative_ontology:cs_axiom('24394578-820c-4e80-87d2-5450332ce36a', foundational, intermediate_copying_for_non_expressive_analysis_non_infringing).
narrative_ontology:cs_axiom_status(intermediate_copying_for_non_expressive_analysis_non_infringing, holdable).
narrative_ontology:cs_axiom_grounding('24394578-820c-4e80-87d2-5450332ce36a', intermediate_copying_for_non_expressive_analysis_non_infringing, empirically_contingent).
narrative_ontology:cs_reference_frame('24394578-820c-4e80-87d2-5450332ce36a', statutory_derivative_work_textualist).
narrative_ontology:cs_drift_state('24394578-820c-4e80-87d2-5450332ce36a', generative_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24394578-820c-4e80-87d2-5450332ce36a', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, research_institutions).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, platform_operators).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, transformative_use_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, intermediate_copying_non_infringing).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, statutory_derivative_work_narrow_construction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train foundation models on copyrighted text, code, and image corpora without licensing. The coordination reading allows this as non-infringing intermediate use / transformative analysis. They can route training to jurisdictions with favorable readings (arbitrage exit), but depend on the coordination reading's stability for predictable global deployment. They collect the economic value of models trained on unlicensed data.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers, beneficiary,
    institutional, biographical, arbitrage, global).

% Create remixes, parodies, critiques, style transfers, and synthetic media that transform source expression without outputting substantially similar fixed recastings. They benefit from a clear, low-cost boundary that does not require clearance. Their exit is mobile: they can shift platforms or jurisdictions, but the coordination reading's global normative reach (via Berne/TRIPS floor) gives them a portable defense.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    organized, biographical, mobile, global).

% Conduct text and data mining, computational analysis, and ML research on copyrighted corpora. The coordination reading treats this as non-infringing intermediate use. They are analytical observers who also benefit directly; their exit is analytical (they can study the boundary from outside) but their research depends on the boundary's permissiveness.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, research_institutions, beneficiary,
    institutional, generational, analytical, global).

% Host generative AI tools, user-generated transformative content, and model-sharing platforms. They benefit from the coordination reading's low-friction boundary (no need to police transformative uploads for derivative work liability). They also set platform policies that operationalize the boundary (e.g., content filters, opt-out tools). They can arbitrage across jurisdictions but prefer global regulatory stability.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, platform_operators, agenda_setter).

% Hold copyright in works used as training data or source material for transformative uses. Under the coordination reading, they have no control right over transformative/intermediate uses and receive no compensation. They would license these uses under the enclosure reading. Their exit is constrained: they are bound by the jurisdiction's construction of the derivative work boundary; they can lobby for legislative change or litigate for narrower transformative exceptions.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, copyright_holders, excluded,
    powerful, biographical, constrained, global).

% Administer collective licensing for reproduction, distribution, and derivative work rights. The coordination reading eliminates the derivative work licensing revenue stream for transformative/intermediate uses. They advocate for the enclosure or hybrid carveout readings. Their exit is constrained: they operate within national/regional copyright systems and cannot easily relocate their mandate.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, collective_management_organizations, excluded,
    organized, biographical, constrained, regional).

% Adjudicate the derivative work boundary in litigation and amend it through legislation. They bear the cost of interpreting and operationalizing the kernel. They are not direct beneficiaries or payers of the constraint's extraction; they set the rules that determine which reading prevails. Their exit is analytical: they observe the structural dynamics but are institutionally bound to resolve them.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Scholars, economists, and policy analysts who track the derivative work boundary's evolution across readings and jurisdictions. They neither collect from nor pay into the constraint; they map its structural dynamics. Their exit is analytical (unbound by the constraint's operation).
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, low-friction boundary that enables generative technologies, transformative creativity, and computational research to proceed without ex-ante licensing negotiation — solving the coordination problem of 'what can I build on?' with a rule that minimizes transaction costs and maximizes permissionless innovation.
% TRANSFER_FUNCTION: Moves zero ex-ante payments from users of copyrighted works (developers, creators, researchers) to copyright holders for transformative and intermediate uses. The only transfers are ex-post litigation costs and voluntary licensing for uses that fall outside the transformative safe harbor.
% ABSENT_VOICES: Individual creators and small rights-holders who lack institutional representation in CMOs or litigation capacity are structurally excluded from shaping the boundary. They would object to uncompensated use of their works in commercial AI training but are not in the room where the coordination reading is operationalized (courts, legislatures, platform policy teams). Their absence is the counterpart to the CMOs' excluded seat — CMOs claim to represent them but have divergent incentives.
% DISAPPEARANCE_RATIONALE: If the coordination reading vanished overnight (replaced by enclosure or hybrid carveout), generative AI developers would face immediate licensing demands or litigation risk; transformative creators would need clearance for remixes and parodies; research institutions would need licenses for TDM; platform operators would need to deploy aggressive content filtering. The generative technology ecosystem would reorganize around licensing regimes, opt-out registries, or jurisdictional arbitrage. The world rearranges substantially.
% FOUNDING_PROBLEM: The pre-digital derivative work right was designed for fixed recastings (translations, adaptations, abridgments) where the new work substitutes for the original. Digital and computational uses (ML training, search indexing, text mining, generative synthesis) create intermediate copies and transformative outputs that do not substitute for the original expression. The founding problem: how to prevent the derivative work right from becoming a veto on computational and transformative uses that the statute did not contemplate and that do not threaten the original's market.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by multiple independent sources outside the beneficiary set: (1) Legislative history of the 1976 Copyright Act shows Congress contemplated 'new forms of expression' but could not foresee computational uses (House Report 94-1476). (2) The EU DSM Directive (2019) Article 3-4 created a specific TDM exception, confirming the founding problem exists and requires legislative solution, not just judicial construction. (3) The US Copyright Office's 2023-2024 AI initiative acknowledges the derivative work boundary is unsettled for generative AI. (4) Economic analyses (e.g., Lemley & Casey 2020, 'Fair Learning') argue the founding problem is real and the coordination reading is a defensible solution. No single beneficiary party controls this corroboration.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the coordination reading imposes no ex-ante transfer — no licensing fees, no clearance costs for transformative use. The minimal extraction reflects enforcement costs (DMCA notices, litigation risk) not rent extraction. Suppression is very low (0.08): alternatives (voluntary licensing, collective licensing, opt-out registries) exist and are not legally foreclosed; the constraint does not actively suppress them. Theater ratio is low (0.15): the boundary has real operational content (courts do distinguish transformative from substitutive use) though some performative 'fair use theater' exists in litigation. Accessibility collapse is low (0.25): the boundary is contested and alternatives remain viable. Resistance is moderate (0.35): enclosure-advocates actively litigate and lobby to narrow the transformative exception. The claim/metric independence is maintained: the constraint is CLAIMED as rope from this reading's structural position; metrics describe its actual low-extraction, low-suppression operation. If the kernel were authored as a mountain (natural law boundary), the declared beneficiaries would trigger FSM — hence the omega documenting the ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (developers, creators, researchers), the constraint is a genuine rope: it solves the coordination problem of 'can I build on this corpus?' with a clear, low-cost rule. From the excluded seats (copyright holders, CMOs), the same constraint appears as a snare or tangential extraction: their expressive assets are used without compensation, and the 'transformative' label feels like a cover for commercial exploitation. From the agenda-setter seat (courts), the constraint is a contested interpretive problem: the statutory text ('derivative work') is the kernel, and the coordination reading is one of several coherent constructions. The engine computes per-seat classification from the structural data; this divergence is the measurement, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Generative AI developers, transformative creators, research institutions, and platform operators are structural beneficiaries (d ≈ 0.1–0.2): the constraint subsidizes their activity by removing clearance friction. Copyright holders and CMOs are excluded from the coordination benefit and bear uncompensated use (d ≈ 0.6–0.7 under enclosure reading, but d ≈ 0.3–0.4 under coordination reading since the constraint does not actively extract from them — it merely fails to grant them a control right). Courts/legislatures are agenda setters (d ≈ 0.5 symmetric): they bear the cost of adjudication but control the boundary. The directionality derivation from beneficiary/victim declarations plus exit options produces this gradient: beneficiaries have arbitrage/mobile exit (can route around restrictive jurisdictions), excluded parties have constrained exit (bound by jurisdiction).
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination reading prevents mislabeling the transformative use exception as pure extraction (snare) by demonstrating its coordination function: it enables generative technologies, research, and transformative creativity that would be paralyzed by ex-ante licensing. It prevents mislabeling as mountain by acknowledging the boundary is a statutory construction, not a natural law — the kernel is contested, and the coordination reading is one defensible construction among others. The mandatrophy risk is that if the founding problem (enabling transformative use without clearance friction) is solved by alternative mechanisms (collective licensing, statutory opt-out, AI-specific regimes), the coordination reading could become a scaffold with implicit sunset — tracked by omega_enclosure_reading_pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural coordination boundary, or a constructed reading of a contested statutory kernel?',
    'Legislative history analysis, judicial citation networks, and cross-jurisdictional statutory comparison to determine whether the narrow ''fixed recasting'' construction reflects original legislative intent or a judicial construction that benefits identifiable actors.',
    'If constructed, the constraint is a false summit candidate (tangled_rope or scaffold) masking beneficiary structure; if natural coordination boundary, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the coordination reading reflects the kernel''s intrinsic structure or a beneficiary-favoring construction.').

omega_variable(
    transformative_boundary_operationalization,
    'Where does the coordination reading draw the line between transformative intermediate use and infringing fixed recasting in practice?',
    'Case law synthesis across jurisdictions on ML training, style transfer, and generative output similarity thresholds; empirical measurement of how courts operationalize ''substantial incorporation of original expression''.',
    'If the boundary is operationally unstable, the constraint''s coordination function degrades into contested extraction (tangled_rope); if stable, low-ε rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformative_boundary_operationalization, empirical, 'Operational stability of the transformative/infringing boundary under the coordination reading.').

omega_variable(
    enclosure_reading_pressure,
    'Does the enclosure reading''s broader derivative work definition create structural pressure that narrows the coordination reading''s safe harbor over time?',
    'Citation network analysis: track whether courts adopting enclosure logic narrow transformative use exceptions in subsequent decisions; legislative lobbying records for statutory amendment expanding derivative work right.',
    'If enclosure reading exerts ratcheting pressure, the coordination reading is a scaffold with implicit sunset, not a stable rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_reading_pressure, empirical, 'Whether competing readings create structural drift against the coordination reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(deri_tr_t0, observed).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement_basis(deri_tr_t5, observed).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(deri_tr_t10, observed).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(deri_tr_t15, observed).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(deri_tr_t20, observed).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(deri_tr_t25, observed).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(deri_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(deri_be_t0, observed).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 5, 0.07).
narrative_ontology:measurement_basis(deri_be_t5, observed).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement_basis(deri_be_t10, observed).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 15, 0.1).
narrative_ontology:measurement_basis(deri_be_t15, observed).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement_basis(deri_be_t20, observed).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement_basis(deri_be_t25, observed).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement_basis(deri_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement_basis(deri_su_t0, observed).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 5, 0.04).
narrative_ontology:measurement_basis(deri_su_t5, observed).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement_basis(deri_su_t10, observed).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 15, 0.06).
narrative_ontology:measurement_basis(deri_su_t15, observed).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 20, 0.07).
narrative_ontology:measurement_basis(deri_su_t20, observed).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 25, 0.08).
narrative_ontology:measurement_basis(deri_su_t25, observed).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement_basis(deri_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__coordination_reading, 0.02).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, generative_ai_training_data_access).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, text_data_mining_exception).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine_scope).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, copyright_opt_out_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the derivative_work_statutory_boundary kernel into three structurally distinct readings with different ε, beneficiaries, and classification. The coordination_reading (this story) has ε ≈ 0.12 (rope). The enclosure_reading has ε ≈ 0.65 (snare/tangled_rope: copyright holders benefit, transformative users pay). The hybrid_carveout_reading has ε ≈ 0.35 (tangled_rope: coordination for non-commercial, extraction for commercial). All three share the same statutory kernel but instantiate different constraints. The coordination reading influences the hybrid carveout (non-commercial safe harbor overlaps) and is foreclosed by the enclosure reading in any single legal framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__coordination_reading, institutional, 0.15).
constraint_indexing:directionality_override(derivative_work_statutory_boundary__coordination_reading, organized, 0.2).
constraint_indexing:directionality_override(derivative_work_statutory_boundary__coordination_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
