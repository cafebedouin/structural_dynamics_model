% ============================================================================
% CONSTRAINT STORY: permissive_software_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_software_licensing, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: permissive_software_licensing
 *   human_readable: Permissive Software Licensing (MIT, Apache, BSD)
 *   domain: technological/legal/economic
 *
 * SUMMARY:
 *   Permissive software licenses (MIT, Apache 2.0, BSD) emerged in the 1980s
 *   as a mechanism to reduce licensing friction in software development. They
 *   enable users to freely copy, modify, and redistribute code with minimal
 *   restrictions—often only requiring attribution. The constraint structure
 *   exhibits a fundamental tension between coordination function (enabling
 *   rapid integration and innovation) and extraction mechanism (enabling
 *   commercial vendors to capture value from unpaid open-source labor). This
 *   tension is not inherent to software economics but is produced by specific
 *   structural asymmetries: the lack of enforceable reciprocity, the
 *   inability of maintainers to claim derivative value, and the concentration
 *   of platform power in commercial vendors. The permissive licensing regime
 *   has become the dominant model globally, creating a global commons whose
 *   value is systematically extracted by organized commercial actors while
 *   maintenance burden concentrates on volunteer maintainers and downstream
 *   developers. The constraint is neither purely extractive (it does solve
 *   real coordination problems) nor purely coordinative (it enables
 *   significant asymmetric extraction). The theater ratio (0.35 at present,
 *   rising over time) reflects the increasing gap between the stated
 *   coordination function ('open source enables rapid innovation') and the
 *   structural reality ('permissive licensing enables value capture by
 *   commercial vendors from volunteer labor'). The academic software
 *   maintenance perspective reveals piton degradation: the open-source norm
 *   persists through institutional inertia and virtue signaling rather than
 *   through genuine sustainability of the maintenance model.
 *
 * KEY AGENTS:
 *   - Open Source Maintainers: Trapped victims (powerless/trapped) — bear unpaid labor burden while gains accrue to commercial users; cannot prevent appropriation
 *   - Downstream Open Source Projects: Constrained victims (moderate/constrained) — face resource starvation as value accumulates in commercial vendors; exit options are expensive or reputation-damaging
 *   - Commercial Vendors: Institutional beneficiaries (institutional/arbitrage) — extract value from permissive code without reciprocal obligation; have abundant exit options and no suppression
 *   - Large Platform Operators: Organized extractors (organized/constrained) — simultaneously benefit from permissive code and gatekeep value through platform lock-in; require active enforcement of veneer of open-source contribution
 *   - Open Source Ecosystem Collective: Organized coordinators (organized/constrained) — achieve genuine coordination benefits through permissive licensing ecosystem; exhibit rope-like properties when viewed as collective
 *   - Academic Software Researchers: Institutional degraders (institutional/mobile) — maintain piton-like open-source publication norm through institutional inertia despite unsustainable maintenance burden
 *   - Analytical Observer: Civilizational naturalizer (analytical/analytical) — risks framing the extraction regime as an inevitable law of software economics rather than as a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_software_licensing, 0.38).
domain_priors:suppression_score(permissive_software_licensing, 0.25).
domain_priors:theater_ratio(permissive_software_licensing, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_software_licensing, extractiveness, 0.38).
narrative_ontology:constraint_metric(permissive_software_licensing, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(permissive_software_licensing, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_software_licensing, tangled_rope).
narrative_ontology:human_readable(permissive_software_licensing, "Permissive Software Licensing (MIT, Apache, BSD)").
narrative_ontology:topic_domain(permissive_software_licensing, "technological/legal/economic").

domain_priors:requires_active_enforcement(permissive_software_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_software_licensing, closed_source_commercial_users).
narrative_ontology:constraint_beneficiary(permissive_software_licensing, dominant_platform_vendors).
narrative_ontology:constraint_beneficiary(permissive_software_licensing, early_adopter_firms).
narrative_ontology:constraint_victim(permissive_software_licensing, open_source_maintainers).
narrative_ontology:constraint_victim(permissive_software_licensing, derivative_project_developers).
narrative_ontology:constraint_victim(permissive_software_licensing, commons_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPEN SOURCE MAINTAINER (SNARE) — Trapped in a structural dynamic where their unpaid labor is extracted via license permissiveness. The maintainer cannot prevent commercial appropriation, has minimal exit (forking requires reputational cost and abandons the primary codebase), and bears the full cost of maintenance while gains accrue to commercial users. High suppression: no legally enforceable claim to derivative value; social norms against forking; reliance on commercial ecosystem for adoption signals.
constraint_indexing:constraint_classification(permissive_software_licensing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM OPEN SOURCE PROJECT (SNARE) — A derivative project that builds on permissive-licensed code faces asymmetric extraction. Upstream libraries capture value through dependency relationships without obligation to support downstream. Exit options are constrained: rewriting is expensive; the permissive license creates expectation of free availability; switching to proprietary tools risks community backlash. The constraint enforces unpaid labor propagation through the open-source supply chain.
constraint_indexing:constraint_classification(permissive_software_licensing, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: COMMERCIAL VENDOR (ROPE) — Experiences permissive licensing as pure coordination with benefit. The vendor can freely integrate, modify, and redistribute without attribution or sharing obligations. Exit options are abundant: use the library, use a competitor library, build proprietary equivalents. The constraint solves real coordination problems: enabling rapid software assembly without licensing friction. No suppression experienced — the vendor has full agency and maximum extraction benefit.
constraint_indexing:constraint_classification(permissive_software_licensing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE PLATFORM OPERATOR (TANGLED ROPE) — Simultaneously benefits from permissive code (enabling rapid development, reduced licensing costs) and contributes to open source (Azure, AWS services, Google Cloud maintain significant open-source projects). But the relationship is extractive: platforms use permissive-licensed code to build proprietary lock-in services, then gatekeep their own open-source projects through cloud infrastructure dependencies. Active enforcement required: maintaining the veneer of open-source contribution while capturing value through platform effects. Moderate suppression through cloud vendor lock-in and subtle API changes that disadvantage local deployment.
constraint_indexing:constraint_classification(permissive_software_licensing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SOURCE ECOSYSTEM COLLECTIVE (ROPE) — When viewed as an organized movement, the permissive licensing regime enables massive coordination: Linux, Git, PostgreSQL, Apache, NumPy, and thousands of projects coexist and interoperate with minimal licensing friction. The ecosystem solves genuine coordination problems: avoiding license incompatibility, enabling rapid innovation, reducing transaction costs. Theater is low — the mechanism is genuinely functional. Suppression emerges only when the collective tries to enforce reciprocity (GPL), which it cannot do within the permissive framework.
constraint_indexing:constraint_classification(permissive_software_licensing, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC SOFTWARE MAINTENANCE (PITON) — University-developed research software (scientific computing, ML frameworks) is increasingly released under permissive licenses as a degraded form of the academic knowledge-sharing ethos. The performative function is 'advancing science through open code'; the structural reality is that maintainers have limited career incentives beyond publication, and commercial companies capture value without reciprocal contribution. Theater ratio is high (0.65+): academic conferences celebrate 'open science,' but the maintenance burden is unsustainable and is transferring to commercial vendors. Exit options exist (researchers can switch to closed source or seek commercial support), but institutional inertia maintains the permissive open-source norm as a theater of academic virtue.
constraint_indexing:constraint_classification(permissive_software_licensing, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — The analytical view at civilizational scale risks naturalizing the permissive licensing regime as an inevitable feature of software economics ('code wants to be free,' 'information has zero marginal cost'). This framing treats the current equilibrium as a law of nature rather than as a contingent institutional arrangement sustained by specific power asymmetries and career incentive structures. The engine's false summit detector identifies this: the 'inevitability' framing naturalizes what is actually a contestable choice enforced through social and economic mechanisms.
constraint_indexing:constraint_classification(permissive_software_licensing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_software_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(permissive_software_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(permissive_software_licensing, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(permissive_software_licensing, TR),
    TR >= 0.70.

:- end_tests(permissive_software_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The permissive licensing regime enables significant value extraction—commercial vendors build proprietary services on permissive code without reciprocal obligation—but it is not extreme because the licensing mechanism itself is non-coercive. No vendor is forced to use permissive code; the extraction occurs through incentive structure rather than prohibition. The rising trajectory (0.15 → 0.38 over 30 years) reflects increasing platform concentration: as commercial vendors grow larger and more dependent on open-source code, the asymmetry between their market power and maintainers' bargaining power has increased. Suppression (0.25): Low-moderate. Maintainers are not legally prevented from forking, using reciprocal licensing, or seeking commercial support. However, social norms (celebrating 'pure open source'), network effects (established projects create high switching costs), and institutional factors (academic career incentives for open publication) all suppress alternatives. The suppression is weaker than in snares (where legal or physical barriers dominate) but stronger than in ropes (where all parties experience low friction). Theater ratio (0.35, rising to 0.35 at T=30): Moderate. The functional mechanism is real—permissive licensing does reduce friction and enable integration—but an increasing share of the ecosystem's activity is performative: corporate open-source initiatives that create the appearance of contribution while extracting value, academic open-source software that signals virtue while degrading unsustainably, and the rhetoric of 'open source as inevitable' that naturalizes extraction asymmetries. The theater trajectory shows increasing divergence between stated function ('enabling rapid innovation') and structural reality ('concentrating value in commercial vendors').
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is wide and reveals the constraint as tangled rope. The commercial vendor perceives pure coordination (Rope)—the license removes friction and enables rapid development. The platform operator perceives a complex hybrid (Tangled Rope)—they benefit from upstream permissive code but must maintain the appearance of open-source contribution to preserve ecosystem access and reputation. The open-source maintainer perceives pure extraction (Snare)—they provide unpaid labor and cannot prevent its appropriation. The downstream project perceives mixed coordination and extraction (Tangled Rope)—permissive licensing enables integration but also enables value to flow away from them toward commercial vendors. The ecosystem collective perceives coordination (Rope) because the permissive regime does enable massive integration and innovation. The academic software perspective perceives degradation (Piton)—the norm persists through institutional inertia despite unsustainability. The civilizational analytical observer risks perceiving a natural law (Mountain) but this is a false summit: the regime is a contingent institutional choice, not an inevitable feature of software economics. The perspectival gaps drive the mandatrophy resolution: the constraint is neither simply extractive nor simply coordinative, but both simultaneously, experienced asymmetrically across structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation follows from structural position. Maintainers (powerless/trapped) derive maximum d ≈ 0.95, corresponding to high f(d) and high experienced extraction. Downstream projects (moderate/constrained) derive d ≈ 0.65-0.75, intermediate extraction. Commercial vendors (institutional/arbitrage) derive d ≈ 0.05-0.15 (beneficiary with exit options), producing negative or near-zero f(d) and subsidized effective extraction (they pay via the formula, not the maintainers). Platform operators (organized/constrained) derive d ≈ 0.40-0.50, reflecting their hybrid position: they benefit from permissive upstream code but are constrained by ecosystem reputation. The analytical observer (analytical/analytical) derives d ≈ 0.70, the canonical fallback for analytical agents, reflecting their abstract position. No directionality overrides are needed—the derivation chain produces the correct structural relationships from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY: Is permissive licensing a coordination mechanism (solving a genuine collective action problem, justifying some extractive overhead as the cost of coordination) or a pure extraction mechanism (enabling value capture under the guise of coordination rhetoric)? RESOLUTION: The constraint is genuinely hybrid—it solves real coordination problems (reducing license friction, enabling rapid assembly) AND enables significant extraction (concentrating value in organized commercial actors). The mandatrophy is resolved by recognizing that BOTH functions are structural, not contextual. Permissive licensing provides genuine coordination benefit that Rope alone cannot explain (reducing transaction costs for integration). It simultaneously enables extraction asymmetry that Rope cannot explain (commercial vendors capture value without reciprocal obligation). The Tangled Rope classification unifies both: the regime is an active-enforcement hybrid where the coordination function (integration, friction reduction) is genuine but the extraction function (value concentration, uncompensated labor) is also genuine. The architectural feature that produces both: the absence of reciprocity requirements. This creates coordination (no attribution requirement simplifies integration) and enables extraction (no obligation to support downstream or share derivative improvements). The theater ratio (0.35) indicates that the 'open source as inevitable' rhetoric has become increasingly performative as the extraction asymmetry has grown visible. The regime persists not because reciprocity is impossible in software (GPL proves it is possible) but because organized commercial actors benefit from permissive licensing and can sustain the norm through market power, institutional inertia, and the strategic deployment of open-source contributions to maintain ecosystem access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_rider_tipping_point,
    'What level of commercial extraction from open-source causes maintainers to collectively switch from permissive to reciprocal licensing (or proprietary models)?',
    'Historical analysis of GPL adoption rates vs commercial value extracted from permissive code; survey data on maintainer decisions post-acquisition or platform monetization',
    'If tipping point is real and approaching: permissive regime is unstable and will degrade to GPL or proprietary licensing. If maintainers accept extraction indefinitely: regime is stable (though unfair).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_tipping_point, empirical, 'At what extraction threshold do maintainers abandon permissive licensing').

omega_variable(
    corporate_contribution_counterfactual,
    'Would commercial vendors contribute to open source at current rates if permissive licensing required attribution or reciprocal benefit-sharing?',
    'Comparative analysis of GPL project contributions vs MIT/Apache contributions by corporate donors; counterfactual modeling of licensing friction impact on corporate participation',
    'If corporations contribute primarily for permissive-license benefits: extraction mechanism is confirmed; if contributions are robust to licensing terms: extraction is secondary to business logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_contribution_counterfactual, empirical, 'Corporate open-source participation under stricter licensing terms').

omega_variable(
    derivative_value_capture_asymmetry,
    'Can downstream open-source projects sustain long-term development under permissive licensing, or does value accumulation in commercial vendors doom them to resource starvation?',
    'Longitudinal tracking of derivative project health metrics (commit frequency, contributor diversity, maintenance backlog) vs upstream resource capture; case studies of successful vs failed downstream projects',
    'If derivative projects systematically degrade: extractive mechanism is confirmed structurally. If many thrive: permissive licensing supports vibrant ecosystem despite extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_value_capture_asymmetry, empirical, 'Long-term viability of downstream open-source projects').

omega_variable(
    coordination_vs_extraction_primacy,
    'Is permissive licensing primarily a coordination mechanism (enabling integration, reducing friction) or primarily an extraction mechanism (enabling value capture by commercial vendors)?',
    'Counterfactual comparison: measure ecosystem health, innovation rate, and code quality under permissive vs reciprocal (GPL) licensing regimes; analyze whether friction-reduction or value-capture is the binding constraint on ecosystem growth',
    'If coordination primacy: classification is Rope (or Rope-dominant Tangled Rope). If extraction primacy: classification is Snare-dominant Tangled Rope. This resolves the core mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_primacy, empirical, 'Whether permissive licensing is coordination or extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_software_licensing, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_software_licensing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perm_tr_t15, permissive_software_licensing, theater_ratio, 15, 0.28).
narrative_ontology:measurement(perm_tr_t30, permissive_software_licensing, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_software_licensing, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(perm_be_t15, permissive_software_licensing, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(perm_be_t30, permissive_software_licensing, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_software_licensing, information_standard).
narrative_ontology:affects_constraint(permissive_software_licensing, open_source_sustainability).
narrative_ontology:affects_constraint(permissive_software_licensing, software_license_lock_in).
narrative_ontology:affects_constraint(permissive_software_licensing, corporate_open_source_performativity).

% DUAL FORMULATION NOTE:
% Permissive licensing is decomposed from the broader 'open source licensing regime' into a specific constraint focused on the coordination-extraction hybrid produced by the absence of reciprocity requirements. Upstream constraint: the institutional choice to privilege integration friction reduction over downstream sustainability. Downstream constraints: the effects on maintainer labor markets, on derivative project sustainability, and on the visibility of corporate value extraction through license terms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
