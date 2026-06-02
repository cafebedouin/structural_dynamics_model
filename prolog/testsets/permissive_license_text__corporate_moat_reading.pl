% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Corporate Moat: Uncompensated Extraction Reading
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   The permissive license text (MIT, Apache 2.0, BSD) is the kernel — a
 *   stabilized commitment that different parties read as enabling different
 *   structural outcomes. This story instantiates the CORPORATE MOAT READING:
 *   permissive licensing creates an asymmetric extraction mechanism where
 *   individual open source maintainers contribute code without compensation,
 *   and enterprise corporations build proprietary derivative products that
 *   capture margin from the unpaid labor. The constraint operates at the
 *   civilizational scale (software infrastructure) but manifests acutely at
 *   the individual level (burnout of unmaintained dependencies). The
 *   corporate moat reading frames this as a snare because: (1) individual
 *   maintainers are trapped — they cannot exit without destroying community
 *   value, cannot enforce compensation despite law recognizing intellectual
 *   property, and face social/reputational pressure to accept corporate use;
 *   (2) suppression mechanisms are active and intensifying — corporate
 *   incentives to avoid contribution obligations, maintainer burnout cycles,
 *   and ecosystem dysfunction from under-resourced dependencies all sustain
 *   the lock; (3) extraction is asymmetric — corporations capture significant
 *   margin on closed-source derivatives while maintainers receive no
 *   compensation. The rising extractiveness trajectory (0.28 → 0.58) reflects
 *   corporate adoption deepening over the interval; rising suppression (0.35
 *   → 0.62) reflects burnout and ecosystem fragmentation accumulating;
 *   falling theater ratio (0.55 → 0.48) reflects that corporate use of
 *   permissive code has become normalized, reducing the performative
 *   justification work (license text is accepted as sufficient without moral
 *   framing).
 *
 * KEY AGENTS:
 *   - Individual Open Source Maintainers: Primary victims (powerless/trapped) — unpaid contributors to projects; cannot exit without destroying community value; subject to uncompensated extraction via corporate derivatives
 *   - Enterprise Corporations: Primary beneficiaries (institutional/arbitrage) — integrate open source code freely; build proprietary derivatives; capture margin without compensation obligation or contribution requirement
 *   - Mid-Market / Smaller Enterprises: Secondary beneficiary but also constrained (moderate/constrained) — depend on ecosystem health but also perpetuate burnout cycle through use without contribution
 *   - Open Source Ecosystem: Organized collective victim (organized/constrained) — benefits from permissive integration but bears sustainability cost through maintainer burnout and dependency degradation
 *   - Package Managers and Distribution Systems: Institutional actors (institutional/arbitrage) — benefit from permissive licensing enabling broad integration; also bear indirect cost when dependencies become unmaintained
 *   - Analytical Observer (This Reading): Civilizational position (analytical/analytical) — identifies structural asymmetry as extraction mechanism sustained by suppression (burnout, powerlessness) and legitimized through license-text framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.58).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.62).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Corporate Moat: Uncompensated Extraction Reading").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '1d6383d9-e8fc-40ca-8227-f13f2e568588').
narrative_ontology:cs_kernel_codification('1d6383d9-e8fc-40ca-8227-f13f2e568588', fixed_text).
narrative_ontology:cs_authority_grounding('1d6383d9-e8fc-40ca-8227-f13f2e568588', extraction).
narrative_ontology:cs_interpretation_layer_present('1d6383d9-e8fc-40ca-8227-f13f2e568588').
narrative_ontology:cs_reading_relation('1d6383d9-e8fc-40ca-8227-f13f2e568588', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d6383d9-e8fc-40ca-8227-f13f2e568588', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('1d6383d9-e8fc-40ca-8227-f13f2e568588', foundational, contribution_should_entail_compensation).
narrative_ontology:cs_axiom_status(contribution_should_entail_compensation, holdable).
narrative_ontology:cs_axiom_grounding('1d6383d9-e8fc-40ca-8227-f13f2e568588', contribution_should_entail_compensation, deontological).
narrative_ontology:cs_axiom('1d6383d9-e8fc-40ca-8227-f13f2e568588', secondary, corporate_extraction_via_permissive_license_constitutes_unfair_asymmetry).
narrative_ontology:cs_axiom_status(corporate_extraction_via_permissive_license_constitutes_unfair_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('1d6383d9-e8fc-40ca-8227-f13f2e568588', corporate_extraction_via_permissive_license_constitutes_unfair_asymmetry, empirically_contingent).
narrative_ontology:cs_reference_frame('1d6383d9-e8fc-40ca-8227-f13f2e568588', individual_contribution_reciprocity_norm).
narrative_ontology:cs_drift_state('1d6383d9-e8fc-40ca-8227-f13f2e568588', contemporary_corporate_adoption_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1d6383d9-e8fc-40ca-8227-f13f2e568588', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, proprietary_derivative_producers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, open_source_ecosystem_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL MAINTAINER (SNARE) — Trapped in a structural position where their unpaid labor is extracted via proprietary derivatives. Cannot exit without abandoning the project; cannot enforce compensation; faces reputational pressure to accept corporate use. Maximum extraction: free labor fuels corporate products with no reciprocal contribution required.
constraint_indexing:constraint_classification(permissive_license_text__corporate_moat_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENTERPRISE CORPORATION (ROPE) — Experiences the permissive license as pure coordination: integrates open source freely, ships proprietary enhancements, captures margin. Net beneficiary with exit options: can fork, replace dependencies, or negotiate custom licensing if needed. The constraint coordinates their product strategy with minimal friction.
constraint_indexing:constraint_classification(permissive_license_text__corporate_moat_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-MARKET ENTERPRISE (TANGLED ROPE) — Benefits from permissive licensing (lowers integration cost, accelerates time-to-market) but also constrained by dependency lock-in and community health erosion. If maintainers burn out, dependency quality declines. Mixed experience: coordination benefit offset by extraction of value through sustainability externalities.
constraint_indexing:constraint_classification(permissive_license_text__corporate_moat_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: OPEN SOURCE ECOSYSTEM (TANGLED ROPE) — Organized actors (package managers, Linux distributions, foundations) benefit from permissive code (low friction integration, broad adoption) but bear the cost of maintainer burnout, security vulnerabilities, and sustainability crises. Genuine coordination function (shared code base reduces duplication) offset by asymmetric extraction of ecosystem labor.
constraint_indexing:constraint_classification(permissive_license_text__corporate_moat_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CORPORATE MOAT READING (SNARE) — From a civilization-scale view, the permissive license text creates a structural asymmetry: individuals contribute without compensation; corporations extract margin from proprietary derivatives. The reading frames this as a snare: suppression mechanisms (contributor burnout, community health erosion, lack of enforcement infrastructure) sustain the extraction. High extractiveness, high suppression, functional enforcement (corporations actively defend derivative products against community control).
constraint_indexing:constraint_classification(permissive_license_text__corporate_moat_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(permissive_license_text__corporate_moat_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(permissive_license_text__corporate_moat_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The corporate moat reading quantifies extraction as the margin corporations capture on derivatives minus contributions they make back to base projects. Empirically, large technology companies (cloud platforms, AI frameworks, databases) extract significant value from permissive open source libraries with minimal reciprocal contribution. This is not maximal extraction (snares can reach 0.72+) because corporations do make some contributions and the system does produce genuine coordination value. Suppression (0.62): High. The suppression mechanisms are: (1) Maintainer burnout — unpaid labor extraction is psychologically and economically unsustainable, creating high barrier to exit; (2) Powerlessness — individual maintainers lack enforcement mechanisms; permissive licenses explicitly waive the right to demand compensation; (3) Network effects and switching costs — once integrated, dependencies are hard to replace; communities locked in by adoption; (4) Social pressure — contributors feel obligated to accept corporate use for the 'greater good' of adoption; (5) Career risk — demanding compensation or imposing license change can damage reputation and adoption. These mechanisms are reinforced by corporate incentives to discourage alternative arrangements (copyleft, contribution requirements). Theater ratio (0.48): Moderate-low. The permissive license text functions relatively directly — the legal instrument is not heavily theatrical. However, there is some performative element: the narrative of 'open source as voluntary community' obscures the structural extraction; corporate adoption framing emphasizes freedom/openness rather than uncompensated labor dependency.
 *
 * PERSPECTIVAL GAP:
 *   The gap between readings is stark. The corporate moat reading (this story) identifies a snare structure; the commons coordination reading (sibling story) sees the same permissive license as enabling global collaboration without restrictive copyleft burdens; the copyleft counterfactual reading asks whether GPL enforcement would reduce extraction but also reduce adoption. From the corporate moat perspective, adoption is a sign of extraction (corporations benefit from free labor). From the commons reading, adoption is a sign of coordination success (code is used, communities form). The perspectival gap reveals that the kernel (license text) is genuinely ambiguous: it can be read as enabling either outcome, and actual outcome depends on institutional context (corporate incentive structures, maintainer power, community governance, payment arrangements). The analytical observer in this reading identifies the snare; the analytical observer in the commons reading identifies rope. No perspective is wrong — they are reading different structural implications from the same text under different assumption sets about how power operates.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes the agent's structural position within the constraint. Individual maintainers occupy d ≈ 0.95 (full target of extraction: they provide value, receive nothing); corporations occupy d ≈ 0.10 (full beneficiary: receive value, provide minimal reciprocal obligation); analytical observer at d ≈ 0.73 (observer position, sees asymmetry). The sigmoid f(d) maps these to effective extraction experienced: maintainers experience χ elevated, corporations experience χ negative (they benefit). The constraint distributes differently across the two populations — same base ε, but opposite directionality flow creates opposite perceived classifications (snare vs rope). Enterprise corporations with constrained exit (mid-market) occupy intermediate d ≈ 0.50, experiencing tangled rope because they both benefit and constrain each other through ecosystem effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: is the permissive license text a mechanism for extraction (snare) or for coordination (rope)? The corporate moat reading resolves this by showing that the same mechanism produces opposite outcomes for different agents — beneficial coordination for beneficiaries (corporations), extraction for victims (maintainers). The classification snare is justified because: (1) the asymmetry is structural (maintained by enforcement of corporate IP rights while denying symmetrical rights to maintainers); (2) the suppression is high (burnout, powerlessness, network lock-in prevent exit); (3) the extraction is asymmetric (margin flows to corporations, labor flows from maintainers, no reciprocal obligation). This reading does NOT claim permissive licensing is evil or should be prohibited — it claims the actual structural outcome (uncompensated extraction) should be named accurately rather than framed as voluntary coordination or inherent to open source culture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    license_text_kernel_ambiguity,
    'Does the permissive license text itself create extraction, or does the reading of the text create a narrative of extraction superimposed on a coordination mechanism?',
    'Comparison of three sibling readings (corporate_moat, commons_coordination, copyleft_counterfactual) and their terminal attractors under identical corporate adoption scenarios. If all three readings produce different sustained equilibria, the ambiguity is genuine (kernel is truly contested). If one reading''s equilibrium is stable while others degrade, the kernel resolves toward that reading.',
    'If kernel is genuinely ambiguous: permissive license text is a coordination-extraction hybrid with rational interpretation multiplicity. If one reading dominates: the others are either false-summit naturalizations or aspirational alternatives that market dynamics eliminate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(license_text_kernel_ambiguity, conceptual, 'Whether extraction is intrinsic to permissive license text or an interpretation imposed on coordination').

omega_variable(
    individual_maintainer_volition,
    'To what extent do individual maintainers trap themselves by choosing permissive licenses and accepting uncompensated corporate use, versus being trapped by structural constraints (career risk of changing terms, network effects locking choices in place)?',
    'Empirical tracking of maintainers'' stated reasons for license choice; analysis of switching costs for changing license terms mid-project; comparison with maintainers who successfully implemented contributor agreements or dual licensing; survey of counterfactual preferences (''would you impose copyleft if you could costlessly change the license?'').',
    'If self-trapping: suppression metric should be lower — the constraint relies on voluntary participation that could be revoked. If structural trapping: suppression metric justified — exit costs are real and external. Affects moral status of ''snare'' vs ''mutual coordination with unequal power''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_maintainer_volition, empirical, 'Degree to which maintainers are structurally trapped vs self-entrapped by license choice').

omega_variable(
    derivative_value_distribution,
    'What proportion of corporate margin on derivative products is attributable to the permissive base library versus proprietary enhancements?',
    'Case-by-case analysis of derivative products (cloud platforms, database systems, AI frameworks) with transparent cost accounting; customer surveys on value attribution; competitive analysis of derivative vs. base library pricing power.',
    'If margin attribution is low (≤20%): permissive license enables value creation but corporations are not primarily extracting through uncompensated base code. If attribution is high (≥40%): uncompensated extraction narrative is empirically justified. Affects epsilon and chi calibration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivative_value_distribution, empirical, 'Proportion of corporate derivative margin attributable to permissive base library').

omega_variable(
    corporate_moat_reading_axiom_hold,
    'Is the foundational axiom of the corporate moat reading (corporations should compensate for extracted value) a holdable normative claim in contemporary software governance, or has it been overridden by market acceptance of permissive licensing?',
    'Tracking of policy proposals, licensing disputes, and institutional position changes: does compensation expectation persist in maintainer discourse despite market evolution? Do new entrants to open source expect compensation or accept volunteer model? Institutional analysis of GPL advocacy communities versus permissive license adoption curves.',
    'If axiom remains holdable: the corporate moat reading describes a genuine violation that could be structurally corrected (copyleft enforcement, mandatory contribution agreements). If axiom is overridden: the reading describes a defeated normative claim that market dynamics have settled, and the piton or rope readings become more accurate characterizations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_moat_reading_axiom_hold, conceptual, 'Whether compensation expectation axiom remains live in contemporary software governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_corp_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(perm_corp_tr_t5, permissive_license_text__corporate_moat_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(perm_corp_tr_t10, permissive_license_text__corporate_moat_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(perm_corp_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(perm_corp_be_t5, permissive_license_text__corporate_moat_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(perm_corp_be_t10, permissive_license_text__corporate_moat_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(perm_corp_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(perm_corp_su_t5, permissive_license_text__corporate_moat_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(perm_corp_su_t10, permissive_license_text__corporate_moat_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, software_dependency_supply_chain_fragility).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, open_source_maintainer_burnout).

% DUAL FORMULATION NOTE:
% The permissive license text is a kernel with three sibling readings instantiated as separate constraint stories. The corporate moat reading identifies uncompensated extraction; the commons reading identifies coordination value; the copyleft counterfactual identifies adoption-reciprocity tradeoff. The three stories are not contradictory — each identifies a real structural feature of the same license text under different institutional contexts. The affects_constraints edges link this reading to sibling readings and downstream constraints (maintainer burnout, supply chain fragility) that this reading's structural mechanism produces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
