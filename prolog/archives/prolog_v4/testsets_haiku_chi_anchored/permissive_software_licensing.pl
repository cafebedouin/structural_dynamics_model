% ============================================================================
% CONSTRAINT STORY: permissive_software_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: permissive_software_licensing
 *   human_readable: Permissive Software Licenses (MIT, Apache, BSD)
 *   domain: technological/legal/economic
 *
 * SUMMARY:
 *   Permissive software licenses (MIT, Apache 2.0, BSD) create a structural
 *   tension in digital economies: they enable rapid ecosystem coordination by
 *   removing licensing friction, but simultaneously allow commercial firms to
 *   extract value from community-contributed code without obligation to
 *   contribute back, maintain attribution, or share derivative improvements.
 *   The constraint exhibits a classic Tangled Rope pattern—it combines
 *   genuine coordination benefits (enabling widespread code reuse, rapid
 *   innovation, ecosystem growth) with asymmetric extraction (commercial
 *   appropriation of community labor, suppression of alternative business
 *   models, invisibility of open source contributors in proprietary
 *   products). The base extractiveness (0.38) reflects moderate but growing
 *   value capture by commercial firms. Suppression (0.42) emerges from market
 *   concentration (dominant platforms like Apple, Google, Microsoft control
 *   distribution channels), intellectual property frameworks that prioritize
 *   commercial ownership over attribution, and organizational asymmetries
 *   (corporate legal/licensing departments vs. individual volunteers).
 *   Theater ratio (0.35) is relatively low—the constraint operates with
 *   minimal performative overlay; the extraction is straightforward and
 *   acknowledged by all parties.
 *
 * KEY AGENTS:
 *   - Original Open Source Authors: Primary victims (powerless/trapped) — release code intending commons contribution but experience loss of control, attribution erasure, and lack of compensation when commercial firms integrate and redistribute work
 *   - Open Source Community and Smaller Maintainers: Secondary victims (moderate/constrained) — benefit from ecosystem growth but constrained by inability to fund maintenance, loss of visibility when code is embedded in proprietary products, and extraction of labor value
 *   - Commercial Software Companies (Non-Platform): Primary beneficiaries (institutional/arbitrage) — experience permissive licensing as pure coordination: access to high-quality, production-grade code without licensing fees, negotiation, or reciprocal obligations
 *   - Large Technology Corporations (Platform-Owners): Enhanced beneficiaries (institutional/arbitrage) — leverage permissive code plus complementary proprietary infrastructure (platforms, data, patents) to create extraction multipliers; control distribution channels that amplify their advantage
 *   - Open Source Foundations (Linux Foundation, Apache Software Foundation, CNCF): Organized reformers (organized/constrained) — building alternative governance structures (trademark protection, maintainer funding, tiered contribution models) with sunset logic
 *   - Legal/IP Framework: Institutional actor (institutional/constrained) — copyright law and attribution requirements persist but are largely unenforceable against resource-rich defendants; framework sees itself as degraded
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (copyright law, market concentration, volunteer participation) as immutable features of digital economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_software_licensing, 0.38).
domain_priors:suppression_score(permissive_software_licensing, 0.42).
domain_priors:theater_ratio(permissive_software_licensing, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_software_licensing, extractiveness, 0.38).
narrative_ontology:constraint_metric(permissive_software_licensing, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(permissive_software_licensing, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_software_licensing, tangled_rope).
narrative_ontology:human_readable(permissive_software_licensing, "Permissive Software Licenses (MIT, Apache, BSD)").
narrative_ontology:topic_domain(permissive_software_licensing, "technological/legal/economic").

domain_priors:requires_active_enforcement(permissive_software_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_software_licensing, commercial_proprietary_software_companies).
narrative_ontology:constraint_beneficiary(permissive_software_licensing, large_technology_corporations).
narrative_ontology:constraint_beneficiary(permissive_software_licensing, downstream_integrators).
narrative_ontology:constraint_victim(permissive_software_licensing, open_source_ecosystem_sustainability).
narrative_ontology:constraint_victim(permissive_software_licensing, original_author_attribution).
narrative_ontology:constraint_victim(permissive_software_licensing, commons_preservation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL AUTHOR (SNARE) — Author releases code under permissive license intending to contribute to commons, but experiences gradual loss of control over attribution, derivative work visibility, and community participation. No contractual recourse when commercial entities strip attribution or monetize contributions. Trapped in initial choice; exit would require litigation against dominant players. d≈0.90, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(permissive_software_licensing, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OSS COMMUNITY (TANGLED ROPE) — Benefits from permissive licenses enabling rapid ecosystem growth, code reuse, and collaborative development. Simultaneously constrained by inability to prevent commercial extraction of community work, lack of funding mechanisms for maintenance, and visibility loss when code is embedded in proprietary products. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(permissive_software_licensing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMERCIAL INTEGRATORS (ROPE) — Permissive licenses solve coordination problem: companies can integrate high-quality code without negotiation, licensing fees, or reciprocal obligations. Constraint operates as pure coordination for this actor: access to free, production-grade components enables rapid development. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Negative extraction = net subsidy.
constraint_indexing:constraint_classification(permissive_software_licensing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE TECH CORPORATIONS (ROPE) — Established companies use permissive licenses as coordination mechanism: contributes to ecosystem legitimacy, attracts engineer talent ('we support open source'), and reduces licensing friction in supply chains. Also leverage complementary proprietary IP (patents, platforms, data) to extract value beyond software itself. d≈0.12, f(d)≈-0.02, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(permissive_software_licensing, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OSS FOUNDATIONS & GOVERNANCE (SCAFFOLD) — Organized actors (Linux Foundation, Apache Software Foundation, CNCF) are building alternative coordination structures with sunset logic: trademark enforcement, maintainer support programs, tiered contribution models, and reciprocal licensing frameworks (GPL variants) are creating pathways to reduce extraction. Constraint seen as temporary institutional gap being filled. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.15.
constraint_indexing:constraint_classification(permissive_software_licensing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: IP LEGAL FRAMEWORK (PITON) — Copyright law, attribution requirements, and patent frameworks persist largely as performative scaffolding. Enforcement is expensive, rare, and ineffective against actors with resources. Theater emerges from the gap between stated attribution rights and actual enforcement capacity. theater_ratio≈0.68. The legal infrastructure sees itself as degraded: designed for copy scarcity, poorly adapted to digital abundance.
constraint_indexing:constraint_classification(permissive_software_licensing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, permissive licensing is sometimes framed as reflecting an immutable property: 'information wants to be free' or 'digital goods are non-rivalrous, so restricting access is inherently costly.' This perspective naturalizes the constraint as an irreducible feature of digital economics. However, structural data (ε=0.38, suppression=0.42, theater=0.35) contradicts mountain classification—the constraint is contingent on specific IP law frameworks, cultural norms around attribution, and market power asymmetries, not inherent to digital goods themselves.
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
    constraint_indexing:constraint_classification(permissive_software_licensing, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.38): Moderate, rising. Initial permissive licensing (1980s-1990s) was genuinely coordinative—small communities sharing code, minimal commercial interest. As software became economically central (2000s-present), commercial firms discovered that permissive licensing enabled free integration of community code into proprietary products. The extraction has grown as platform firms achieved dominance: they can now embed open source components into closed ecosystems (app stores, cloud platforms) where original authors have no visibility or control. The metric reflects the growing value asymmetry without reaching snare-level suppression because alternatives (reciprocal licensing, foundation governance) exist and some commercial actors voluntarily contribute back. Suppression (0.42): Moderate-high. Significant barriers to author recourse include expensive litigation against well-resourced corporations, intellectual property law favoring commercial ownership over attribution, concentration of distribution channels in platform firms' hands, and cultural norms treating open source as 'free labor.' However, suppression is not total—some mechanisms exist (trademark enforcement, GPL enforcement, foundation support programs) and are slowly improving. Theater ratio (0.35): Low. The constraint operates with minimal performative content. Commercial integration and redistribution are transparent; attribution stripping is acknowledged and debated; licensing compliance is measurable. The theater that does exist comes from the legal framework's performative aspects—copyright law and attribution requirements persist but are rarely enforced, creating an illusion of protection without substance.
 *
 * PERSPECTIVAL GAP:
 *   The original author and open source community see pure extraction or constrained tangled rope—they contribute code and lose control. Commercial integrators and platform firms see rope or even pure subsidy—they access production-grade code without negotiation or obligation, solving coordination problems at massive scale. The legal framework sees itself as degraded (piton)—designed for copy scarcity, poorly adapted to digital abundance, enforcement-light. The analytical observer risks naturalizing this as immutable ('information wants to be free') when in fact it is contingent on specific institutional arrangements: IP law, market concentration, volunteer norms. Foundations see a temporary gap (scaffold) being filled by alternative governance and sustainability mechanisms. The perspectival gap reveals that 'permissiveness' is not neutral—it is a structural choice that advantages actors with distribution power and disadvantages individual contributors.
 *
 * DIRECTIONALITY LOGIC:
 *   Original authors: Victim + trapped → d≈0.90, f(d)≈1.38. Maximum extraction. Cannot exit the choice to open-source or claw back control once code is widely distributed. Commercial integrators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net subsidy. Can freely integrate and redistribute; exit option (not using permissive code) is costless but value-destroying. Platform corporations: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.02. Net subsidy, with additional advantage from platform control. Open source community: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal; community can organize (foundations) and has some alternatives (reciprocal licensing). Foundations: Organized + constrained → d≈0.35, f(d)≈0.35. Low effective extraction due to agency and visible exit paths. Legal framework: Institutional + constrained → d≈0.55, f(d)≈0.72. Moderate extraction reflected in piton classification (theater gate) rather than snare (enforcement gap).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that permissive licensing is genuinely coordinative (enabling ecosystem growth, solving distribution friction, enabling rapid innovation) AND genuinely extractive (from individual authors to commercial firms, from community to platform owners, from maintenance labor to shareholder value). The Tangled Rope classification captures this hybrid: χ≈0.40 reflects both coordination benefits and asymmetric extraction. The false mountain temptation ('information wants to be free') naturalizes what is actually a contingent institutional choice—copyright law + market concentration + volunteer norms = extraction. If we changed the institutional context (e.g., mandatory contribution-back clauses, platform neutrality, formalized maintainer compensation), the extraction would decline without eliminating coordination. The constraint is not immutable; it is institution-dependent. The scaffold perspective (foundations building alternatives) and the piton perspective (legal framework recognizing its own degradation) both suggest that the current equilibrium is unstable. Over a 20-30 year horizon, permissive licensing may shift toward reciprocal or tiered models as the commons sustainability problem becomes acute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_enforcement_viability,
    'Can permissive license attribution requirements be effectively enforced against commercial extraction without expensive litigation?',
    'Audit studies of proprietary software codebases for unattributed permissive-licensed components; tracking of successful DMCA, GPL, or attribution enforcement actions; cost-benefit analysis of enforcement vs damage to industry relationships',
    'If enforceable: attribution becomes real constraint on extraction, shifting commercial perspective toward Snare or Tangled Rope. If unenforceable: attribution is performative legal theater, confirming Piton classification for legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attribution_enforcement_viability, empirical, 'Whether attribution requirements can be enforced against commercial misappropriation').

omega_variable(
    commons_sustainability_mechanism,
    'What sustainable funding model prevents the collapse of widely-used permissive-licensed projects as maintenance burdens outpace volunteer capacity?',
    'Longitudinal tracking of project abandonment rates; correlation with funding models (corporate sponsorship, foundation support, dual licensing); emergence of new institutional forms (maintainer collectives, DAO structures, platform cooperation models)',
    'If commons-preserving mechanism emerges: scaffold perspective confirmed; permissive licensing becomes temporary coordination gap. If mechanism fails: victims perspective confirmed; extraction of value from community labor is structurally irreversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_sustainability_mechanism, empirical, 'Viability of sustainable funding for open source commons').

omega_variable(
    reciprocal_licensing_adoption_threshold,
    'At what critical mass does adoption of reciprocal licenses (GPL-style) or tiered contribution models (Contributor License Agreements) shift the equilibrium away from pure extraction?',
    'Analysis of GPL adoption trends; comparison of ecosystem health and contributor retention between permissive and reciprocal projects; measurement of commercial participation in GPL-licensed ecosystems',
    'If reciprocal licenses achieve critical mass: structural alternative exists, permissive licensing becomes niche choice (Rope/Scaffold). If permissive remains dominant: extraction mechanism proves robust against licensing alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_licensing_adoption_threshold, empirical, 'Whether reciprocal licensing can achieve structural parity with permissive models').

omega_variable(
    platform_dependence_asymmetry,
    'Does commercial extraction of permissive-licensed code intensify when the extracting firm controls essential digital infrastructure (app stores, cloud platforms, distribution channels)?',
    'Comparative analysis of extraction rates for permissive code in open platforms vs closed platforms; tracking of code appropriation and attribution-stripping in app ecosystems; measurement of commercial visibility advantage accruing to code owners with platform control',
    'If platform control amplifies extraction: suppression increases, commercial players shift toward Snare perspective. If extraction remains independent of platform control: suppression is structural, not dependent on market dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_dependence_asymmetry, empirical, 'How platform control amplifies extraction from permissive-licensed code').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_software_licensing, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_software_licensing, theater_ratio, 0, 0.25).
narrative_ontology:measurement(perm_tr_t15, permissive_software_licensing, theater_ratio, 15, 0.3).
narrative_ontology:measurement(perm_tr_t30, permissive_software_licensing, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_software_licensing, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(perm_be_t15, permissive_software_licensing, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(perm_be_t30, permissive_software_licensing, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_software_licensing, information_standard).
narrative_ontology:affects_constraint(permissive_software_licensing, open_source_commons_sustainability).
narrative_ontology:affects_constraint(permissive_software_licensing, software_supply_chain_security).
narrative_ontology:affects_constraint(permissive_software_licensing, platform_gatekeeper_power).

% DUAL FORMULATION NOTE:
% Permissive licensing is downstream of copyright law and platform concentration but represents a distinct structural choice about how to allocate control over digital intellectual goods. The upstream constraints (copyright law, platform gatekeeping) enable the extraction mechanism; permissive licensing is the specific institutional form through which extraction occurs. These constraints are linked: platform control amplifies permissive licensing extraction; copyright law enables the asymmetry between author and integrator.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_software_licensing, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
