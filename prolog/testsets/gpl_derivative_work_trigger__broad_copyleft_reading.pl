% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Derivative Work Trigger (Broad Copyleft Reading)
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   The GPL derivative work trigger is a contested kernel in open source
 *   governance: does linking — static or dynamic — create a derivative work
 *   that obligates source code disclosure? The broad copyleft reading
 *   instantiated here treats any meaningful integration (linking, embedding,
 *   delegation) as creating derivative work status under copyright law, which
 *   in turn activates GPL Section 2's reciprocity obligation: the combined
 *   work must be distributed under GPL terms, forcing proprietary vendors to
 *   either release source code or abandon the GPL dependency. This reading
 *   interprets GPL as a maximalist commons-protecting mechanism that uses
 *   copyright's derivative work doctrine as the enforcement lever. The
 *   constraint exhibits Tangled Rope structure: genuine coordination benefits
 *   exist (code reuse, maintenance burden sharing, community-driven feature
 *   development), but these are asymmetrically distributed, with GPL authors
 *   and downstream users capturing most benefit while proprietary vendors
 *   bear the disclosure cost. The measurement trajectory shows increasing
 *   extractiveness (0.35→0.52) over the observation interval, reflecting
 *   increased GPL adoption in critical infrastructure and enterprise systems,
 *   raising the practical stakes of the derivative work trigger. Suppression
 *   has also increased (0.50→0.65) as compliance mechanisms have matured
 *   (automated scanning, supply chain audits, enforcement action patterns).
 *   The constraint is not purely theatrical (theater_ratio declining
 *   0.52→0.48) because the legal threat is real and enforced, though there is
 *   performative component in compliance theater (audit rituals, license
 *   scanning tools, certification processes).
 *
 * KEY AGENTS:
 *   - GPL Author Community: Primary beneficiary (institutional/arbitrage) — controls license terms, defines derivative work scope, captures reciprocity obligation enforcement
 *   - Proprietary Software Vendors: Primary victim (institutional/constrained to powerless depending on integration depth) — faces source disclosure obligation or dependency removal
 *   - Downstream End Users: Secondary beneficiary (powerful/mobile) — gain source access and transparency from reciprocity obligation
 *   - Commercial Integrators: Secondary victim (organized/constrained) — moderate-power agents that incorporate GPL code bear compliance burden without negotiating capacity
 *   - Small Independent Developers: Tertiary victim (moderate/constrained) — face real extraction pressure but lack large vendors' exit options or negotiating power
 *   - Copyright Law Tradition: Authority structure (lineage/distributed) — derivative work doctrine provides the legal kernel; multiple interpretations of how broadly it applies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.52).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.65).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Derivative Work Trigger (Broad Copyleft Reading)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '0ef0d8fd-c003-4146-b2b2-8bf7934c5780').
narrative_ontology:cs_kernel_codification('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', fixed_text).
narrative_ontology:cs_authority_grounding('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', lineage).
narrative_ontology:cs_interpretation_layer_present('0ef0d8fd-c003-4146-b2b2-8bf7934c5780').
narrative_ontology:cs_reading_relation('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', foundational, all_linking_creates_derivative_status).
narrative_ontology:cs_axiom_status(all_linking_creates_derivative_status, holdable).
narrative_ontology:cs_axiom_grounding('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', all_linking_creates_derivative_status, deontological).
narrative_ontology:cs_axiom('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', foundational, reciprocal_source_sharing_enforceable_via_copyright).
narrative_ontology:cs_axiom_status(reciprocal_source_sharing_enforceable_via_copyright, holdable).
narrative_ontology:cs_axiom_grounding('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', reciprocal_source_sharing_enforceable_via_copyright, conventional).
narrative_ontology:cs_reference_frame('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', maximalist_commons_protection).
narrative_ontology:cs_drift_state('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', contemporary_supply_chain_integration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ef0d8fd-c003-4146-b2b2-8bf7934c5780', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_commons).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_author_community).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_integrators).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_derivative_authors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMERCIAL INTEGRATOR (SNARE) — A proprietary software vendor that links GPL code into a closed-source product faces binary choice: disclose full source code (destroying proprietary value) or remove the dependency (sunk cost). No intermediate exits available. The derivative work trigger traps them regardless of whether the linking was dynamic or static, aggressive or incidental. Maximum experienced extraction — forced redistribution of intellectual property.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__broad_copyleft_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROPRIETARY ECOSYSTEM (TANGLED ROPE) — Commercial software vendors as an organized collective experience mixed dynamics: genuine coordination benefit from reusing GPL libraries (cost reduction, feature access, maintenance burden sharing), alongside extraction cost (source disclosure obligation, license compliance overhead, strategic constraint on product architecture). Organized agents can negotiate exceptions, use alternative libraries, or architect around GPL dependencies. Significant asymmetry — GPL community benefits, proprietary vendors bear compliance burden.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GPL AUTHOR COMMUNITY (ROPE) — GPL creators experience the broad copyleft reading as pure coordination: the license communicates their sharing intention, enforces reciprocity, enables collaborative improvement, and creates a commons. They benefit from downstream code contribution (copyleft pull), reputation, and license-enforced community participation. Net beneficiary — extraction runs toward this agent through license compliance mechanisms that pull proprietary improvements back into the commons.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__broad_copyleft_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOWNSTREAM END USERS (ROPE) — Benefit from source disclosure obligation: proprietary vendors must release source code or discontinue derivative products. This creates transparency, reduces lock-in, and enables community auditing. End users experience the constraint as pure coordination — it enforces disclosure that aligns with their interests. Low experienced extraction because users have exit options (choose non-GPL products) and the coordination function is genuine.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__broad_copyleft_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SMALL INDEPENDENT DEVELOPERS (TANGLED ROPE) — Moderate-power agents that incorporate GPL libraries experience genuine coordination (feature reuse, maintenance burden sharing, community quality feedback) alongside real extraction pressure (source disclosure requirement that may expose proprietary techniques or break business model). Unlike large vendors, they lack negotiating power for alternatives or exemptions. Constrained exit: they can avoid GPL libraries but at feature/quality cost.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LEGAL NATURALISM VIEW (MOUNTAIN) — From a universalist/positivist legal perspective, GPL derivative work scope is treated as a discovered legal truth: linking creates derivative works as a matter of copyright law's mathematical logic (combining protected works → combined protection scope). The constraint appears immutable, a feature of copyright doctrine itself. However, the structural data contradicts this — identifiable beneficiaries exist (GPL authors, downstream users), suppression is engineered (not natural), and the classification serves specific interests. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__broad_copyleft_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpl_derivative_work_trigger__broad_copyleft_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpl_derivative_work_trigger__broad_copyleft_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The broad copyleft reading treats linking as a low bar for triggering derivative work status, which forces proprietary vendors to either surrender source code (destroying proprietary value) or abandon the dependency. This is real extraction — the GPL author captures value through enforced reciprocity. However, it is not maximal (0.72+) because vendors have partial exit options: they can rewrite dependencies, use alternative libraries, or license proprietary code separately. The measurement trajectory (0.35→0.52) reflects increasing practical extraction pressure as GPL code moves into critical infrastructure where avoiding the dependency is increasingly costly. Suppression (0.65): Moderate-high and rising. Significant barriers to exit include: (1) technical cost of rewriting dependencies; (2) supply-chain complexity making GPL detection difficult until too late; (3) legal uncertainty about derivative work scope; (4) enforcement actions (SFLC, GPL-vigilante litigation) creating reputational/legal risk. However, suppression is not total (0.80+) because alternatives exist — proprietary libraries, non-copyleft licenses, architectural refactoring. The rising trajectory reflects maturation of compliance monitoring (automated scanners, supply-chain audits) that increase the cost of non-compliance detection. Theater ratio (0.48, stable at ~0.50): Low-moderate and declining slightly. The constraint has real enforcement (SFLC litigation, GitHub policy, enterprise audits), not purely performative. The theatrical component exists (compliance certifications, audit rituals, scanning tools that produce reports vendors never fully act on) but does not dominate. This distinguishes it from Piton (theater ≥0.70).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival gap between beneficiaries and victims. The GPL author community sees Rope — pure coordination that enforces reciprocal sharing and commons participation. Downstream users see Rope — transparency obligation aligned with their interests. Proprietary vendors see Snare — they are trapped between source disclosure and dependency abandonment with no intermediate exits for large integrated systems. Small developers see Tangled Rope — they benefit from GPL code but face real extraction pressure. The analytical observer risks seeing a Mountain (derivative work scope is a discovered legal truth) but the structural data contradicts this: identifiable beneficiaries (GPL authors, downstream users) exist, suppression is engineered through legal doctrine rather than physical law, and the classification serves specific epistemic interests in the open-source movement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural relationship to the extraction flow. GPL authors and downstream users are beneficiaries with institutional/analytical power and arbitrage/mobile exit options → low d → negative effective extraction (they see benefit). Proprietary vendors are victims with varying power (institutional to moderate) and constrained exit → high d → high effective extraction (they see burden). Small developers are victims with moderate power and constrained exit → medium-high d → moderate extraction. The powerless agent (small integrator with no alternatives) approaches d=1.0, experiencing maximum extraction. The engine computes f(d) from these derived d values to produce χ = ε × f(d) × σ(S). Vendors at global scope (σ=1.2) experience amplified extraction; those at regional scope (σ=0.9) experience dampened extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITTER-AXIS RESOLUTION: This constraint resolves the mandatrophy by documenting that it is one reading of a contested kernel, not a measurement disagreement. The broad copyleft reading instantiates the FSF/maximalist commons position: linking = derivative work = GPL reciprocity obligation. Sibling readings instantiate different legal and practical positions on where the derivative work boundary sits. The mandatrophy is not 'which reading is true?' but 'which legal/practical framing is authoritative?' This is a committer-axis question, not a measurement-axis question. The omega variables route the reading-conflict into conceptual uncertainty (kernel_definition_contest) and empirical uncertainty (enforcement rates, vendor workarounds, GPL version evolution). The constraint's Tangled Rope classification is stable across this reading — genuine coordination (code reuse) alongside asymmetric extraction (source disclosure obligation). Sibling readings would produce different classifications by instantiating different baseline extraction values and beneficiary/victim assignments.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_scope,
    'What constitutes a derivative work under copyright law? Does dynamic linking create derivative works under US/EU/international law?',
    'Legal precedent analysis (Oracle v Google, Galoob, Lexmark, CMM Industries); legislative text review (Copyright Office guidance, TRIPS WCT); empirical survey of enforcement patterns',
    'Narrow interpretation: dynamic linking may not trigger derivative work status → constraint reclassifies as Rope (pure coordination benefit). Broad interpretation (current axis): all linking triggers derivative status → constraint remains Tangled Rope / Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition_scope, empirical, 'Legal definition of derivative work scope for linked code').

omega_variable(
    copyleft_enforceability_empirical,
    'How frequently is GPL derivative work obligation actually enforced? What is the empirical compliance rate among commercial vendors?',
    'SFLC enforcement action database; GPL violation detection systems (Black Duck, Synopsys, Reuse.software); commercial compliance audits; litigation patterns',
    'High enforcement (>60%): suppression stays high, constraint remains Snare/Tangled Rope. Low enforcement (<20%): suppression drops significantly, constraint degrades toward Piton (theatrical but ineffective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyleft_enforceability_empirical, empirical, 'Empirical GPL enforcement rate and compliance patterns').

omega_variable(
    architectural_workarounds_availability,
    'How easily can proprietary vendors architect around GPL dependencies without significant feature or performance cost?',
    'Case study analysis of GPL-dependent products; alternative library ecosystems (proprietary equivalents); refactoring cost estimates; technical survey of vendors',
    'Easy workarounds available: exit_options for vendors upgrade from trapped→constrained, experienced extraction drops. No workarounds: vendors remain trapped, extraction stays severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architectural_workarounds_availability, empirical, 'Availability of GPL dependency workarounds for proprietary vendors').

omega_variable(
    kernel_definition_contest,
    'What constitutes the contested kernel? Is it the copyright law definition of derivative work, the GPL license text interpretation, or the underlying question of source code commensurability?',
    'This omega documents the committer-axis under-determination. The broad copyleft reading treats ''linking creates derivative works'' as the kernel. The narrow linking reading treats ''derivative work scope excludes unmodified linked code'' as the alternative kernel. The interface boundary reading treats ''only direct API modifications trigger derivative status'' as a distinct kernel.',
    'Different kernels yield different constraint classifications. This reading instantiates the broad-copyleft kernel. Sibling readings instantiate different kernels, not different measurements of the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_definition_contest, conceptual, 'Committer-axis kernel definition: which GPL claim is the contested commitment?').

omega_variable(
    gpl_vs_gplv3_drift,
    'How has the derivative work scope shifted between GPL v2 and v3? Does v3 narrow or expand the triggering condition?',
    'Textual comparison of licensing language; FSF official guidance evolution; case law on version-specific interpretation; field practice changes post-v3 adoption',
    'If v3 narrows scope: this reading is becoming foreclosed by GPL''s own authority structure. If v3 maintains or expands scope: the reading''s axioms remain holdable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpl_vs_gplv3_drift, empirical, 'GPL version evolution and derivative work definition scope drift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_broad_tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(gpl_broad_tr_t5, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(gpl_broad_tr_t10, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(gpl_broad_be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gpl_broad_be_t5, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gpl_broad_be_t10, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gpl_broad_su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gpl_broad_su_t5, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(gpl_broad_su_t10, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_vendor_lock_in).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_library_ecosystem_extraction).

% DUAL FORMULATION NOTE:
% This constraint is part of the GPL derivative work trigger kernel family. Three sibling constraints exist, each representing a different reading of where the derivative work boundary lies: narrow_linking_permissive_reading (dynamic linking does not trigger), interface_boundary_reading (API-level coupling excludes binary linking). These are not three measurements of one constraint; they are three structurally distinct constraints grounded in different legal/practical kernels. The broad copyleft reading (this file) treats all linking as derivative work. Siblings would have different ε values reflecting their different extraction assumptions. All three are linked via network.affects_constraints to represent the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__broad_copyleft_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
