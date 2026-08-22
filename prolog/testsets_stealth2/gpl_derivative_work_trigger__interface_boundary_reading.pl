% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: Interface-Boundary Reading of the GPL Derivative-Work Trigger
 *   domain: legal/technological
 *
 * SUMMARY:
 *   The interface-boundary reading holds that programs communicating through
 *   clean, documented APIs are separate works aggregated together — not a
 *   derivative work — even when runtime coupling is tight. Under this
 *   reading, a vendor may combine GPL-licensed kernels, libraries, and
 *   runtimes with proprietary modules across stable interfaces and distribute
 *   the result while offering source only for the GPL components it modified.
 *   The arrangement is the operating consensus of the embedded-software
 *   economy: it is relied on by device manufacturers and platform vendors,
 *   operationalized daily by corporate compliance functions, disputed by
 *   copyleft stewards and enforcement nonprofits, and never definitively
 *   resolved by any court. Per the epsilon-invariance principle, the
 *   colloquial question 'does linking trigger the GPL' is decomposed into
 *   three structurally distinct constraints — the broad copyleft reading,
 *   this interface-boundary reading, and the narrow linking-permissive
 *   reading — each with its own epsilon, beneficiary/victim structure, and
 *   file; this story authors only the interface-boundary arrangement. The
 *   claim/metrics gap is deliberate: the reading is CLAIMED as scaffold
 *   (transitional support for mixed-licensing modular architecture) while the
 *   authored metrics describe moderate, slowly accumulating extraction and a
 *   maturing enforcement ratchet — the engine measures the divergence; the
 *   claim is not reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - proprietary_ecosystem_integrators: primary beneficiary (institutional/constrained) — combines GPL components with proprietary modules across clean APIs, ships without reciprocal source
 *   - embedded_device_manufacturers: secondary beneficiary (organized/constrained) — ships GPL foundations with closed applications separated at defined interfaces
 *   - gpl_adoption_projects: conditional beneficiary (organized/identity_locked) — gains ubiquity from embedding; chafes at diluted reciprocity
 *   - reciprocity_framing_contributors: primary payer (moderate/trapped) — released contributions power proprietary products without return
 *   - full_stack_source_expectation_users: payer and absent voice (powerless/constrained) — receive component-level source only; represented only by advocacy proxies
 *   - dual_license_vendors: payer (organized/mobile) — licensing-revenue model priced against trigger risk is confiscated by the boundary
 *   - federal_courts: agenda setter (institutional/constrained) — adjudicate the trigger; repeated avoidance keeps the boundary standing
 *   - corporate_open_source_programs: agenda setter (institutional/mobile) — operationalize the boundary in classifications, scanner policy, and architecture approval
 *   - non_us_jurisdiction_developers: excluded (moderate/mobile) — inherit the boundary without seats in its negotiation
 *   - academic_ip_scholars: analytical observer (analytical/analytical) — document the gap between doctrinal text and industry practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.33).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.52).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.33).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.46).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "Interface-Boundary Reading of the GPL Derivative-Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "legal/technological").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f').
narrative_ontology:cs_kernel_codification('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', fixed_text).
narrative_ontology:cs_authority_grounding('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', practice).
narrative_ontology:cs_interpretation_layer_present('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f').
narrative_ontology:cs_reading_relation('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', gpl_derivative_work_trigger__narrow_linking_permissive_reading, influences).
narrative_ontology:cs_axiom('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', foundational, api_boundary_defeats_derivativeness).
narrative_ontology:cs_axiom_status(api_boundary_defeats_derivativeness, holdable).
narrative_ontology:cs_axiom_grounding('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', api_boundary_defeats_derivativeness, empirically_contingent).
narrative_ontology:cs_axiom('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', secondary, copyleft_obligations_attach_per_work_not_per_product).
narrative_ontology:cs_axiom_status(copyleft_obligations_attach_per_work_not_per_product, holdable).
narrative_ontology:cs_axiom_grounding('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', copyleft_obligations_attach_per_work_not_per_product, conventional).
narrative_ontology:cs_reference_frame('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', interface_separate_works_baseline).
narrative_ontology:cs_drift_state('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', post_oracle_v_google_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('83da0d02-5c1c-4842-a1c4-a9e4a5af5f9f', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, embedded_device_manufacturers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, gpl_adoption_projects).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, reciprocity_framing_contributors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, full_stack_source_expectation_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, dual_license_vendors).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, abstraction_filtration_doctrine).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, mere_aggregation_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build commercial products that combine GPL-licensed kernels, libraries, and runtimes with proprietary modules communicating through versioned internal APIs. Under this reading their combinations are aggregations: they ship without publishing proprietary source, owing obligations only for GPL code they modified. Rearchitecting around permissively licensed substitutes or buying commercial licenses is possible but slow and costly, and their installed architectures presuppose the boundary holds.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators, beneficiary,
    institutional, biographical, constrained, global).

% Ship consumer and industrial hardware running GPL foundations with proprietary applications and drivers separated at defined interfaces. The reading lets them withhold application source while distributing GPL-derived firmware components with source offers for those components. Opening their stacks or switching foundations would forfeit the differentiation they sell on.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, embedded_device_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% Volunteer-maintained GPL projects whose code achieves ubiquity partly because integrators may embed it behind interfaces. Maintainers gain distribution, bug reports, and relevance; some chafe that embedding without reciprocal source dilutes the bargain they licensed for. Relicensing away from GPL would require locating every contributor and would betray the project's founding commitments, so leaving the arrangement is fused with the project's identity.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_adoption_projects, beneficiary,
    organized, generational, identity_locked, global).

% Individual developers who contribute code under GPL expecting reciprocal ecosystem growth. Once merged upstream, their contributions cannot be recalled; under this reading their code routinely powers proprietary products that return no source across the interface. Recourse is limited to forking, lobbying maintainers, or choosing differently next time — the released work itself is gone.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, reciprocity_framing_contributors, payer,
    moderate, biographical, trapped, global).

% Purchasers, deployers, and security researchers who read copyleft as a promise of full corresponding source for the software running on devices they own. Under this reading they receive source only for the GPL components; proprietary modules communicating over interfaces stay closed. They can decline individual products, but nearly the whole embedded market shares the structure, so escape is nominal, and they appear in licensing debates only through advocacy proxies.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, full_stack_source_expectation_users, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, full_stack_source_expectation_users, excluded).

% Companies that monetize GPL projects by selling commercial licenses priced against the risk that linking triggers copyleft. The boundary reading shrinks their addressable market: if clean-API combination is aggregation, customers need no commercial license to integrate safely. Their response is product strategy — shifting to support and hosted-service revenue — which is viable but abandons the licensing-revenue model the broad reading sustained.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, dual_license_vendors, payer,
    organized, biographical, mobile, global).

% Adjudicate copyright infringement and derivative-work questions that determine where the GPL's obligations attach. They have never squarely ruled on whether interface-mediated combination is derivation; each avoided issue or narrowly disposed case lets the boundary stand while leaving the question open. Bound by precedent and the copyright statute, they can move the arrangement only case by case.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Compliance offices, review boards, and outside counsel inside large vendors who operationalize the boundary daily: classifying combinations as aggregation or derivation, configuring license scanners, approving architectures. Their classifications decide in practice where obligations attach for their employers, and their compensation and standing depend on employer outcomes.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, corporate_open_source_programs, agenda_setter,
    institutional, biographical, mobile, global).

% Developers and small vendors in jurisdictions without practical GPL enforcement capacity whose code enters global supply chains governed by interpretations negotiated in US and EU courtrooms and bar associations. The boundary's placement is argued without them; they inherit both its compliance demands and its gaps.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, non_us_jurisdiction_developers, excluded,
    moderate, generational, mobile, continental).

% Copyright scholars analyzing whether a derivative-work doctrine built for literary adaptation can carry the weight the boundary reading places on it. They publish critiques and defenses, testify in litigation, and document the gap between doctrinal text and industry practice without collecting from or bearing the arrangement.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, academic_ip_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__interface_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the software industry a shared, administrable rule for when combining separately licensed programs triggers copyleft: components that interoperate through stable, documented interfaces are treated as separate works, so teams can compose mixed-license systems without renegotiating every dependency. It coordinates expectations among upstream projects, integrators, and compliance functions about which obligations attach where.
% TRANSFER_FUNCTION: Moves the use-value of GPL-licensed code into proprietary products without reciprocal source return across the interface; moves residual compliance obligations and litigation risk onto parties who modify GPL code directly or combine across boundaries too blurred to classify cleanly.
% ABSENT_VOICES: Non-US-jurisdiction developers inherit the boundary without seats in its negotiation; end users appear only through advocacy proxies; and the broad reading's proponents have never obtained the definitive appellate ruling that would force the contest into the open — the reading persists partly because its principal opponents' chosen forum has not yet decided.
% DISAPPEARANCE_RATIONALE: An overnight rejection of the reading (every interface-mediated combination deemed derivation) would render thousands of shipped products noncompliant at once, freeze firmware updates pending relicensing or code segregation, spike commercial-license pricing, and push integrators toward permissive substitutes — redistributing the embedded-software stack. Adoption of the narrow reading instead would delete the boundary-cleanliness condition and empty the doctrine's middle ground. Either resolution rearranges the mixed-licensing equilibrium the arrangement holds in place.
% FOUNDING_PROBLEM: Copyright's derivative-work doctrine was built for adaptations of single-author literary works; modular, multi-party software broke it. The GPL needed an answer to whether linking separately developed programs creates 'a work based on the Program,' and the industry needed a predictable line before investing in component reuse.
% FOUNDING_PROBLEM_CORROBORATION: Academic copyright scholarship, from the abstraction-filtration analysis forward, documents the doctrine's poor fit with software composition; courts' own hedged dispositions in software-copyright cases attest that the line remains unsettled; and the license steward's published guidance concedes the aggregation/derivation line requires case-by-case judgment. Corroboration comes from outside the integrator beneficiaries — including from the reading's principal opponent, who disputes where the line sits while affirming that the line-drawing problem is real.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.33, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).
:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.33 at interval end): the reading preserves real obligations for direct modification of GPL code, but it permits the use-value of GPL code to flow into proprietary products across interfaces without reciprocal return, and even by the reading's own lights the residual flow is not zero — the boundary line is contestable, and parties who blur it absorb settlement leverage. Suppression (0.52) reflects an enforcement ratchet rather than heavy coercion: compliance audits, settlement demands, and the chilling ambiguity of an unsettled trigger question, intensifying as stakes grew (the series runs 0.24 to 0.52 over the interval). Theater ratio (0.28) is low-moderate but rising: license scanners, SBOM pipelines, and review-board documentation increasingly ritualize compliance without changing architecture decisions. Accessibility collapse (0.46) is well below natural-law levels because genuine alternatives persist — permissively licensed substitutes, commercial licenses, deliberate avoidance of GPL code — and resistance (0.60) is substantial: steward opposition, enforcement litigation, scholarly critique, and the API-copyrightability challenge. Scaffold rationale: the reading's justification is transitional, not steady-state — it holds the mixed-licensing equilibrium stable during the binary-distribution era and pending definitive adjudication; its de facto sunset clause is the migration of software delivery to network services, where the distribution trigger dissolves, plus the eventual court ruling that resolves the trigger question in one direction or the other. The temporal series run on one shared time grid ({0,6,12,18,24,30}) so every tracked metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the integrator and manufacturer seats the arrangement is enabling coordination they built businesses on: the boundary is sound architecture recognized in law. From the contributor seat the same structure operates as reciprocity breach — code licensed for mutual growth powering closed products. From the user seat it is lost auditability on devices they own. From the dual-license vendor seat it is expropriation of a revenue model priced against the very risk the reading removes. The compliance-function seat leans integrator-ward while presenting as neutral administration. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: integrators and manufacturers collect the arrangement's gains directly; GPL projects collect adoption and relevance (with identity-locked exit fusing them to the arrangement). Victim declarations map to high directionality: contributors are trapped (released code cannot be recalled), users are constrained (market-wide practice limits meaningful escape), and dual-license vendors are directly targeted in their revenue model. Courts and compliance functions sit near the administrative middle by derivation. Two known residuals are noted rather than overridden: the compliance-function seat structurally leans beneficiary-ward (its classifications systematically favor its employers' aggregation findings) beyond what a neutral-administrator derivation yields; and the dual-license vendors' mobile exit reflects business-model flexibility, not insulation from the constraint's cost. Directionality overrides were deliberately omitted: the override surface is keyed by power atom, and both corrections would misfire across same-power seats (courts share the institutional atom with integrators and compliance functions; projects and manufacturers share the organized atom with the vendors), corrupting seats the derivation already handles correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as scaffold rather than rope prevents mislabeling a transitional accommodation as steady-state coordination: the reading's justification is the transition it manages (mixed-licensing modular architecture pending adjudication and amid a delivery-model shift), not a permanent equilibrium. The founding problem — fitting modular multi-party software into a derivative-work doctrine built for literary adaptation — remains live, so no zombie flag is warranted on the status-by-verdict mismatch (live x world_rearranges). But the mandate's domain is contracting: if network delivery completes the sunset without adjudication, expect the arrangement to decay toward theatrical maintenance — scanner rituals and policy documents attached to a trigger question that no longer arises — which the rising theater_ratio trajectory already anticipates. The omega on sunset status carries the resolution path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the gpl_derivative_work_trigger kernel; what structural changes would the sibling readings (broad_copyleft_reading, narrow_linking_permissive_reading) produce if adopted?',
    'Authoritative adjudication of the trigger question (appellate treatment of linking and derivation) or license-steward redrafting that redefines the trigger; each sibling is maintained as its own constraint story with its own epsilon.',
    'The broad reading converts today''s beneficiaries (integrators, manufacturers) into payers and raises extraction sharply; the narrow reading deletes the boundary-cleanliness condition and drains the doctrine''s middle ground, dropping extraction toward the permissive baseline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: sibling readings of the same kernel instantiate different constraints with different beneficiary/victim sets.').

omega_variable(
    coupling_quality_legal_cognition,
    'Is coupling quality — boundary cleanliness, tightness of runtime coupling — a legally cognizable variable at all, or must the trigger turn on binary facts like linking mechanism?',
    'Doctrinal development: whether courts adopt filtration-style analysis of interface-dictated expression in derivative-work determinations, or reject quality-of-boundary evidence as administrable.',
    'If courts refuse to cognize boundary quality, this reading collapses toward the broad reading; if they cognize it without limit, the reading slides toward the narrow permissive reading — the disagreement among all three readings is located exactly here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coupling_quality_legal_cognition, conceptual, 'Whether the boundary variable this reading rests on is judicially administrable.').

omega_variable(
    saas_transition_sunset_status,
    'Is the doctrine''s de facto sunset — network-delivered software dissolving the distribution trigger — actually closing the arrangement''s domain, or does embedded and IoT distribution keep it live indefinitely?',
    'Track the share of GPL-code deployment occurring in distributed binaries versus network services over the coming decade; monitor AGPL-style patch adoption by former GPL projects.',
    'If the domain closes, the transitional classification is confirmed and the mandate decays toward theatrical compliance; if distributed deployment persists at scale, the arrangement stabilizes as a steady-state coordination structure rather than a transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_transition_sunset_status, empirical, 'Whether the arrangement''s transitional justification is expiring on schedule.').

omega_variable(
    maintainer_benefit_asymmetry,
    'Do GPL projects genuinely benefit from integrator embedding under this reading, or does benefit concentrate in integrators while contributors bear reciprocity loss?',
    'Maintainer revealed-preference studies: do projects select GPL knowing embedding-without-return occurs, and do they migrate licenses when it does?',
    'If maintainer benefit is illusory, the beneficiary set shrinks to integrators and manufacturers, the arrangement reads as more extractive than authored, and classification drifts toward the hybrid coordination/extraction type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_benefit_asymmetry, empirical, 'Whether the declared coordination-side beneficiaries actually collect.').

omega_variable(
    oracle_precedent_spread,
    'Will the API-copyrightability holding in Oracle v. Google spread into derivative-work analysis (undermining the premise that interface crossing is legally inert), or remain confined to copyrightability and fair use?',
    'Subsequent case law citing Oracle for derivation rather than copyrightability; treatise and restatement uptake.',
    'Spread would drive the reading''s foundational axiom toward superseded-in-practice and push this constraint toward the broad reading''s structure; confinement leaves the reference frame intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oracle_precedent_spread, empirical, 'Precedential trajectory of the main challenge to the reading''s foundational premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t6, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(gpl__tr_t6, observed).
narrative_ontology:measurement(gpl__tr_t12, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(gpl__tr_t12, observed).
narrative_ontology:measurement(gpl__tr_t18, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(gpl__tr_t18, observed).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(gpl__tr_t24, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t6, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement_basis(gpl__be_t6, observed).
narrative_ontology:measurement(gpl__be_t12, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 12, 0.29).
narrative_ontology:measurement_basis(gpl__be_t12, observed).
narrative_ontology:measurement(gpl__be_t18, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 18, 0.31).
narrative_ontology:measurement_basis(gpl__be_t18, observed).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 24, 0.32).
narrative_ontology:measurement_basis(gpl__be_t24, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement_basis(gpl__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t6, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement_basis(gpl__su_t6, observed).
narrative_ontology:measurement(gpl__su_t12, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(gpl__su_t12, observed).
narrative_ontology:measurement(gpl__su_t18, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement_basis(gpl__su_t18, observed).
narrative_ontology:measurement(gpl__su_t24, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(gpl__su_t24, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(gpl__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'does linking trigger the GPL' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the broad reading (trigger at any linkage), this interface-boundary reading (clean boundaries defeat derivation even under tight coupling), and the narrow permissive reading (trigger only on modification). Their epsilon values differ because their beneficiary/victim sets differ: the broad reading's operation is cited as authority pressure on this one, and this reading's industry adoption supplies practice-legitimacy that the narrow reading draws on. Family members link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
