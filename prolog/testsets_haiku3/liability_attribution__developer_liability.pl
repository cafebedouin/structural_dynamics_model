% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__developer_liability, []).

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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer Primary Liability for Technology-Enabled Harms
 *   domain: legal_theory/technology_governance/regulatory_design
 *
 * SUMMARY:
 *   A liability framework assigns primary legal responsibility to technology
 *   developers and creators when their capability is deployed in ways that
 *   cause harm—even when the deployer (a corporation, government agency, or
 *   institution) had superior knowledge of deployment context, direct control
 *   over usage rules, and decision authority to restrict or refuse
 *   deployment. The developer is sued, fined, and forced to pay settlements
 *   regardless of whether they designed the technology with harmful intent or
 *   attempted to discourage misuse. This reading of the liability_attribution
 *   kernel treats developer creation as the morally and legally salient fact;
 *   sibling readings (deployer_liability, shared_liability) treat
 *   deployer-context control and deployment decision as more salient. This
 *   story instantiates the developer-liability reading cleanly: a constraint
 *   whose persistence depends on active enforcement (litigation, regulatory
 *   action) of the developer-as-primary-defendant premise.
 *
 * KEY AGENTS:
 *   - technology_developers: creators bearing primary liability (moderate power, constrained exit)
 *   - deploying_organizations: benefit from liability externalization (powerful, arbitrage-capable)
 *   - open_source_maintainers: powerless creators identity-locked to their projects, same liability exposure as commercial developers
 *   - regulatory_bodies: enforce the developer liability doctrine (institutional agenda-setters)
 *   - tort_plaintiffs: harmed individuals structurally excluded from liability-allocation debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.71).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Primary Liability for Technology-Enabled Harms").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "legal_theory/technology_governance/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '6af03404-e8f3-4925-80cf-a76d2b2d1541').
narrative_ontology:cs_kernel_codification('6af03404-e8f3-4925-80cf-a76d2b2d1541', distributed).
narrative_ontology:cs_authority_grounding('6af03404-e8f3-4925-80cf-a76d2b2d1541', distributed).
narrative_ontology:cs_reading_relation('6af03404-e8f3-4925-80cf-a76d2b2d1541', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('6af03404-e8f3-4925-80cf-a76d2b2d1541', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('6af03404-e8f3-4925-80cf-a76d2b2d1541', foundational, developer_creation_is_primary_responsibility_locus).
narrative_ontology:cs_axiom_status(developer_creation_is_primary_responsibility_locus, holdable).
narrative_ontology:cs_axiom_grounding('6af03404-e8f3-4925-80cf-a76d2b2d1541', developer_creation_is_primary_responsibility_locus, deontological).
narrative_ontology:cs_axiom('6af03404-e8f3-4925-80cf-a76d2b2d1541', secondary, foreseeability_of_developer_as_capability_creator).
narrative_ontology:cs_axiom_status(foreseeability_of_developer_as_capability_creator, holdable).
narrative_ontology:cs_axiom_grounding('6af03404-e8f3-4925-80cf-a76d2b2d1541', foreseeability_of_developer_as_capability_creator, empirically_contingent).
narrative_ontology:cs_reference_frame('6af03404-e8f3-4925-80cf-a76d2b2d1541', developer_primary_accountability_framework).
narrative_ontology:cs_drift_state('6af03404-e8f3-4925-80cf-a76d2b2d1541', post_deployer_control_maturity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6af03404-e8f3-4925-80cf-a76d2b2d1541', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deploying_organizations).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_users_liability_shield).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, technology_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_maintainers).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, creator_responsibility_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, foreseeability_of_developer_intent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create software, algorithms, and technical systems intended for legitimate uses. They face primary legal liability when deployers misuse their technology to cause harm—lawsuits, settlements, regulatory fines—even when the deployer had superior knowledge of deployment context, control over usage rules, and authority to refuse harmful applications. Exit is constrained because ceasing development or open-sourcing code does not eliminate past-liability exposure, and market pressure to create capability-rich tools incentivizes features that can be repurposed.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, technology_developers, payer,
    moderate, biographical, constrained, global).

% Deploy technology in specific contexts with knowledge of local laws, usage controls, and decision authority over deployment rules. Under developer liability, they externalize risk to the creator: if deployment causes harm, the developer is the primary target for compensation and blame, not the entity that made the deployment decision. They retain full operational control and can adjust deployment rules, but legal exposure sits with the developer.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deploying_organizations, beneficiary,
    powerful, generational, arbitrage, global).

% Volunteer or minimally-compensated creators of infrastructure tools used by deployers they never meet. They face the same primary liability doctrine as commercial developers despite having no control over deployment context, no enforcement authority over usage, and no resources to defend against litigation. Identity-locked: abandoning a widely-relied-upon open-source project is professionally and communally devastating; continuing exposes them to liability.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_maintainers, payer,
    powerless, biographical, identity_locked, global).

% Final users of deployed systems (patients using medical AI, individuals targeted by algorithmic decisions, etc.). They benefit from the liability framework's implicit assumption that developers should bear responsibility: it creates a named defendant who has resources and incentive to address harms. They do not face primary legal liability for the technology's design flaws even when their use triggered the harm, as long as they were not the deployed-context-controlling party.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, end_users_liability_shield, beneficiary,
    organized, biographical, mobile, global).

% Enforce the liability regime through litigation, regulatory action, and standard-setting. They enforce developer primary liability by holding creators legally responsible for foreseeable misuse, regardless of whether the deployer had superior contextual control or could have implemented use restrictions.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Individuals harmed by deployed technology (discriminatory AI decisions, autonomous system failures, data breaches enabled by capability the developer created). They are excluded from the core liability-allocation debate: the constraint determines who pays, not whether compensation is adequate. Their voice would emphasize that developers' resources and technical knowledge make them better positioned to prevent harms than deployers, but they are not seated in the regulatory/corporate governance conversation that sets the liability rule.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, tort_plaintiffs, excluded,
    powerless, immediate, trapped, local).

% Adjudicate specific liability cases and de facto shape the constraint through precedent. Different jurisdictions have adopted different readings—developer primary liability in some, shared liability in others—creating regulatory arbitrage and fragmented compliance burdens on global technology firms.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, jurisdiction_specific_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, deploying_organizations).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single named defendant (the developer/creator) for technology-enabled harms, enabling plaintiffs and regulators to hold someone accountable for foreseeable misuse and to compel design changes, safety features, and risk communication. Avoids the diffusion problem where deployers disclaim responsibility by citing 'developer's design choices' and developers disclaim responsibility by citing 'deployer's context-specific decisions.'
% TRANSFER_FUNCTION: Transfers legal liability, litigation costs, settlement obligations, and regulatory fines from deployers and end-user defendants to developers and open-source maintainers—the creators who have no enforcement authority over deployment context but bear the primary legal burden for harms that flow from their capability.
% ABSENT_VOICES: Tort plaintiffs harmed by technology are structurally excluded from the liability-allocation debate itself (they appear only as the ultimate recipients of compensation, if any). Open-source maintainers and individual developers with limited resources are also minimally represented in the regulatory processes that set the liability standard, which are dominated by large technology firms, corporate counsel, and institutional regulators.
% DISAPPEARANCE_RATIONALE: If developer primary liability vanished and replaced with deployer liability, deployers would face direct incentives to audit and restrict deployments themselves; developers would shift resources from defensive legal compliance to enabling more powerful capability; litigation would target deployers instead of creators; open-source projects might expand without liability chilling effects. The entire allocation of risk, compliance burden, and capability development would reorganize.
% FOUNDING_PROBLEM: Early technology governance struggled to assign responsibility when capability created for legal uses was deployed in harmful ways. Without a primary liable party, harmed individuals could not recover, deployers could escape accountability by claiming ignorance, and developers could disclaim responsibility by claiming deployment was outside their control. A named defendant was needed to ensure accountability and fund remediation.
% FOUNDING_PROBLEM_CORROBORATION: Technology regulators and plaintiff attorneys attest the founding problem remains live: deployers still attempt to escape accountability, and some way to ensure remediation is necessary. Developers and open-source communities counter that the problem has shifted: deployer-side controls (access restrictions, usage monitoring, audit trails) are now mature and enforceable, making developer liability a cover for deployer negligence. Shared-liability advocates (e.g., legislative bodies in EU and some US states) attest the founding problem is partially solved but the solution has over-corrected toward developers.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__developer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__developer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__developer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 by interval end because developers bear a significant transfer (legal liability, litigation costs, settlements, compliance burden) without corresponding control over deployer behavior. Suppression is high (0.71) because the constraint persists through active enforcement mechanisms (litigation, regulatory action) that keep developers named as primary defendants despite structural arguments that deployers have superior information and control. Theater is moderate (0.42): some performative activity around 'safety reviews' and 'responsible disclosure' protects developers, but the core enforcement machinery remains directed at developer liability. Measurements run on a shared time grid across all three metrics. The trajectory shows extractiveness and suppression both rising from early period (0.48, 0.55) to stable plateau by mid-interval (0.66–0.68, 0.70–0.71), consistent with a constraint whose enforcement architecture matured around year 15 and stabilized. Theater ratio shows continued incremental rise, indicating growing defensive performative activity as the constraint's burden accumulates.
 *
 * PERSPECTIVAL GAP:
 *   From the deployer seat, the constraint is a beneficial coordination mechanism: it ensures developers fund risk remediation and disincentivizes careless capability creation. From the developer seat, especially open-source maintainers with no deployer relationship, the constraint is extractive enforcement: they bear legal exposure for decisions made by others who controlled the deployment context. From the regulatory seat, the constraint is legitimate accountability. From the excluded tort-plaintiff seat, the constraint is better than nothing (ensures someone pays) but under-inclusive (deployers who controlled deployment rules escape primary liability). The engine should compute markedly different type classifications across these seats from the structural data: deployers near the beneficiary end (low d), developers and open-source maintainers near the full-target end (high d), regulatory bodies near the enforcer end.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers (d ≈ 0.9): bear primary liability with constrained exit (cannot escape past-liability exposure by ceasing development); deployers externalize risk to them despite having superior deployment-context knowledge and control. Open-source maintainers (d ≈ 0.95): same liability exposure as commercial developers but with identity-locked exit (abandoning a widely-relied project is professionally devastating) and no deployer relationship or enforcement authority. Deploying organizations (d ≈ 0.1): benefit from liability externalization; retain operational control and arbitrage ability (can shift to alternative suppliers if liability pressure mounts on one developer ecosystem). Regulatory bodies (d ≈ 0.5): enforce the regime impartially but derive legitimacy and power from the liability framework's persistence. Tort plaintiffs (excluded, not seated): would show high d if seated (harmed by the technology, minimal exit options) but are not in the conversation that shapes liability allocation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested with respect to whether it remains live or has been substantially solved. Technology advocates argue that deployer-side controls (access restrictions, usage monitoring, model cards, audit trails) have matured and made deployer-level enforcement viable; under this reading, developer primary liability is a zombie—its original function (ensuring accountability when deployers disclaim responsibility) is no longer necessary, but the constraint persists as institutional inertia and as a benefit to deployers who prefer it to deployer-accountability regimes. Regulators counter that deployers still attempt to externalize accountability and that developer incentives to build safety constraints remain crucial. The measurement series shows extractiveness stabilizing after year 15, consistent with a constraint whose function has plateaued and whose persistence is maintained by institutional machinery rather than by solving a live coordination problem—weak evidence for mandatrophy, not definitive. The contested founding_problem_status and the moderate-rising theater_ratio both support an omega asking whether the constraint has outlived its founding function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foreseeability_burden_on_developer,
    'What degree of foreseeability should trigger developer primary liability? Should developers be liable for all foreseeable misuses, only highly probable misuses, only uses explicitly within their design intent, or only uses they explicitly encouraged?',
    'Comparative legal analysis across jurisdictions with different foreseeability thresholds; empirical study of how different thresholds affect developer behavior (safety investment, capability breadth, open-source participation).',
    'A narrow foreseeability standard (only uses developer explicitly intended) would reduce developer liability and move the constraint toward shared liability. A broad standard (any possible misuse) would increase extraction and intensify suppression, pushing the constraint further into snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreseeability_burden_on_developer, empirical, 'The boundary of foreseeable misuse that triggers developer liability.').

omega_variable(
    deployer_control_vs_developer_knowledge,
    'Which is more salient in responsibility allocation: the developer''s technical knowledge of the capability''s potential or the deployer''s control over deployment context and usage rules?',
    'Experimental or quasi-experimental test: compare harm outcomes in regimes emphasizing developer liability (high developer investment in safety) vs. deployer liability (high deployer investment in use restrictions) vs. shared liability (both). Examine which configuration actually reduces harm most.',
    'If deployer-context control is more salient, the constraint should shift toward deployer_liability reading; if knowledge is more salient, developer_liability remains justified; if both are equally salient, shared_liability becomes the better frame. This directly determines which reading is structurally defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deployer_control_vs_developer_knowledge, empirical, 'Whether the salient fact in liability allocation is technical knowledge or deployment-context control.').

omega_variable(
    open_source_maintainer_exception,
    'Should open-source maintainers face the same primary liability as commercial developers, given their lack of resources, enforcer relationship with deployers, and identity-locked exit?',
    'Policy analysis of jurisdictions that exempt open-source or non-commercial creators from primary liability; study of how exemptions affect open-source ecosystem health and deployment patterns.',
    'If open-source exemptions are adopted, the constraint bifurcates: commercial developers remain primary-liable payers, open-source maintainers exit the payer set, and deployers of open-source tools gain further liability protection. This would reduce the apparent extraction from the payer-victim set while maintaining the beneficial-liability coordination for commercial software.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_maintainer_exception, preference, 'Whether the developer liability standard should apply uniformly to open-source and commercial creators.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (deployers escaping accountability) been substantially solved by the maturation of deployer-side controls (access restrictions, audit trails, model cards), making developer primary liability a mandatrophic zombie constraint?',
    'Post-hoc evaluation of deployer behavior in jurisdictions with strong deployer-liability or shared-liability regimes vs. developer-primary-liability regimes: do deployers invest in use restrictions when not shielded by developer liability? If so, the founding problem is solvable without developer primary liability.',
    'If the founding problem is resolved or resolvable without developer liability, the constraint''s persistence becomes institutional inertia and benefit to deployers, reclassifying the constraint as piton or transitioning it toward shared liability. This omega gates the mandatrophy analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether deployer-side governance maturity has obsoleted the founding problem justifying developer primary liability.').

omega_variable(
    reading_foreclosure_and_coexistence,
    'Do the three readings (developer_liability, deployer_liability, shared_liability) coexist as live positions within a single legal framework, or does adopting one reading logically foreclose the others?',
    'Jurisprudential analysis: examine whether a single jurisdiction can hold developer primary liability AND deployer primary liability in different contexts (e.g., different sectors, different harm types). If yes, they coexist; if no, one forecloses the other.',
    'If readings coexist, they are competing live positions held by different jurisdictions—modeling them as sibling constraints with coexists_with relations is correct. If one reading forecloses others within any framework, the foreclosure relation is structurally true and the readings cannot cohabit a jurisdiction''s legal code.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_and_coexistence, conceptual, 'The logical structure of alternative liability readings as competing or mutually exclusive positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__developer_liability, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(liab_tr_t5, observed).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__developer_liability, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(liab_tr_t10, observed).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__developer_liability, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(liab_tr_t15, observed).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__developer_liability, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(liab_tr_t20, observed).
narrative_ontology:measurement(liab_tr_t25, liability_attribution__developer_liability, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(liab_tr_t25, observed).
narrative_ontology:measurement(liab_tr_t30, liability_attribution__developer_liability, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(liab_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t5, liability_attribution__developer_liability, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(liab_be_t5, observed).
narrative_ontology:measurement(liab_be_t10, liability_attribution__developer_liability, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(liab_be_t10, observed).
narrative_ontology:measurement(liab_be_t15, liability_attribution__developer_liability, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(liab_be_t15, observed).
narrative_ontology:measurement(liab_be_t20, liability_attribution__developer_liability, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(liab_be_t20, observed).
narrative_ontology:measurement(liab_be_t25, liability_attribution__developer_liability, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(liab_be_t25, observed).
narrative_ontology:measurement(liab_be_t30, liability_attribution__developer_liability, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(liab_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t5, liability_attribution__developer_liability, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(liab_su_t5, observed).
narrative_ontology:measurement(liab_su_t10, liability_attribution__developer_liability, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(liab_su_t10, observed).
narrative_ontology:measurement(liab_su_t15, liability_attribution__developer_liability, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(liab_su_t15, observed).
narrative_ontology:measurement(liab_su_t20, liability_attribution__developer_liability, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(liab_su_t20, observed).
narrative_ontology:measurement(liab_su_t25, liability_attribution__developer_liability, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(liab_su_t25, observed).
narrative_ontology:measurement(liab_su_t30, liability_attribution__developer_liability, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(liab_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__developer_liability, 0.12).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, technology_design_incentive_structure).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, regulatory_jurisdictional_arbitrage).

% DUAL FORMULATION NOTE:
% liability_attribution kernel decomposes into three structurally distinct constraints: (1) developer_liability: developers bear primary responsibility (this story); (2) deployer_liability: deployers bear primary responsibility due to deployment-context control; (3) shared_liability: responsibility distributed by causal contribution and control. Each reading instantiates a different victim/beneficiary structure and produces different incentive effects on developer safety investment and deployer use-restriction investment. The three stories are sibling readings linked by network.affects_constraints; they share a single contested kernel but differ in their ε values, beneficiary/victim sets, and structural justifications. See kernel_context for reading relations and cs_structure for axioms distinguishing the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__developer_liability, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
