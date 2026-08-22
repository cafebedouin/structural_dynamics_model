% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text — Corporate Moat Reading
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   A dominant share of the modern software stack — cloud infrastructure, ML
 *   frameworks, cryptographic libraries, developer tooling — ships under
 *   permissive license texts (MIT, BSD, Apache-2.0) that grant anyone the
 *   right to use, modify, embed, and sell the code with no compensation or
 *   contribution obligation. This story instantiates the corporate-moat
 *   reading of that kernel: the standing arrangement under which firms build
 *   proprietary derivative products and paid managed services on
 *   commons-produced code while the individuals who maintain the code bear
 *   its costs unpaid. Under this reading the arrangement operates as a snare:
 *   the genuine coordination achievement (frictionless reuse) functions as
 *   the cover story, while the durable operation is a one-way value transfer
 *   sustained not by participant net benefit but by structural exit-blocking
 *   — relicensing is legally blocked by scattered copyright and work-for-hire
 *   assignments, and dependency entrenchment punishes any project that tries.
 *   The ε referent is the standing permissive arrangement as this reading
 *   assesses it, never the reciprocity regime this reading would prefer.
 *   Family decomposition: 'permissive licensing' splits into three readings
 *   of one kernel text (see network.dual_formulation_note); this file authors
 *   only the moat reading. KEY AGENTS (by structural relationship): -
 *   hyperscale_cloud_operators: Primary beneficiary (institutional/arbitrage)
 *   — sells managed versions of commons-built systems at planetary scale -
 *   proprietary_software_vendors: Secondary beneficiary (powerful/arbitrage)
 *   — bundles the code into closed products at zero license cost -
 *   volunteer_maintainers: Primary target (powerless/identity_locked) —
 *   performs unpaid upkeep, security response, and release work; cannot
 *   relicense - independent_oss_contributors: Secondary target
 *   (moderate/constrained) — donates patches whose value accrues to
 *   productizing firms - permissive_license_stewards: Agenda setter
 *   (institutional/identity_locked) — publishes and ideologically defends the
 *   texts - security_research_community: Excluded critic (organized/mobile) —
 *   documents the unfunded-maintenance exposure from outside governance -
 *   competition_regulators: Analytical observer (institutional/analytical) —
 *   examines cloud concentration and resilience rules
 *
 * KEY AGENTS:
 *   - hyperscale_cloud_operators: Primary beneficiary (institutional/arbitrage)
 *   - proprietary_software_vendors: Secondary beneficiary (powerful/arbitrage)
 *   - volunteer_maintainers: Primary target (powerless/identity_locked)
 *   - independent_oss_contributors: Secondary target (moderate/constrained)
 *   - permissive_license_stewards: Agenda setter (institutional/identity_locked)
 *   - security_research_community: Excluded critic (organized/mobile)
 *   - competition_regulators: Analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.6).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.55).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text — Corporate Moat Reading").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "economic/technological/legal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, 'fb931772-0d9a-4d2e-ab0c-d5d2100bf587').
narrative_ontology:cs_kernel_codification('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', fixed_text).
narrative_ontology:cs_authority_grounding('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', lineage).
narrative_ontology:cs_interpretation_layer_present('fb931772-0d9a-4d2e-ab0c-d5d2100bf587').
narrative_ontology:cs_reading_relation('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', foundational, uncompensated_proprietary_appropriation_is_exploitation).
narrative_ontology:cs_axiom_status(uncompensated_proprietary_appropriation_is_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', uncompensated_proprietary_appropriation_is_exploitation, deontological).
narrative_ontology:cs_axiom('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', secondary, supply_erosion_from_uncompensated_burden).
narrative_ontology:cs_axiom_status(supply_erosion_from_uncompensated_burden, holdable).
narrative_ontology:cs_axiom_grounding('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', supply_erosion_from_uncompensated_burden, empirically_contingent).
narrative_ontology:cs_reference_frame('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', unconditional_grant_without_reciprocity).
narrative_ontology:cs_drift_state('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', cloud_extraction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb931772-0d9a-4d2e-ab0c-d5d2100bf587', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, hyperscale_cloud_operators).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, volunteer_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, independent_oss_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, independent_oss_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Foundations, license-steward bodies, and project leads who selected MIT/BSD/Apache terms and continue to publish, maintain, and publicly defend them. They run trademark programs, approve derivative license variants, and issue statements framing unrestricted reuse as the definition of openness. Their institutions were chartered around these texts; endorsing restrictions would contradict their founding identity. Leaving the position would mean dismantling the institutions themselves.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, permissive_license_stewards, agenda_setter,
    institutional, generational, identity_locked, global).

% Operate the large cloud platforms that sell managed versions of widely-used permissively-licensed databases, orchestration layers, and ML frameworks. They run the code at planetary scale, charge customers for the managed experience, and contribute upstream selectively where a patch serves their own roadmap. Their engineering depth lets them sustain private forks if a project turns hostile, so departing any single project costs them little.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, hyperscale_cloud_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Ship closed-source products that bundle permissively-licensed components — compilers, libraries, codecs, crypto stacks. The texts impose no fee, no disclosure, and no contribution duty, so component acquisition cost is effectively zero. Substituting a restrictively-licensed component would trigger procurement review and possible rewrite, but the vendor bears none of the upstream upkeep that keeps the components alive.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Individual developers who keep critical packages alive — triaging issues, reviewing patches, cutting releases, responding to security incidents, usually nights and weekends beside day jobs. Most receive little or no money; some pay for hosting out of pocket. Relicensing is rarely available to them: copyright in a mature codebase is scattered across thousands of past contributors, corporate employers hold rights to work-for-hire commits, and the installed base of dependents makes any term change disruptive. Walking away means abandoning users and a project that anchors their reputation and sense of purpose.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, volunteer_maintainers, payer,
    powerless, biographical, identity_locked, global).

% Developers outside corporate employment who send patches, documentation, and bug fixes. They accrue reputation and portfolio value and many enjoy the work, but their labor flows into codebases that firms then productize at no charge. Stepping back costs them community standing and, often, career prospects tied to their contribution history.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, independent_oss_contributors, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, independent_oss_contributors, beneficiary).

% Auditors and incident responders who examine widely-deployed open components. After failures like the Log4j and xz episodes they document how few hands and dollars stand behind critical code, and they press for funded-maintenance mandates. They operate outside license governance, which has no seat for them; their findings surface episodically after breaches rather than shaping license policy.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, security_research_community, excluded,
    organized, immediate, mobile, global).

% National and regional authorities examining cloud-market concentration and, recently, cyber-resilience rules that touch open-source maintenance. They gather evidence from every seat, can compel disclosure, and could reshape the economics through procurement rules or liability regimes, though license terms themselves sit largely outside their current remit.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, competition_regulators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, hyperscale_cloud_operators).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates per-use legal negotiation: one standardized grant lets any party — commercial or not — integrate, modify, and redistribute the code without seeking permission, solving the transaction-cost problem that copyright's all-rights-reserved defaults create for cumulative software building.
% TRANSFER_FUNCTION: Moves uncompensated labor — maintenance, security response, documentation, release engineering — from volunteer and independent producers into the product lines and managed-service margins of firms that ship the code commercially, with no reciprocal flow of money or code back to the source projects.
% ABSENT_VOICES: Burned-out former maintainers who have left the conversation entirely, security researchers who sit outside license governance, and copyleft stewards whose alternative was marginalized in the infrastructure niches where permissive texts became standard. All three would object that upkeep costs are externalized onto unpaid labor; none holds a seat in the forums where license policy is defended.
% DISAPPEARANCE_RATIONALE: If the permissive texts ceased to bind overnight, the software economy would rearrange violently: every closed product bundling the code and every managed cloud service built on it would become infringing at once; firms would scramble to negotiate, replace, or fork; prices and roadmaps would shift industry-wide; and maintainers would suddenly hold bargaining power they have never had. The world runs on this arrangement — its removal is not absorbable.
% FOUNDING_PROBLEM: Copyright's default terms made ordinary software reuse a legal hazard: every integration, redistribution, or commercial embedding required a negotiated permission. Permissive license texts were drafted to delete that friction — to let code circulate, be studied, modified, and sold without bilateral deals.
% FOUNDING_PROBLEM_CORROBORATION: From outside the benefiting parties: peer-reviewed maintainer-census and burnout studies, post-incident engineering analyses (Heartbleed, Log4j, xz) authored by security researchers, and methodologically independent funding-gap surveys corroborate that the friction problem is solved while upkeep goes unfunded. Corporate beneficiaries attest the opposite — that the arrangement's justification is intact — and no neutral body adjudicates between them; hence contested.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. claimed_type=snare states this reading's structural verdict: identifiable victims, a coordination story that functions as cover, and persistence that depends on suppressing exits rather than on participant net benefit. The metrics describe the arrangement's actual operation. Extractiveness 0.60 (interval end): heavy but not total — maintainers retain career-capital and ecosystem-health returns, and some corporate spend reaches foundations, so the transfer is large yet partial. Suppression 0.55 is a raw structural property, deliberately unscaled: no coercion enforces the texts (requires_active_enforcement=false); the binding force is exit-blocking — scattered copyright and work-for-hire rights make relicensing practically unavailable, and gift-culture identity makes restriction feel like betrayal. Theater 0.26: sustainability summits, pledge walls, and appreciation branding perform concern while the value flow is untouched; the ratio stays low because the arrangement needs little performance to persist. Accessibility_collapse 0.50: alternatives (copyleft relaunches, dual licensing, patronage models) remain legible but collapse against network effects once a codebase is entrenched. Resistance 0.55: license-change waves, fork movements, funder campaigns, and cyber-resilience lobbying are real and rising. All three series share one six-point grid (T0..T30); suppression_requirement is authored because the story traces a hardening exit-blockade rather than a static enforcement picture — the arrangement's suppressive force grew as corporate entrenchment deepened. Receipt surface: the gains demonstrably accrue to the hyperscaler seat (largest managed-service margins on commons-built systems), and fixing is prohibitive relative to benefit — mid-life relicensing requires locating thousands of copyright holders, surviving corporate fork threats, and rebuilding contributor trust.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the hyperscaler and vendor seats the arrangement computes as benign coordination they fund selectively and could exit cheaply — arbitrage-grade exit pulls their experienced burden toward subsidy. From the maintainer seat the identical structure computes as one-way appropriation with no affordable exit. The steward seat experiences the texts as near-natural law — openness itself — and resists the suggestion that a grant could victimize. The engine computes these per-seat classifications from power, exit, and declared position; the authored snare claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. hyperscale_cloud_operators and proprietary_software_vendors sit in beneficiaries with arbitrage exit — nearest the beneficiary end of d, so effective extraction inverts toward subsidy for them. volunteer_maintainers and independent_oss_contributors sit in victims; identity-lock and constrained exit push them toward the full-target end, maximizing their effective extraction. permissive_license_stewards are neither declared beneficiary nor victim; their institutional continuity rides on the texts, placing them mildly on the beneficiary side. Every operating seat holds global scope, which raises verification difficulty and thus amplifies effective extraction for targets. Suppression enters the computation unscaled; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — copyright's all-rights-reserved default making routine reuse legally hazardous — was real and is solved wherever the texts operate. The arrangement nonetheless persists through entrenchment rather than through the founding problem's continued pull, and the parties dispute whether the problem is live (new code, new reuse daily) or dead (the negotiation world is gone; what remains is appropriation infrastructure). Mandatrophy discipline cuts both ways here: it blocks the commons reading from laundering the value transfer as pure coordination cost, and it blocks a lazy piton verdict — theater is low and the extraction is live, so the arrangement is neither mostly performance nor inertial residue. mandatrophy_resolved is therefore left undeclared: the function is still performed daily even as its justificatory story frays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story is one reading of the permissive_license_text kernel — the corporate_moat_reading. What would the sibling readings change structurally?',
    'Compare compiled classifications across the three sibling stories: commons_coordination_reading (ε near coordination cost, rope-shaped) and copyleft_counterfactual_reading (referent shifted to the reciprocity counterfactual). The disagreement is located in the normative status of the grant''s asymmetry: constitutive coordination benefit versus constitutive extraction channel.',
    'Adopting the commons reading would drop ε toward ~0.1 and reclassify toward rope; adopting the copyleft reading relocates the referent to a counterfactual arrangement and widens the victim set to all commons participants. This story''s snare verdict holds only within the moat reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Indexical position of this story within the permissive_license_text kernel contest.').

omega_variable(
    epsilon_referent_fixation,
    'Does ε measure the standing permissive arrangement as this reading assesses it, rather than the reciprocity regime this reading endorses?',
    'Audit that base_properties.extractiveness is authored against the existing MIT/BSD/Apache deployment pattern, not against a hypothetical GPL-style regime the reading would prefer.',
    'If the referent slipped to the endorsed alternative, ε would collapse toward zero and the story would become vacuous advocacy; a fixed referent keeps the reading comparable to its siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_fixation, conceptual, 'Guards the ε referent against drift toward the reading''s endorsed alternative.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is maintainer-side suppression structural (relicensing blocked by scattered copyright, work-for-hire assignments, dependent entrenchment) or internalized (gift-culture guilt and stewardship identity that make restriction unthinkable even where legally feasible)?',
    'Post-exit trajectory study: track maintainers who fully leave — do they subsequently support restrictive or dual licensing? Separate relicensing attempts blocked by law from those abandoned for identity reasons.',
    'If largely internalized, effective suppression exceeds the structural measure and persists after any legal reform; reform targeting legal barriers alone would under-deliver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism on the payer side.').

omega_variable(
    reciprocity_offset_measurement,
    'What fraction of the value firms derive from permissively-licensed code returns upstream as contributions, funding, or free infrastructure?',
    'Commit-provenance telemetry crossed with disclosed sponsorship and foundation budgets, set against estimated derived revenue from managed services and bundled products.',
    'Material offsets would lower ε and could move the computed type toward tangled_rope; negligible offsets confirm the snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_offset_measurement, empirical, 'Whether corporate reciprocity materially offsets the measured value transfer.').

omega_variable(
    maintainer_coalition_potential,
    'Can powerless maintainers convert diffuse grievance into coalition power — foundation bargaining, collective relicensing vehicles, regulatory leverage under cyber-resilience rules?',
    'Track the formation and durability of maintainer associations and their measurable effect on license-term negotiations and procurement requirements.',
    'Successful coalition would raise resistance, narrow the extraction channel, and could shift the arrangement''s dynamics from unilateral appropriation toward negotiated terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maintainer_coalition_potential, empirical, 'Coalition-power potential of the powerless payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perm_tr_t6, permissive_license_text__corporate_moat_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__corporate_moat_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(perm_tr_t18, permissive_license_text__corporate_moat_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__corporate_moat_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(perm_tr_t30, permissive_license_text__corporate_moat_reading, theater_ratio, 30, 0.26).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perm_be_t6, permissive_license_text__corporate_moat_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__corporate_moat_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(perm_be_t18, permissive_license_text__corporate_moat_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__corporate_moat_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(perm_be_t30, permissive_license_text__corporate_moat_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(perm_su_t6, permissive_license_text__corporate_moat_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement(perm_su_t12, permissive_license_text__corporate_moat_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(perm_su_t18, permissive_license_text__corporate_moat_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(perm_su_t24, permissive_license_text__corporate_moat_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(perm_su_t30, permissive_license_text__corporate_moat_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% 'Permissive licensing' decomposes into three structurally distinct constraints sharing one kernel text, authored separately per the ε-invariance principle: this corporate-moat reading (ε ≈ 0.60, claimed snare — the standing arrangement as an extraction channel), the commons-coordination reading (ε near coordination cost, rope-shaped — the same texts as friction-eliminating coordination), and the copyleft-counterfactual reading (referent relocated to a viral-reciprocity counterfactual). Each file links its siblings via affects_constraints; the upstream commons reading is typically cited as evidence by the downstream moat and copyleft readings, which is why the family edges run from this file to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
