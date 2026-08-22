% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation — Copyleft as Commons Reading
 *   domain: legal/technological/economic
 *
 * SUMMARY:
 *   The GPL's reciprocity clause requires anyone who distributes a derivative
 *   of GPL-licensed code to license that derivative under the GPL and supply
 *   its source. This story instantiates ONE reading of that kernel — the
 *   commons reading — under which the clause is an institutional technology
 *   that prevents enclosure of a shared resource by making reciprocity the
 *   price of admission: the beneficiary is the commons as a continuing
 *   institution, and the cost-bearers are individual exit-maximizers who
 *   would otherwise appropriate improvements into closed products. The ε
 *   referent is the standing arrangement under contest — the reciprocity
 *   obligation as it actually operates — assessed by this reading's own
 *   lights, which judge the imposed cost real, deliberate, and a fair price
 *   for commons persistence; hence medium extractiveness, not negligible and
 *   not predatory. The sibling readings (copyleft_as_freedom_reading,
 *   copyleft_as_restriction_reading) instantiate different constraints with
 *   different beneficiary sets and ε values and are authored separately; they
 *   are linked, not averaged, here. KEY AGENTS (by structural relationship):
 *   - free_software_commons: Primary beneficiary (organized/constrained) —
 *   the pooled codebase whose persistence the obligation maintains -
 *   gpl_project_maintainers: Beneficiary-stewards (moderate/identity_locked)
 *   — receive and give contributions under the reciprocal terms -
 *   downstream_commercial_deployers: Net beneficiary with compliance costs
 *   (powerful/mobile) — symmetric position - proprietary_integration_firms:
 *   Primary target (powerful/constrained) — bears the closure-forfeiture cost
 *   - embedded_systems_vendors: Secondary target (organized/constrained) —
 *   the largest historical violation class - fsf_license_stewards: Agenda
 *   setter (institutional/identity_locked) — authors and interprets the
 *   license - software_freedom_enforcers: Enforcement arm
 *   (organized/identity_locked) — converts the license text into binding
 *   force - embedded_device_end_users: Excluded rights-holders
 *   (powerless/trapped) — hold unexercisable rights -
 *   open_source_legal_scholars: Analytical observer — produces the external
 *   record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.34).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation — Copyleft as Commons Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "legal/technological/economic").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '7214359d-9409-4653-90df-71b9537a2f99').
narrative_ontology:cs_kernel_codification('7214359d-9409-4653-90df-71b9537a2f99', fixed_text).
narrative_ontology:cs_authority_grounding('7214359d-9409-4653-90df-71b9537a2f99', lineage).
narrative_ontology:cs_interpretation_layer_present('7214359d-9409-4653-90df-71b9537a2f99').
narrative_ontology:cs_reading_relation('7214359d-9409-4653-90df-71b9537a2f99', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('7214359d-9409-4653-90df-71b9537a2f99', gpl_reciprocity_obligation__copyleft_as_restriction_reading, influences).
narrative_ontology:cs_axiom('7214359d-9409-4653-90df-71b9537a2f99', foundational, commons_survival_requires_reciprocity).
narrative_ontology:cs_axiom_status(commons_survival_requires_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('7214359d-9409-4653-90df-71b9537a2f99', commons_survival_requires_reciprocity, instrumental).
narrative_ontology:cs_axiom('7214359d-9409-4653-90df-71b9537a2f99', secondary, enclosure_is_default_failure_mode).
narrative_ontology:cs_axiom_status(enclosure_is_default_failure_mode, holdable).
narrative_ontology:cs_axiom_grounding('7214359d-9409-4653-90df-71b9537a2f99', enclosure_is_default_failure_mode, empirically_contingent).
narrative_ontology:cs_reference_frame('7214359d-9409-4653-90df-71b9537a2f99', reciprocity_conditioned_commons).
narrative_ontology:cs_drift_state('7214359d-9409-4653-90df-71b9537a2f99', contemporary_cloud_delivery_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7214359d-9409-4653-90df-71b9537a2f99', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, free_software_commons).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_project_maintainers).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_commercial_deployers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integration_firms).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, embedded_systems_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_commercial_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The pooled body of GPL-licensed source code and the communities that maintain it. Every compliant derivative returns its improvements to the pool; the pool grows and no participant can withdraw a piece of it into a closed product. Its continuation depends on the license terms holding across millions of lines contributed by thousands of hands.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, free_software_commons, beneficiary,
    organized, civilizational, constrained, global).

% Individual developers and small teams who accept patches, cut releases, and decide what enters projects such as the Linux kernel or GNU tools. They receive contributions they did not pay for and give away their own work on reciprocal terms. Leaving usually means abandoning a project they have invested years in and a community whose norms they share.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_project_maintainers, beneficiary,
    moderate, biographical, identity_locked, global).

% Companies that run GPL software in their infrastructure — web farms, build systems, backend services — without distributing modified binaries. They get industrial-grade shared infrastructure at zero license cost and carry compliance overhead: tracking provenance, honoring attribution, and restructuring products before any distribution triggers source-disclosure duties. Permissively licensed substitutes exist for many components but switching at scale is expensive.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_commercial_deployers, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_commercial_deployers, payer).

% Firms that want to build closed products incorporating GPL components — shipping modified kernels, libraries, or toolchains inside proprietary appliances and applications. The license offers two doors: publish the derivative source, or forgo the code. For products already architected around GPL parts, unwinding to alternatives means re-engineering; taking the disclosure door can mean handing competitors their differentiation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integration_firms, payer,
    powerful, biographical, constrained, global).

% Manufacturers putting GPL code inside routers, televisions, vehicles, and industrial controllers. Their products ship binaries to customers, which activates the source-disclosure terms, and their industry practice historically favored withholding source. Compliance requires firmware rebuild pipelines and supplier negotiations; refusal invites infringement claims from enforcement organizations.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, embedded_systems_vendors, payer,
    organized, biographical, constrained, global).

% The Free Software Foundation, which authors the license texts, publishes interpretive guidance, and defines the movement's normative line. Its authority rests on continuity with the founding text and its author's tradition. It collects no fees; its return is the survival of the arrangement it designed and the continued relevance of its stewardship.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, fsf_license_stewards, agenda_setter,
    institutional, generational, identity_locked, global).

% Organizations such as the Software Freedom Conservancy and predecessor enforcement projects that investigate suspected violations, send compliance notices, and litigate when notices fail. They operate on donations, seek compliance rather than damages, and their docket is the visible edge of the license's enforcement.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_freedom_enforcers, agenda_setter,
    organized, biographical, identity_locked, global).

% Owners of routers, smart TVs, and vehicles containing GPL code. The license grants them rights to the corresponding source, but they rarely know those rights exist, lack the technical means to assert them, and have no seat where compliance norms are negotiated between vendors and enforcement groups.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, embedded_device_end_users, excluded,
    powerless, biographical, trapped, global).

% Academic lawyers and economists who track license litigation, measure ecosystem composition, and debate whether reciprocity terms help or hinder adoption. They take no operational side and produce the external record the other seats cite.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, free_software_commons).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider and enclosure problem for shared source code: improvements made to common infrastructure remain available to every contributor and downstream user, and no single participant can privatize collectively produced value while continuing to draw on the pool.
% TRANSFER_FUNCTION: Moves source-code rights and improvement disclosures from derivative-work builders — firms integrating GPL code into shipped products — to the commons and its downstream users; equivalently, it transfers the option value of closure from integrators to the public pool.
% ABSENT_VOICES: Embedded-device end users hold license rights they cannot exercise and are absent from every venue where compliance norms are set; firms that quietly avoid GPL code altogether also never enter the conversation, so the obligation's costs are debated mainly by parties already inside it.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight, the existing pool would persist for a time, but new contributions would immediately fragment into proprietary forks: firms shipping GPL-derived kernels and toolchains would stop publishing source, competitors' access to improvements would close, and the infrastructure economy built on guaranteed future sharing would reorganize around permissive licenses with enclosure pressure unopposed.
% FOUNDING_PROBLEM: In the early 1980s, commercially developed software was rapidly becoming proprietary and users were losing the ability to study, modify, and share the programs they depended on. The arrangement was built to guarantee that improvements to freely shared software could never again be locked away by whoever got there first.
% FOUNDING_PROBLEM_CORROBORATION: Corporate compliance counsel outside the beneficiary set attest the obligation is real and binding — they build product-review processes around it; courts in Germany and settlement agreements in the United States have enforced it; academic IP scholarship corroborates both the enclosure-prevention record and the argument that cloud delivery has mutated the original problem faster than the license's trigger conditions track it. No attesting source is inside the benefiting parties alone.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.42 (medium, per the reading's own assessment): the obligation deliberately imposes asymmetric costs on firms seeking proprietary integration, and those costs are sometimes existential for small vendors, but they are avoidable ex ante — the constraint conditions entry rather than trapping occupants — and the reading judges them a fair admission price rather than rent. Suppression is 0.34: enforcement runs through copyright law and litigation, which is real but narrow, and alternatives (permissive-licensed code, clean-room reimplementation, negotiated commercial terms) remain open, so accessibility_collapse is low at 0.28. Resistance is 0.48: three decades of corporate friction, avoidance engineering, and compliance disputes, occasionally litigated. Theater is low (0.22) because the function demonstrably operates — major GPL pools remain unenclosed after decades — with a modest rhetorical surplus where movement rhetoric outruns enforcement reach. The temporal series share one grid (1991–2025, eight points, all three metrics at every point). Base extractiveness rises as industry adoption widens the bound population, peaks around 2011, then eases as dual-licensing and compliance routines normalize. The suppression_requirement series tracks enforcement-capacity history specifically: a quiet first decade, a ramp through the BusyBox-era litigation wave, a plateau, then a dip after the SFC v. Vizio standing setback raised doubts about US enforcement routes.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the maintainer and commons seats, the obligation is the mechanism that keeps gifted work from being strip-mined — an experience near pure coordination. From the proprietary-integration seat, the same clause reads as confiscation of product strategy — an experience near pure extraction. The deployer seat sits between: genuine subsidy received, real compliance overhead paid. The engine derives these per-seat classifications from the power, exit, and beneficiary/victim data; the authored claim does not adjudicate among them. Inter-institutionally, the FSF and enforcement organizations experience the constraint as their life's work, while corporate legal departments experience it as an external tax on architecture decisions — same instrument, opposite phenomenology. Same-level lateral divergence appears between the two powerful firm classes: deployers (mobile exit, no distribution trigger fired) and integrators (constrained exit, trigger fired) face the identical license text from structurally different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The commons sits nearest the beneficiary pole (d near 0.05): every compelled disclosure lands in the pool it constitutes. Maintainers sit low (d near 0.15): they receive the pool's protection directly. Deployers derive near-symmetric (d near 0.4): large subsidy received, real overhead borne, mobile exit. Integration firms and embedded vendors sit near the target pole (d near 0.8–0.85): they bear the transfer, and their constrained exits (architectural lock-in, re-engineering cost) push them toward full-target rather than mobile-target treatment. Stewards and enforcers derive moderately low (d near 0.2–0.25): they collect mission-relevance rather than rents. Excluded end users derive mildly beneficiary-leaning (d near 0.3) on paper but collect nothing while excluded — the gap between their derived position and their realized position is recorded in the commons_beneficiary_reification omega. No directionality overrides are used: the derivation chain distinguishes the two powerful firm classes through their exit options, which is exactly the axis on which they differ.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing lock-away of improvements to shared software — is contested, not dead: its original trigger (binary distribution) fires less often as delivery moves to cloud services, but the underlying appropriation pressure has mutated rather than disappeared, which is why AGPL exists. Because founding_problem_status is contested and disappearance_verdict is world_rearranges, the mismatch consumer finds no dead-mandate-plus-dependence signature; this is not a zombie arrangement. The classification discipline cuts both ways here: reading the obligation as pure coordination ignores that its asymmetry is deliberate and enforced — someone specific pays through the structure; reading it as pure extraction ignores that the paying class is defined by intent to appropriate and that a functioning, non-theatrical coordination outcome (unenclosed pools) is observable. Tangled rope is the honest claim from this seat, and the medium ε reflects the reading's judgment that the extracted cost is the price of the coordination rather than parasitic on it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel gpl_reciprocity_obligation — the commons reading, which locates the beneficiary in the commons as an institution and the cost-bearers in individual exit-maximizers. How would the sibling readings restructure the classification?',
    'Author the sibling stories (copyleft_as_freedom_reading, copyleft_as_restriction_reading) and compare computed types: the freedom reading shifts the beneficiary to holders of user freedoms and narrows victims to freedom-diminishing distributors; the restriction reading raises epsilon and widens the victim class to bound business models generally.',
    'If the freedom reading dominates, the obligation computes closer to a defended coordination norm with weaker measured extraction; if the restriction reading dominates, it computes as substantially extractive with a broad victim class. The disagreement is located in beneficiary identification and in whether mandatory reciprocity is a fair admission price or an imposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the GPL reciprocity kernel governs the beneficiary/victim structure and epsilon.').

omega_variable(
    counterfactual_enclosure_threat,
    'Would the shared codebase actually have been enclosed absent the reciprocity obligation — is mandatory reciprocity load-bearing, or would permissive licensing have preserved an open pool anyway?',
    'Compare long-run trajectories of permissive-licensed pools (BSD Unix lineage, Apache, Chromium) for enclosure episodes against copyleft pools; test whether closure attempts correlate with the absence of reciprocal terms.',
    'If permissive commons survived comparably, part of the measured extraction is unjustified overhead and the reading''s fairness claim weakens; if permissive pools show systematic capture, the obligation is the load-bearing wall and its extraction is the price of persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_enclosure_threat, empirical, 'Whether the enclosure threat the obligation answers is real or counterfactual.').

omega_variable(
    commons_beneficiary_reification,
    'Does the commons as an institution actually collect the gains, or do the gains pass through to identifiable persons and firms — making ''the commons'' a reification of diffuse individual benefit?',
    'Trace compelled disclosures to incorporation: who builds returned source into products and captures the resulting savings; measure whether any seat captures disproportionate rents versus universal diffusion.',
    'If gains diffuse universally with no capturing seat, receipt behaves as diffuse despite the institutional beneficiary framing, altering capture and piton-side analysis; if specific ecosystems capture disproportionate gains, the arrangement tilts toward managed extraction with an identifiable accruer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_beneficiary_reification, conceptual, 'Whether the declared institutional beneficiary is a real collector or a reification.').

omega_variable(
    steward_identity_lock,
    'How much of the enforcement apparatus''s persistence rests on internalized ideological identity — career and self-concept fused with free-software stewardship — rather than on structural incentive?',
    'Observe enforcement capacity through leadership transitions and funding shocks (the post-2019 FSF controversy, SFC fundraising cycles): if activity persists through personnel turnover the mechanism is structural; if it decays with cohorts, it is identity-carried.',
    'If identity-carried, part of the measured suppression is internalized rather than structural, the constraint''s coercive surface is thinner than litigation counts suggest, and classification becomes sensitive to steward attrition events.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(steward_identity_lock, empirical, 'Structural versus internalized basis of enforcement persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gpl__tr_t1996, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1996, 0.12).
narrative_ontology:measurement(gpl__tr_t2001, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2001, 0.14).
narrative_ontology:measurement(gpl__tr_t2006, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2006, 0.16).
narrative_ontology:measurement(gpl__tr_t2011, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2011, 0.18).
narrative_ontology:measurement(gpl__tr_t2016, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(gpl__tr_t2021, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2021, 0.22).
narrative_ontology:measurement(gpl__tr_t2025, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1991, 0.2).
narrative_ontology:measurement(gpl__be_t1996, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1996, 0.28).
narrative_ontology:measurement(gpl__be_t2001, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2001, 0.38).
narrative_ontology:measurement(gpl__be_t2006, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2006, 0.46).
narrative_ontology:measurement(gpl__be_t2011, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2011, 0.5).
narrative_ontology:measurement(gpl__be_t2016, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2016, 0.47).
narrative_ontology:measurement(gpl__be_t2021, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement(gpl__be_t2025, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1991, 0.05).
narrative_ontology:measurement(gpl__su_t1996, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1996, 0.08).
narrative_ontology:measurement(gpl__su_t2001, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2001, 0.15).
narrative_ontology:measurement(gpl__su_t2006, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2006, 0.3).
narrative_ontology:measurement(gpl__su_t2011, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2011, 0.38).
narrative_ontology:measurement(gpl__su_t2016, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement(gpl__su_t2021, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2021, 0.36).
narrative_ontology:measurement(gpl__su_t2025, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2025, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'GPL viral licensing' per the epsilon-invariance principle. The single natural-language concept covers three structurally distinct claims: (1) this story — the clause as commons-protecting institutional technology, beneficiary = the commons as institution, victims = exit-maximizing integrators, medium epsilon; (2) copyleft_as_freedom_reading — the clause as a guarantee of end-user freedoms, beneficiary = freedom-holders, different victim set; (3) copyleft_as_restriction_reading — the clause as a business-model prohibition, elevated epsilon, broad victim class. Each member carries its own epsilon, beneficiaries, and victims; the family is linked through network.affects_constraints in all three files. The upstream member (this reading, highest adoption legitimacy) influences the environment in which the restriction reading registers its grievances.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
