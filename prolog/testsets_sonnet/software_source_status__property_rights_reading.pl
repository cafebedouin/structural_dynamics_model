% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Proprietary Software Licensing as Legitimate Property Right
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the property_rights_reading of the
 *   software_source_status kernel: source code is a proprietary asset,
 *   licensing restrictions are legitimate exercises of ownership analogous to
 *   any other IP right, and end users hold contractual rights only — not
 *   ownership, modification, or inspection rights — over the software they
 *   license. This is a deliberately narrow, internally coherent reading. It
 *   is NOT a story about whether open-source development is better or whether
 *   software freedom is an ethical imperative; those are separate constraints
 *   (pragmatic_development_reading, freedom_imperative_reading) with
 *   different beneficiary/victim structures and different ε values, linked
 *   here via network.affects_constraints. Under this reading, the
 *   coordination function (funding engineering investment through
 *   excludability) is real, and the extraction (locking users and downstream
 *   developers out of modification and interoperability) is also real and
 *   requires active enforcement (DMCA anti-circumvention, EULA litigation,
 *   DRM) — hence tangled_rope, not mountain or rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.52).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.61).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Proprietary Software Licensing as Legitimate Property Right").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '9a6e2839-f70e-4179-8f3b-34bca3fb60b7').
narrative_ontology:cs_kernel_codification('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', distributed).
narrative_ontology:cs_authority_grounding('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', extraction).
narrative_ontology:cs_interpretation_layer_present('9a6e2839-f70e-4179-8f3b-34bca3fb60b7').
narrative_ontology:cs_reading_relation('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', foundational, creative_labor_grounds_exclusive_control).
narrative_ontology:cs_axiom_status(creative_labor_grounds_exclusive_control, holdable).
narrative_ontology:cs_axiom_grounding('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', creative_labor_grounds_exclusive_control, deontological).
narrative_ontology:cs_axiom('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', foundational, licensing_restriction_is_ownership_not_injustice).
narrative_ontology:cs_axiom_status(licensing_restriction_is_ownership_not_injustice, holdable).
narrative_ontology:cs_axiom_grounding('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', licensing_restriction_is_ownership_not_injustice, conventional).
narrative_ontology:cs_axiom('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', secondary, users_hold_contractual_not_proprietary_interest).
narrative_ontology:cs_axiom_status(users_hold_contractual_not_proprietary_interest, holdable).
narrative_ontology:cs_axiom_grounding('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', users_hold_contractual_not_proprietary_interest, conventional).
narrative_ontology:cs_reference_frame('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', classical_authorial_property_doctrine).
narrative_ontology:cs_drift_state('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', post_open_source_movement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9a6e2839-f70e-4179-8f3b-34bca3fb60b7', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, enterprise_software_shareholders).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, end_users_without_modification_rights).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, interoperability_dependent_developers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, right_to_repair_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, software_engineers_at_vendor_firms).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, software_engineers_at_vendor_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write license terms, enforce them through copyright litigation and DRM, and lobby for statutory protections like the DMCA anti-circumvention provisions. Capture licensing revenue directly and set the terms under which anyone else may run, inspect, or modify the code. Can relocate incorporation and enforcement venue to whichever jurisdiction offers the strongest IP protection.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, proprietary_software_vendors, beneficiary).

% Fund software companies on the premise that exclusive control over source code is what makes the resulting revenue streams defensible and thus investable. Their return depends on the enforceability of restriction; they have no operational exposure to the restriction's effects on users.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, venture_capital_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Hold equity whose valuation depends on the company's ability to license, not sell, its software and to prevent competitors or users from replicating its function. Can divest if enforcement weakens, without personal exposure to the underlying product's restrictions.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, enterprise_software_shareholders, beneficiary,
    organized, biographical, arbitrage, global).

% Purchase or subscribe to software they cannot inspect, repair, or adapt to their needs. Bound by clickwrap terms they did not negotiate; modifying the product to fix a defect or extend its life voids warranty or breaches license and may trigger legal liability. Exit means abandoning data, workflows, and sunk training cost, not achieving equivalent function elsewhere.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, end_users_without_modification_rights, payer,
    powerless, biographical, constrained, global).

% Build products that must interface with proprietary platforms and are legally exposed for reverse-engineering interfaces the platform declines to document. Pay licensing fees, absorb compatibility risk when the vendor changes undocumented behavior, and cannot litigate on equal footing with the platform holder.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, interoperability_dependent_developers, payer,
    moderate, biographical, constrained, global).

% Argue that ownership of a device should include the right to inspect and modify its embedded software, and push legislation to that effect. Largely locked out of the licensing negotiations themselves; their remedy runs through slow legislative or regulatory processes rather than direct bargaining with vendors.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, right_to_repair_advocates, excluded,
    organized, biographical, constrained, national).

% Write the code that is then locked behind the license; their labor produces the asset without their holding any residual claim to it beyond salary. Benefit indirectly through employment and equity, but cannot themselves exercise the modification rights the license withholds from users, and are typically bound by employment agreements assigning all IP to the employer.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_engineers_at_vendor_firms, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, software_engineers_at_vendor_firms, beneficiary).

% Adjudicate disputes over the boundary between legitimate IP enforcement and anticompetitive lock-in (tying, interoperability refusal, DRM overreach). Take testimony from vendors, developers, and advocacy groups and can narrow or widen the scope of enforceable restriction through rulings and legislation.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, competition_and_ip_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Exclusive rights over source code let a firm capture the return on the engineering investment required to build complex software, which funds continued development, security maintenance, and support that would be underprovided if the code were immediately copyable by competitors.
% TRANSFER_FUNCTION: Moves control over modification, inspection, and reuse from users and downstream developers to the vendor, and moves payment from users to the vendor in exchange for a license to run (not own) the software; interoperability costs are shifted onto third-party developers who must reverse-engineer or license access to interfaces.
% ABSENT_VOICES: Right-to-repair advocates and long-tail users of abandoned software (orphaned by vendors who stop maintaining it while the license still forbids modification) are not party to the license negotiation and have no seat in setting its terms; their objections surface only through legislative advocacy or litigation years after the fact.
% DISAPPEARANCE_RATIONALE: If proprietary licensing enforceability vanished overnight, current vendor revenue models built on per-seat or subscription licensing would collapse or convert to service/support models; venture funding calculus for software would shift toward first-mover speed and service moats rather than code exclusivity; users would gain modification rights but vendors would lose the primary mechanism funding continued maintenance of complex codebases. The arrangement is load-bearing for a specific business model, not a background fact about software.
% FOUNDING_PROBLEM: Software development requires substantial up-front engineering investment; without a mechanism to prevent immediate free-riding by copiers, firms argued they could not recoup that investment or fund the ongoing maintenance and support complex software requires.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and their investors attest the problem remains live — security maintenance and feature development require sustained revenue that only exclusivity protects. Independent economists studying software markets and the free/open-source movement's four-decade track record of maintained, funded complex systems (operating system kernels, database engines, cryptographic libraries) attest the founding problem is empirically separable from the exclusivity mechanism: sustainable funding models exist without source restriction, which is corroboration from outside the beneficiary set that the coordination function and the extraction mechanism are not the same thing.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52) because the underlying coordination function — funding continued development — is genuine and partially justifies the transfer; it is not zero because the restriction extends well beyond recouping investment into perpetual rent extraction (subscription renewal, planned obsolescence via license expiry, interoperability gatekeeping) that has no clear terminus. Suppression (0.61) is higher than extraction because the enforcement apparatus (anti-circumvention law, contractual liability, technical DRM) forecloses self-help remedies even where extraction in a given case is modest — a user who simply wants to fix a bug in software they purchased faces the same legal exposure as a commercial pirate. Theater is low (0.22): most enforcement activity is functionally real (it does gate access), not merely performative, though litigation posturing and boilerplate license terms contribute a nontrivial performative share. Accessibility collapse (0.58) reflects that once the licensing regime is understood, workaround options (reverse engineering, jailbreaking, open alternatives) are legally risky or functionally incomplete for most users, though not fully closed — open-source substitutes exist for many categories, which keeps this below mountain-level collapse. Resistance (0.55) reflects active organized pushback (right-to-repair movements, open-source advocacy, regulatory scrutiny of tying and DRM) — this is not an uncontested constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor/agenda-setter seat, this is straightforwardly a rope: a legitimate property right that funds an investment they made and that no one is forced to purchase. From the end-user/payer seat, the same license terms compute as extraction with real suppression — they cannot fix, adapt, or transfer what they've paid for, and circumvention carries legal risk disproportionate to the harm caused. The engine's per-seat computation is expected to diverge along exactly this line; that divergence is not an error in the reading, it is what a tangled_rope structurally is.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors, their investors, and shareholders sit at the beneficiary end: they collect licensing revenue and control the terms, with mobile-to-arbitrage exit (can relocate IP strategy, jurisdiction, or business model). End users and interoperability-dependent developers sit at the target end: they pay through licensing fees and bear the restriction's costs (no repair, no modification, no interoperability guarantee) with constrained exit — switching costs, data lock-in, and network effects make exit costly even when legally available. Software engineers employed by vendors are a genuinely dual seat: their labor produces the asset, they benefit indirectly via employment, but they hold no residual claim to the code itself and are bound by IP-assignment clauses — hence payer/beneficiary secondary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (funding engineering investment) has NOT disappeared — complex software still requires sustained investment — which is why founding_problem_status is 'contested' rather than 'dead.' But the corroboration from outside the beneficiary set (economists, the sustained track record of funded open development models) establishes that the specific mechanism of source-exclusivity is not the only viable funding path, which is what keeps this a tangled_rope rather than a clean rope: the coordination story is real but is not the only story, and the persistence of source-restriction specifically (as opposed to funding software development generally) depends on continued active legal enforcement rather than being self-evidently necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ip_analogy_validity,
    'Is software genuinely analogous to physical or traditional creative property such that ownership-style exclusivity is the correct legal frame, or is the analogy itself doing unwarranted normative work (software''s near-zero marginal reproduction cost and functional/interoperability requirements distinguish it from a novel or a physical invention)?',
    'Comparative legal-economic analysis of how exclusivity regimes perform in domains with near-zero marginal cost and required interoperability versus domains without those features; historical comparison of patent vs. copyright vs. sui generis software protection outcomes across jurisdictions.',
    'If the analogy is weak, the property_rights_reading''s foundational premise is undermined even on its own terms, strengthening the case for the utilitarian_hybrid or pragmatic_development readings as better fits; if strong, this reading''s axioms hold without qualification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ip_analogy_validity, conceptual, 'Whether treating software as classical IP is a sound analogy or a category error with distributive consequences.').

omega_variable(
    recoupment_vs_perpetual_rent,
    'Where is the line, if any, between exclusivity that recoups genuine development investment and exclusivity that has become pure perpetual rent extraction (e.g., subscription renewal on unchanged software, planned license expiry, interoperability gatekeeping unrelated to development cost)?',
    'Firm-level financial disclosure correlating licensing revenue against ongoing development/maintenance spend over the product lifecycle; identify the point at which cumulative licensing revenue exceeds total development cost by orders of magnitude with no corresponding maintenance obligation.',
    'If most licensing revenue tracks ongoing investment, the tangled_rope''s coordination component is doing most of the work and extraction is closer to incidental; if revenue diverges sharply and persistently from investment, the coordination story is largely cover and the classification should trend toward snare for mature, minimally-maintained products.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recoupment_vs_perpetual_rent, empirical, 'Whether licensing revenue tracks investment recoupment or has decoupled into rent.').

omega_variable(
    kernel_reading_selection_basis,
    'What structural or contextual signal should determine which of the four kernel readings (property_rights, freedom_imperative, pragmatic_development, utilitarian_hybrid) applies to a given piece of software, given that all four are simultaneously defensible as general theories?',
    'This is inherently a preference/framing question rather than one resolvable by further data — different legal traditions, industries, and communities have stably held different readings for four decades without convergence.',
    'Determines whether this reading''s classification should be read as describing a universal default (most commercial software today) or one live option among contested alternatives whose relative prevalence shifts by sector and jurisdiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, preference, 'Committer-axis: which reading governs is a matter of contested normative commitment, not empirical settlement — routed here per Rule 2 rather than folded into this reading''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t8, software_source_status__property_rights_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(soft_tr_t16, software_source_status__property_rights_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(soft_tr_t24, software_source_status__property_rights_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(soft_tr_t32, software_source_status__property_rights_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(soft_tr_t40, software_source_status__property_rights_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(soft_be_t8, software_source_status__property_rights_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(soft_be_t16, software_source_status__property_rights_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(soft_be_t24, software_source_status__property_rights_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(soft_be_t32, software_source_status__property_rights_reading, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(soft_be_t40, software_source_status__property_rights_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(soft_su_t8, software_source_status__property_rights_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(soft_su_t16, software_source_status__property_rights_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(soft_su_t24, software_source_status__property_rights_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(soft_su_t32, software_source_status__property_rights_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(soft_su_t40, software_source_status__property_rights_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__property_rights_reading, 0.15).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language concept 'software is/is not intellectual property.' Per the ε-invariance principle, each reading of the software_source_status kernel is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification, since the readings produce structurally different (not merely differently-evaluated) claims. This reading (property_rights) computes as tangled_rope: genuine coordination (funding investment) plus asymmetric extraction (perpetual restriction beyond recoupment) requiring active enforcement. The freedom_imperative_reading is expected to compute closer to snare from the perspective this reading's own vendor seat would reject (restriction as pure injustice with no coordination story credited). The pragmatic_development_reading is expected to compute closer to rope (open methodology as voluntary superior coordination with minimal suppression). The utilitarian_hybrid_reading is expected to compute as a context-dependent hybrid, potentially scaffold-like, allocating each model to the domain where its welfare case is strongest. All four share the same underlying object (software source code control) but are structurally distinct constraints under DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
