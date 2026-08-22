% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right (Property-Rights Reading)
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   A commercial-software regime rests on the claim that control over use,
 *   modification, and distribution is a creator's property right: vendors
 *   write standard-form licenses, deploy technical protection measures, audit
 *   large customers, and litigate or lobby to hold the perimeter, on the
 *   justification that securable returns fund the engineering. This file is
 *   ONE READING of the contested kernel software_control_legitimacy — the
 *   property_rights_reading — and decomposes per the epsilon-invariance
 *   principle: the sibling readings (freedom_imperative_reading,
 *   pragmatic_openness_reading, commons_reading) are separate constraint
 *   files over the same standing arrangement, each with its own epsilon,
 *   victim set, and classification. This reading authors moderate epsilon: it
 *   credits the funding function the arrangement performs while registering
 *   the lock-in rents and enforcement excess layered on top of it. KEY AGENTS
 *   (by structural relationship): - proprietary_software_vendors:
 *   Agenda-setter (institutional/arbitrage) — writes and enforces the terms,
 *   collects the revenue - software_investors: Primary beneficiary
 *   (powerful/arbitrage) — collects returns without bearing any restriction -
 *   end_users: Payer with secondary beneficiary position
 *   (moderate/constrained) — pays and is restricted, receives maintained
 *   product - corporate_licensees: Payer with secondary beneficiary position
 *   (powerful/trapped) — deep integration, audit exposure, non-negotiable
 *   dependency - foss_developers: Payer (moderate/constrained) — denied
 *   inputs, legal pressure against interoperability work -
 *   independent_software_competitors: Payer (moderate/constrained) — blocked
 *   from dominant platforms - digital_rights_advocates: Excluded
 *   (organized/constrained) — no seat where license terms are drafted -
 *   competition_and_legislative_regulators: Analytical observer
 *   (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.6).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.65).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Property-Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '6a2a374c-1e15-4860-b563-fa3df29e3415').
narrative_ontology:cs_kernel_codification('6a2a374c-1e15-4860-b563-fa3df29e3415', formalized).
narrative_ontology:cs_authority_grounding('6a2a374c-1e15-4860-b563-fa3df29e3415', lineage).
narrative_ontology:cs_interpretation_layer_present('6a2a374c-1e15-4860-b563-fa3df29e3415').
narrative_ontology:cs_reading_relation('6a2a374c-1e15-4860-b563-fa3df29e3415', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a2a374c-1e15-4860-b563-fa3df29e3415', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a2a374c-1e15-4860-b563-fa3df29e3415', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('6a2a374c-1e15-4860-b563-fa3df29e3415', foundational, creation_confers_control_entitlement).
narrative_ontology:cs_axiom_status(creation_confers_control_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('6a2a374c-1e15-4860-b563-fa3df29e3415', creation_confers_control_entitlement, deontological).
narrative_ontology:cs_axiom('6a2a374c-1e15-4860-b563-fa3df29e3415', secondary, exclusion_required_for_investment_recovery).
narrative_ontology:cs_axiom_status(exclusion_required_for_investment_recovery, holdable).
narrative_ontology:cs_axiom_grounding('6a2a374c-1e15-4860-b563-fa3df29e3415', exclusion_required_for_investment_recovery, instrumental).
narrative_ontology:cs_reference_frame('6a2a374c-1e15-4860-b563-fa3df29e3415', creator_exclusive_control_baseline).
narrative_ontology:cs_drift_state('6a2a374c-1e15-4860-b563-fa3df29e3415', contemporary_open_source_cloud_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6a2a374c-1e15-4860-b563-fa3df29e3415', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, independent_software_competitors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, end_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, corporate_licensees).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, corporate_licensees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the license terms, end-user agreements, and technical protection measures that govern how their products may be used, modified, and redistributed. Operate activation servers, telemetry, and audit programs to detect terms violations, and litigate or lobby when detection fails. Collect license fees, subscriptions, and support contracts as revenue. Can reincorporate in favorable jurisdictions, pivot to service models, or acquire rivals; the terms they write bind others, not themselves.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Supply capital to software firms on the expectation that exclusive control over products will yield returns through sales, subscriptions, or acquisition. Bear none of the usage restrictions personally and can move capital between sectors or geographies at low cost if returns disappoint.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Pay for licenses or subscriptions and accept standard-form terms they never negotiated. May not inspect, modify, repair, or redistribute the software they run, and may face activation checks or feature limits for noncompliance. In exchange they receive maintained, compatible, supported products. Switching to a different ecosystem means relearning tools, migrating data, and losing familiarity — possible but costly.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, end_users, beneficiary).

% Run their operations on vendor-controlled systems — operating systems, databases, enterprise applications — under enterprise agreements with audit clauses and escalating renewal pricing. Decades of integration make migration prohibitively expensive even when fees rise faster than delivered value; they receive support, certification, and compliance coverage in return. Procurement teams negotiate hard at the margin, but the underlying dependency is not negotiable.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, corporate_licensees, payer,
    powerful, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, corporate_licensees, beneficiary).

% Produce software collaboratively under licenses that grant everyone the freedoms the proprietary terms withhold. Are barred from reading or building on proprietary codebases, face patent assertions and anti-circumvention claims aimed at interoperability work, and spend effort on license compatibility rather than code. A parallel ecosystem shields much of their work, but the legal environment they operate in is written elsewhere. Paid proprietary employment remains available and many take it; declining it is a commitment rather than an impossibility.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_developers, payer,
    moderate, generational, constrained, global).

% Build products adjacent to dominant proprietary platforms. Cannot interoperate without permission, face interface and API restrictions, marketplace gatekeeping, and patent exposure, and must either reverse-engineer at legal risk, license on offered terms, or construct parallel stacks at multiplied cost.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, independent_software_competitors, payer,
    moderate, biographical, constrained, global).

% Organizations and coalitions that challenge standard-form software terms through litigation, legislative testimony, and public campaigns — defending security research, repair, preservation, and interoperability. They hold no seat where license terms are drafted or renewed; the users whose terms they contest are likewise absent from those negotiations.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, digital_rights_advocates, excluded,
    organized, generational, constrained, global).

% Agencies and legislatures reviewing merger, interoperability, right-to-repair, and anti-circumvention questions. Take evidence from the other parties, commission studies, and can reshape the legal environment through remedies, exemptions, and statutes, though jurisdiction stops at borders the vendors arbitrage across.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, competition_and_legislative_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures appropriable returns on software creation: because copies are costless to duplicate, exclusion rights over use, modification, and distribution let firms charge for software, recover development cost, and plan multi-year engineering investments. Support, warranty, and maintenance offerings attach to the same exclusivity.
% TRANSFER_FUNCTION: Moves money from end users, enterprises, and licensees to vendors and their investors as license fees, subscriptions, and audit settlements; moves control — the rights to use, modify, and distribute — from everyone who touches the software to the vendor holding title.
% ABSENT_VOICES: No end user negotiates a standard-form license; the terms are offered take-it-or-leave-it at install time. Future developers who might have built on the code, and the public interest in a reusable body of software, have no representative at terms-drafting. Digital-rights advocates speak in courtrooms and legislatures but hold no seat where license text is written.
% DISAPPEARANCE_RATIONALE: If exclusion rights over software vanished overnight, upfront license revenue would collapse before service-and-support models could absorb displaced firms; prices, employment, and investment would swing widely while the industry reorganized around services, dual licensing, and funded commons production. Software would still be written — the volunteer and foundation sectors prove that — but the commercial half of the ecosystem would rebuild itself from scratch.
% FOUNDING_PROBLEM: Standalone software had to become sellable once hardware bundling ended: programs are expensive to create and cost nothing to copy, so without some way to appropriate value from copies, commercial producers could not recover development cost. Exclusive control over use, modification, and distribution was the answer built.
% FOUNDING_PROBLEM_CORROBORATION: Innovation economists outside the vendor set corroborate that the appropriability problem is real — cheaply copied goods are systematically underproduced without some exclusion mechanism. FOSS-economics research corroborates that partial non-market and service-funded alternatives exist at scale, while not disputing that the funding problem itself persists. What no party outside the benefiting set corroborates is that exclusive control is the unique or proportionate solution; corroboration attaches to the problem, not to the property answer.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.60): genuine value — working, maintained, supported software — flows back to payers, but license and subscription prices sit far above marginal reproduction cost, and lock-in lets vendors raise renewal pricing against captive installed bases. Suppression (0.65) reflects the enforcement stack: anti-circumvention law, activation and attestation, license audits, and litigation against interoperability work; architectural enforcement inside service delivery now does much of what lawsuits once did. Theater (0.30): security, safety, and innovation rationales are partly real but increasingly dress platform rent. Accessibility_collapse (0.50): free alternatives exist and are excellent in many categories, yet compatibility gravity and organizational inertia keep most payers in place. Resistance (0.60): an organized free-software movement, right-to-repair campaigns, security-research litigation, and regulatory attention meet the terms continuously. All three temporal series share one seven-point grid (1980-2026); endpoint values equal the base_properties scalars. Claim and metrics are authored independently: the tangled_rope claim states the structure I believe true — real funding coordination carrying asymmetric extraction — while the metrics describe operation as observed.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the vendor seat the arrangement is the funding machine that built the software industry — coordination it constructed and maintains, with the restrictions as the price of admission. From the end-user seat the same terms are take-it-or-leave-it limits on products they thought they owned. Corporate licensees compute something harsher than consumers do: trapped exit converts rising fees into uncompensated burden even while they consume real support value. FOSS developers experience the regime primarily as a wall around inputs and a legal threat against their method. The engine computes these per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors sit nearest the beneficiary pole: they write the terms, collect the revenue, and hold arbitrage-grade exit (reincorporation, model pivots, acquisitions). Investors sit at the pole itself — pure collection, frictionless exit. End users occupy a moderated middle-high position: they pay and are restricted, but receive maintained product, so their dual declaration tempers the target-side pull. Corporate licensees land nearer the full-target end than consumers because trapped exit removes the damping that choice provides. FOSS developers and independent competitors are targets by construction — the regime's benefit to vendors is partly constituted by what it denies them — and their constrained exits amplify effective extraction. Regulators are analytical seats with no directional stake. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — funding the creation of costlessly copyable goods — is still live, so this is not a mandate outliving its function; mandatrophy_resolved stays undeclared. The classification work runs the other direction: a pure-coordination reading would credit the funding function while ignoring the enforcement ratchet and lock-in rents layered on top of it; a pure-extraction reading would erase the demonstrable coordination achievement — the commercial software economy exists because returns were securable. Tangled rope holds both: the same structure that funds engineering transfers control and surplus asymmetrically. The temporal series show the extraction share growing (0.34 to 0.60) faster than the coordination function expanded, which is the accumulation signature the corpus watches for. The mismatch consumer should find status=live paired with verdict=world_rearranges — the consistent cell, no zombie flag expected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexing,
    'This story instantiates only the property_rights_reading of kernel software_control_legitimacy; which structural elements would change under the sibling readings?',
    'Cross-reading corpus comparison: author the sibling files (freedom_imperative_reading, pragmatic_openness_reading, commons_reading) over the same standing arrangement and diff beneficiary/victim sets, epsilon, and computed per-seat types.',
    'Under freedom_imperative_reading the same arrangement authors epsilon near ceiling with end_users as the primary victim set; under pragmatic_openness_reading epsilon falls toward the coordination-cost floor; under commons_reading the victim set redistributes toward enclosed commoners and the enforcement picture softens. Classification of this file must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexing, conceptual, 'Committer structure: one reading of a four-reading kernel; sibling deltas documented here rather than folded into this constraint.').

omega_variable(
    appropriability_necessity,
    'Does commercial-scale software investment actually require exclusion rights over use, modification, and distribution, or do alternative funding mechanisms (service contracts, dual licensing, foundation and public funding, patronage) sustain comparable output?',
    'Compare investment levels and output quality in segments where exclusion is unenforceable or voluntarily waived (foundation-hosted infrastructure, service-funded open cores) against proprietary segments, controlling for demand and category.',
    'If alternatives sustain comparable investment, the coordination justification narrows and a larger share of measured extraction is rent; if not, part of the measured extraction is the price of funding production and the tangled-rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriability_necessity, empirical, 'Whether the funding function genuinely depends on the exclusion rights, or rides on them opportunistically.').

omega_variable(
    lockin_separability,
    'Are switching costs and ecosystem lock-in separable from the legitimate protection function, or intrinsic to it?',
    'Interoperability mandates and data-portability regimes as natural experiments: if vendor returns persist while lock-in falls, the functions are separable.',
    'If separable, lock-in components count as extraction riding on a real coordination function; if intrinsic, effective extraction includes a genuine coordination-cost component and the payer-seat classifications soften.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lockin_separability, conceptual, 'Boundary between protection-of-investment and captive-customer construction.').

omega_variable(
    suppression_mechanism_shift,
    'Has the arrangement''s suppressive force migrated from legal machinery (anti-circumvention statutes, audits, litigation) to architectural control (activation, attestation, SaaS-only delivery), and does the scalar suppression capture the new mechanism?',
    'Track enforcement-mechanism composition over time: litigation and audit volume versus telemetry and attestation deployment; measure circumvention success rates and exemption uptake.',
    'Architectural suppression raises effective suppression for end-user seats while lowering visible legal conflict; misreading the shift understates suppression for end seats and overstates it for institutional ones, distorting per-seat divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_shift, empirical, 'Whether the rising suppression series reflects legal intensification, architectural substitution, or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_control_legitimacy__property_rights_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(soft_tr_t1990, software_control_legitimacy__property_rights_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(soft_tr_t1998, software_control_legitimacy__property_rights_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(soft_tr_t2008, software_control_legitimacy__property_rights_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(soft_tr_t2016, software_control_legitimacy__property_rights_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(soft_tr_t2020, software_control_legitimacy__property_rights_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(soft_tr_t2026, software_control_legitimacy__property_rights_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_control_legitimacy__property_rights_reading, base_extractiveness, 1980, 0.34).
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__property_rights_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(soft_be_t1998, software_control_legitimacy__property_rights_reading, base_extractiveness, 1998, 0.46).
narrative_ontology:measurement(soft_be_t2008, software_control_legitimacy__property_rights_reading, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement(soft_be_t2016, software_control_legitimacy__property_rights_reading, base_extractiveness, 2016, 0.56).
narrative_ontology:measurement(soft_be_t2020, software_control_legitimacy__property_rights_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(soft_be_t2026, software_control_legitimacy__property_rights_reading, base_extractiveness, 2026, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_control_legitimacy__property_rights_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__property_rights_reading, suppression_requirement, 1990, 0.33).
narrative_ontology:measurement(soft_su_t1998, software_control_legitimacy__property_rights_reading, suppression_requirement, 1998, 0.47).
narrative_ontology:measurement(soft_su_t2008, software_control_legitimacy__property_rights_reading, suppression_requirement, 2008, 0.54).
narrative_ontology:measurement(soft_su_t2016, software_control_legitimacy__property_rights_reading, suppression_requirement, 2016, 0.59).
narrative_ontology:measurement(soft_su_t2020, software_control_legitimacy__property_rights_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(soft_su_t2026, software_control_legitimacy__property_rights_reading, suppression_requirement, 2026, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, commons_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of kernel software_control_legitimacy: the colloquial debate 'who should control software' covers four structurally distinct claims, written as four files. The property_rights_reading is the legally instantiated baseline (copyright statute, license enforceability, anti-circumvention law) and therefore exerts upstream structural influence on the operating environment of the other three — the freedom and commons readings define themselves partly against it, and the pragmatic reading grants it conditional legitimacy. Each file carries its own epsilon over the shared referent arrangement; this file's 0.60 is the property-lens value, not a family average.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
