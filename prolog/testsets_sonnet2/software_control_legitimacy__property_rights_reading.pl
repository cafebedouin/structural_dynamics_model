% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   domain: technology/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the property-rights reading of the software
 *   control legitimacy kernel: the position that a creator's authority to
 *   restrict use, modification, and distribution of software is a legitimate
 *   extension of ordinary property rights, justified by the need to protect
 *   investment and sustain commercial development. Under this reading,
 *   proprietary software vendors and their investors are the beneficiaries of
 *   enforceable exclusivity; FOSS advocates, independent modifiers, and
 *   interoperability-seeking developers who cannot capture value without
 *   licensing revenue enter the victim set because the same restrictions that
 *   protect vendor investment deny them the ability to build on, repair, or
 *   compete with the controlled code. Extraction is moderate (not extreme)
 *   because the coordination function — enabling investment in
 *   costly-to-produce, cheap-to-copy goods — is real and non-trivial, but is
 *   authored alongside the acknowledgment that alternative funding models
 *   exist and are actively suppressed as competitors by the same enforcement
 *   machinery, not merely bypassed by choice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.48).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.55).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Property-Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "technology/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'b285cd40-31b5-42dc-9424-df8b7152c6b1').
narrative_ontology:cs_kernel_codification('b285cd40-31b5-42dc-9424-df8b7152c6b1', formalized).
narrative_ontology:cs_authority_grounding('b285cd40-31b5-42dc-9424-df8b7152c6b1', extraction).
narrative_ontology:cs_interpretation_layer_present('b285cd40-31b5-42dc-9424-df8b7152c6b1').
narrative_ontology:cs_reading_relation('b285cd40-31b5-42dc-9424-df8b7152c6b1', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('b285cd40-31b5-42dc-9424-df8b7152c6b1', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('b285cd40-31b5-42dc-9424-df8b7152c6b1', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('b285cd40-31b5-42dc-9424-df8b7152c6b1', foundational, creator_investment_grounds_exclusion_right).
narrative_ontology:cs_axiom_status(creator_investment_grounds_exclusion_right, holdable).
narrative_ontology:cs_axiom_grounding('b285cd40-31b5-42dc-9424-df8b7152c6b1', creator_investment_grounds_exclusion_right, instrumental).
narrative_ontology:cs_axiom('b285cd40-31b5-42dc-9424-df8b7152c6b1', secondary, software_is_property_analogous_to_physical_goods).
narrative_ontology:cs_axiom_status(software_is_property_analogous_to_physical_goods, holdable).
narrative_ontology:cs_axiom_grounding('b285cd40-31b5-42dc-9424-df8b7152c6b1', software_is_property_analogous_to_physical_goods, conventional).
narrative_ontology:cs_reference_frame('b285cd40-31b5-42dc-9424-df8b7152c6b1', copyright_and_patent_property_analogy).
narrative_ontology:cs_drift_state('b285cd40-31b5-42dc-9424-df8b7152c6b1', post_open_source_movement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b285cd40-31b5-42dc-9424-df8b7152c6b1', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, venture_investors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, enterprise_licensing_customers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, independent_modifiers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, interoperability_seeking_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write license terms restricting copying, modification, and redistribution; enforce them through copyright litigation, DRM, and EULA terms. Capture the licensing and subscription revenue this authority generates, and frame the restriction as the necessary condition for continued investment in the product.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, proprietary_software_vendors, beneficiary).

% Fund software development on the expectation that exclusivity over the resulting code enables a return; the enforceability of control rights is a precondition for their capital allocation decision. They do not administer enforcement directly but their return depends entirely on it holding.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, venture_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Pay for licensed, supported, warrantied software and benefit from vendor accountability, security patching, and liability structures that a purely open, unowned codebase would not guarantee. Their benefit is real but conditioned on continuing to pay; switching away from an entrenched vendor stack is costly.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, enterprise_licensing_customers, beneficiary,
    powerful, biographical, constrained, national).

% Write and want to redistribute or fork software freely; blocked by copyright, patent, and license enforcement from doing so with proprietary codebases they may have contributed effort or ideas toward. They bear the cost of restricted collaboration and denied return on volunteer or public investment, and cannot exit the restriction except by refusing to interact with proprietary code at all, which forecloses large parts of the software ecosystem to them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    moderate, generational, constrained, global).

% Individual users and small developers who want to repair, adapt, or extend software they've purchased or that runs their hardware. License terms and technical enforcement (signed binaries, anti-tamper) block modification regardless of ownership of the underlying device. They have no practical alternative once locked into a vendor's ecosystem.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, independent_modifiers, payer,
    powerless, biographical, trapped, national).

% Want to build compatible tools, plugins, or competing products that interact with proprietary systems. Restricted APIs, reverse-engineering prohibitions, and licensing terms raise their costs or bar their access entirely, protecting vendor market position under the property-rights framing.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, interoperability_seeking_developers, payer,
    moderate, biographical, constrained, global).

% Adjudicate the boundaries of copyright, patent, and license enforceability in software; issue rulings on fair use, interoperability exceptions, and DMCA anti-circumvention scope that determine how far the property claim actually extends in practice.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, legal_scholars_and_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enforceable control rights let a firm capture the value of software it invests in building, which coordinates capital toward software development that would otherwise be undersupplied if anyone could copy and redistribute a finished product freely without cost.
% TRANSFER_FUNCTION: Moves the economic value created by code — and by unpaid contributions, ideas, and collaborative practices that occur outside the ownership boundary — from those who cannot capture licensing revenue (users, would-be modifiers, competing developers) to the vendor and its investors, via the enforcement of exclusivity.
% ABSENT_VOICES: FOSS communities and public-interest technologists rarely have a seat in license drafting or DMCA rulemaking; their objection — that treating software as ordinary property ignores its non-rivalrous, cumulative, and collaborative nature — is litigated reactively (in court, in exemption petitions) rather than built into the initial framing.
% DISAPPEARANCE_RATIONALE: If enforceable software property rights vanished overnight, the venture-funded proprietary software business model would collapse for products whose value depends on excludability; enterprise licensing revenue would disintegrate; but a large parallel software ecosystem (open source, service-based, hardware-bundled) already demonstrates the world does not require this constraint to produce software — it requires it to produce THIS FINANCING MODEL for software.
% FOUNDING_PROBLEM: Software is costly to produce and trivially cheap to copy; without some mechanism to prevent free-riding on the creator's investment, the argument goes, no one would fund substantial software development because competitors could copy and undercut immediately.
% FOUNDING_PROBLEM_CORROBORATION: Software vendors and their investors attest the problem remains live and central. Economists studying open-source and public-goods software funding, and the FOSS movement itself, attest that alternative funding models (service revenue, support contracts, public funding, cooperative development) demonstrably solve the underinvestment problem without exclusivity — this corroboration comes from outside the beneficiary set and is actively contested by vendors.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.48, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.48) and suppression (0.55) sit at moderate levels because enforcement (copyright litigation, DRM, anti-circumvention law) is real and growing (suppression_requirement rises from 0.35 to 0.55 over the interval as DMCA-style anti-circumvention regimes and technical enforcement like signed firmware have hardened), but the underlying coordination function — funding investment in software production — is genuine, not pure pretext. Theater ratio is low (0.2) because the enforcement machinery mostly does what it claims to do (protect revenue capture), rather than performing unrelated functions. Accessibility collapse (0.5) and resistance (0.55) reflect that meaningful alternatives (open source, service-based models) persist and are actively championed, but enforcement steadily narrows the practical space for interoperability and modification.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and investors sit near the full-beneficiary end: they set enforcement terms and capture the resulting revenue with strong exit options (arbitrage — they can restructure licensing or relocate operations). FOSS advocates and independent modifiers sit near the full-target end: they bear the cost of denied redistribution and modification rights with constrained or trapped exit, since leaving the proprietary ecosystem means forgoing large parts of modern computing infrastructure. Enterprise customers are a genuine intermediate case — they benefit from vendor accountability and support, but their exit is constrained by switching costs, so treat them as beneficiaries with qualified capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (underinvestment absent excludability) has empirical corroboration on both sides — the property-rights reading treats it as still live, while independent economic analysis of the FOSS ecosystem's productivity treats it as substantially solved by alternative models. Classifying this as tangled_rope rather than snare or rope prevents two mislabelings: treating vendor exclusivity as pure extraction with no coordination value (ignoring that investment-funded software genuinely gets built under this model) and treating it as pure coordination with no victims (ignoring that FOSS contributors and modifiers bear real, structurally enforced costs). The tangled_rope classification holds both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_analogy_validity,
    'Is software''s non-rivalrous, cumulative, collaboratively-built nature sufficiently analogous to physical property that ordinary property-rights reasoning (protecting investment via exclusion) transfers validly, or does the analogy break down in ways that make the property framing itself the contested move?',
    'Comparative analysis of software''s economic properties (non-rivalry, low marginal reproduction cost, network effects, cumulative technical dependency) against the standard justifications for physical property rights (scarcity, rivalry, exclusion enabling efficient allocation); track whether legal doctrine on software IP diverges structurally from real-property doctrine over time.',
    'If the analogy holds, the property-rights reading''s coordination claim strengthens and the tangled_rope classification''s coordination component is well-grounded. If it breaks down, the reading''s core premise is a naturalized metaphor rather than a structural fact, and effective extraction should be read higher than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_analogy_validity, conceptual, 'Whether treating software as ordinary property is a valid analogy or a contested framing choice.').

omega_variable(
    alternative_funding_sufficiency,
    'Do alternative funding models (service contracts, public funding, cooperative/foundation-backed development, dual licensing) sufficiently solve the underinvestment problem the property-rights reading identifies, making exclusivity enforcement unnecessary rather than merely one option among several?',
    'Empirical comparison of investment levels, software quality, and maintenance sustainability across proprietary-funded and open-source/service-funded software sectors over multi-year windows, controlling for domain.',
    'If alternative models prove sufficient at comparable investment levels, the founding problem is functionally dead and the property-rights reading''s coordination justification weakens substantially, pushing this constraint''s classification toward snare from the FOSS-advocate seat. If insufficient, the tangled_rope reading''s coordination component is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_sufficiency, empirical, 'Whether alternative funding models make exclusivity-based investment protection unnecessary.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which reading of the software_control_legitimacy kernel best describes the actual dominant institutional practice — is the property-rights reading the operative framing in courts and legislatures, or has practice already drifted toward the pragmatic_openness_reading (treating both models as legitimate alternatives) while property-rights rhetoric persists as justification?',
    'Track legislative and judicial language in software IP cases and DMCA rulemaking proceedings over time for shifts in framing language between absolute-property claims and pragmatic-coexistence claims.',
    'If institutional practice has drifted toward pragmatic openness while property-rights framing persists rhetorically, this reading''s stakeholders and metrics describe a framing that is losing operative force even where it remains legally codified — a drift the story''s temporal measurements would need to track separately.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the property-rights reading remains the operative institutional framing or persists mainly as justificatory rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__property_rights_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__property_rights_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__property_rights_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(soft_tr_t32, software_control_legitimacy__property_rights_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__property_rights_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__property_rights_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__property_rights_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__property_rights_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(soft_be_t32, software_control_legitimacy__property_rights_reading, base_extractiveness, 32, 0.46).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__property_rights_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__property_rights_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__property_rights_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(soft_su_t32, software_control_legitimacy__property_rights_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This story is one of four members of the software_control_legitimacy kernel family. Each sibling reading (freedom_imperative, pragmatic_openness, commons) authors a distinct ε, beneficiary/victim structure, and classification over the SAME standing arrangement (enforceable proprietary software control) rather than sharing one averaged value — per the ε-invariance principle, differing readings of a contested kernel are structurally distinct constraints, not one constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
