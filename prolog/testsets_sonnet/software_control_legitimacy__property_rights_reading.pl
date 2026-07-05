% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Proprietary Software Control as Legitimate Property Right
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the property-rights reading of the software
 *   control legitimacy kernel: software creators hold legitimate authority to
 *   restrict use, modification, and distribution, grounded in the need to
 *   protect investment and sustain commercial development. Under this
 *   reading, vendors and their investors are structural beneficiaries and
 *   FOSS advocates, downstream modifiers, and interoperability-dependent
 *   developers enter the victim set — they bear the cost of exclusion in
 *   exchange for the industry's claimed sustainability benefit. This is a
 *   distinct constraint from the freedom_imperative_reading (which treats the
 *   same restriction as ethically illegitimate), the
 *   pragmatic_openness_reading (which treats it as a neutral methodology
 *   choice with no rights claim at all), and the commons_reading (which
 *   reframes the entire question as collective governance rather than
 *   individual property or individual freedom). Each of those is a separate
 *   story with its own beneficiary/victim structure and its own epsilon; this
 *   file does not average or hedge across them.
 *
 * KEY AGENTS:
 *   - software_vendors: agenda_setter/beneficiary (institutional/arbitrage) — sets and enforces license terms, captures recurring revenue
 *   - venture_investors: beneficiary (institutional/mobile) — funds development on the exclusivity thesis
 *   - foss_advocates: payer (organized/constrained) — denied return on shared derivative work, builds parallel ecosystems
 *   - downstream_modifiers: payer (powerless/trapped) — cannot repair or modify owned devices
 *   - courts_and_ip_regulators: observer (institutional/analytical) — adjudicates the boundary over time
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
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Proprietary Software Control as Legitimate Property Right").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '89c6e44d-10e1-402b-bfaf-6e33f949caa3').
narrative_ontology:cs_kernel_codification('89c6e44d-10e1-402b-bfaf-6e33f949caa3', distributed).
narrative_ontology:cs_authority_grounding('89c6e44d-10e1-402b-bfaf-6e33f949caa3', distributed).
narrative_ontology:cs_reading_relation('89c6e44d-10e1-402b-bfaf-6e33f949caa3', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('89c6e44d-10e1-402b-bfaf-6e33f949caa3', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('89c6e44d-10e1-402b-bfaf-6e33f949caa3', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('89c6e44d-10e1-402b-bfaf-6e33f949caa3', foundational, creator_investment_grounds_exclusivity_right).
narrative_ontology:cs_axiom_status(creator_investment_grounds_exclusivity_right, holdable).
narrative_ontology:cs_axiom_grounding('89c6e44d-10e1-402b-bfaf-6e33f949caa3', creator_investment_grounds_exclusivity_right, instrumental).
narrative_ontology:cs_axiom('89c6e44d-10e1-402b-bfaf-6e33f949caa3', secondary, user_computing_autonomy_is_not_a_trumping_claim).
narrative_ontology:cs_axiom_status(user_computing_autonomy_is_not_a_trumping_claim, holdable).
narrative_ontology:cs_axiom_grounding('89c6e44d-10e1-402b-bfaf-6e33f949caa3', user_computing_autonomy_is_not_a_trumping_claim, deontological).
narrative_ontology:cs_reference_frame('89c6e44d-10e1-402b-bfaf-6e33f949caa3', exclusive_authorship_control_default).
narrative_ontology:cs_drift_state('89c6e44d-10e1-402b-bfaf-6e33f949caa3', post_open_source_movement_maturity, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('89c6e44d-10e1-402b-bfaf-6e33f949caa3', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, venture_investors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, proprietary_platform_operators).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, downstream_modifiers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, interoperability_dependent_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, end_users).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write license terms restricting copying, modification, and redistribution; enforce them through EULAs, DRM, and litigation. Capture recurring license revenue and control over the software's evolution. Can relicense, sue infringers, or acquire competitors — their exit options are the broadest in the arrangement.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, software_vendors, beneficiary).

% Fund software development on the expectation that exclusivity rights will produce a return; the property claim is what makes the investment thesis work. They can diversify or exit a given company but depend on the legitimacy of the control mechanism across the portfolio.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, venture_investors, beneficiary,
    institutional, biographical, mobile, global).

% Operate platforms (app stores, OS ecosystems, enterprise suites) whose value depends on enforceable exclusivity; they lobby for stronger IP enforcement and litigate against reverse-engineering or unauthorized interoperability.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, proprietary_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, proprietary_platform_operators, agenda_setter).

% Build and maintain alternatives under permissive or copyleft licenses, arguing the restriction denies users control over their own computing and denies developers the ability to build on prior work. They cannot force proprietary vendors to open source; their exit is limited to building parallel, often under-resourced, ecosystems that must interoperate with a proprietary world that resists them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, civilizational, constrained, global).

% Individuals and small firms who need to modify, repair, or extend software they depend on (e.g., embedded devices, agricultural equipment firmware) but are legally and technically barred from doing so. They bear real costs — inability to repair, vendor lock-in, forced repurchase — with essentially no leverage against license terms.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, downstream_modifiers, payer,
    powerless, biographical, trapped, national).

% Build products that must interface with dominant proprietary systems; access to APIs, formats, and protocols is gated by the vendor's control rights. They can seek official partnerships (costly, discretionary) or reverse-engineer (legally risky) but cannot bypass the control claim itself.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, interoperability_dependent_developers, payer,
    moderate, biographical, constrained, global).

% Benefit from a sustained commercial software industry funding continued development, support, and security patching, while also bearing restrictions on repair, resale, and modification of the software they've purchased or licensed.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, end_users, payer).

% Adjudicate the boundaries of the property claim — fair use, reverse engineering exemptions, right-to-repair statutes. Their rulings shift the balance between vendor control and downstream access over time.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, courts_and_ip_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables firms to internalize the return on software development investment, coordinating capital toward sustained engineering, support, and security maintenance that would be underprovided if output could be freely copied without compensation.
% TRANSFER_FUNCTION: Moves control over modification, redistribution, and interoperability from users and downstream developers to the rights-holding vendor, and moves recurring payment from users/licensees to vendors and their investors in exchange for continued access and support.
% ABSENT_VOICES: FOSS advocates and downstream modifiers are represented in litigation and policy debates but rarely inside the vendor's own licensing decisions; embedded-device owners seeking right-to-repair are structurally outside the room where license terms are drafted.
% DISAPPEARANCE_RATIONALE: If enforceable software property rights vanished overnight, the venture-funded commercial software industry's investment thesis would collapse for a large share of current business models; vendors would need to pivot to service, subscription-support, or open-core models. Some FOSS ecosystems would expand rapidly; others depending on proprietary interop would be disrupted. The change would be substantial, not cosmetic.
% FOUNDING_PROBLEM: Software is trivially copyable at near-zero marginal cost; without some enforceable exclusivity, firms that invest heavily in original development could be immediately undercut by copiers who bore none of the development cost, undermining the incentive to invest in the first place.
% FOUNDING_PROBLEM_CORROBORATION: Independent economists studying public-goods provision and free-rider dynamics corroborate that non-excludable digital goods face underinvestment absent some exclusivity mechanism — a finding that predates and sits outside the software industry's own advocacy. However, FOSS-adjacent economists and legal scholars (e.g., studies of the Linux/Apache ecosystems) corroborate that peer-production and service-revenue models solve the same underinvestment problem without exclusivity, meaning the founding problem's necessity claim is actively disputed by parties outside both the vendor and FOSS camps.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.48 at interval end) because the restriction genuinely correlates with a real investment-protection function for a meaningful share of the commercial software industry — this is not a pure rent, but it is also not costless to those it excludes. Suppression (0.55) reflects the active enforcement machinery (DRM, EULAs, litigation, DMCA-style anti-circumvention law) required to keep the exclusivity durable against a technology that is trivially copyable absent enforcement. Theater ratio stays low (0.2) because enforcement substantially tracks the claimed function rather than performing it — most licensing enforcement actually protects real revenue streams, though the modest upward drift signals growing use of IP enforcement against right-to-repair and interoperability efforts that serve vendor lock-in more than investment protection.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors and their investors sit at the beneficiary end: they set the terms, capture the transfer, and have the widest exit options (relicense, acquire, diversify). FOSS advocates and downstream modifiers sit at the target end: they bear the restriction's cost (denied modification rights, denied ability to build derivative works, denied repair) with little to no capacity to alter the terms. Interoperability-dependent developers occupy a middle position — moderate power, constrained exit — because they can sometimes negotiate official access but remain structurally dependent on vendor discretion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (free-rider risk on a non-excludable, zero-marginal-cost good) is genuinely live for a segment of the industry — first-party engine or platform vendors funding continuous R&D — but is contested as a universal justification, since large, healthy ecosystems (Linux kernel, Apache Foundation projects, PostgreSQL) demonstrate viable commercial sustainability without the exclusivity claim. Classifying this as tangled_rope rather than snare or rope prevents two mislabelings: treating all software property claims as pure extraction (ignoring the real coordination function for capital-intensive proprietary R&D) and treating all software property claims as costless coordination (ignoring the well-documented harms to repair rights, interoperability, and derivative innovation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the property-rights framing of software control the correct lens for a given case, or does it obscure that a freedom-based, methodology-neutral, or commons-based framing better describes the same underlying dispute?',
    'This ambiguity is not resolved within a single story — it is routed to the sibling readings (freedom_imperative_reading, pragmatic_openness_reading, commons_reading) as separate constraint files linked via network.affects_constraints. Each reading is evaluated on its own structural merits; no single epsilon spans all four.',
    'Selecting the property-rights reading fixes FOSS advocates and downstream modifiers as victims and vendors/investors as beneficiaries; under the freedom_imperative_reading the same facts would instead classify proprietary vendors as victimizers with no legitimate claim at all, producing a much higher extraction reading for the same base conduct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading applies is itself contested and not resolvable by data internal to this story.').

omega_variable(
    investment_protection_necessity,
    'Is enforceable exclusivity actually necessary to sustain commercial software investment, or do service/support/open-core revenue models demonstrate the founding problem is largely solved by non-exclusive means?',
    'Comparative economic analysis of R&D investment levels and sustainability across proprietary-exclusive firms versus open-core/service-revenue firms of comparable scale and complexity over multi-decade horizons.',
    'If non-exclusive models are shown to sustain comparable investment at scale, the coordination justification for the property claim weakens substantially and the classification would drift toward snare; if exclusivity is shown necessary for high-capital-intensity segments (e.g., specialized enterprise or safety-critical software), the tangled_rope reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_protection_necessity, empirical, 'Whether exclusivity is empirically necessary for the claimed coordination function.').

omega_variable(
    right_to_repair_carveout_scope,
    'Should the property-rights reading exempt repair and interoperability from the exclusivity claim without abandoning the core investment-protection rationale?',
    'Track legislative and judicial right-to-repair and interoperability carveouts (e.g., DMCA exemptions, EU interoperability directives) over the interval and observe whether extraction and suppression metrics decline as carveouts expand.',
    'A durable, well-scoped carveout would reduce victim-side harm to downstream_modifiers and interoperability_dependent_developers without eliminating the coordination function for core commercial development, potentially shifting the classification toward scaffold-like transitional accommodation within the property frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_to_repair_carveout_scope, empirical, 'Whether carveouts can decouple repair/interop harms from the core exclusivity claim.').


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
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__property_rights_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__property_rights_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(soft_be_t32, software_control_legitimacy__property_rights_reading, base_extractiveness, 32, 0.46).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__property_rights_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__property_rights_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__property_rights_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(soft_su_t32, software_control_legitimacy__property_rights_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This story is one of four siblings decomposing the natural-language concept 'software control legitimacy' per the epsilon-invariance principle. The property_rights_reading claims moderate, contested extraction grounded in investment-protection coordination with identifiable victims (tangled_rope). The freedom_imperative_reading treats the identical restriction as categorically illegitimate (expected much higher extraction, likely snare-leaning). The pragmatic_openness_reading strips out the rights claim entirely and treats the choice as a neutral methodology question (expected near-rope, minimal victim structure). The commons_reading reframes the underlying resource as collectively governed rather than individually owned or individually free (expected distinct beneficiary/victim topology centered on collective-action dynamics). Each has its own epsilon; none should be averaged with another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
