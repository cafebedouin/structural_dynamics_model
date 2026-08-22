% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: Narrow Linking Reading: Linking Is Aggregation, Only Modification Triggers GPL Obligations
 *   domain: software licensing / copyright law / open source governance
 *
 * SUMMARY:
 *   The GPL fixes its license text but not the boundary of what counts as a
 *   derivative work, and three readings of that boundary compete. This story
 *   instantiates the narrow permissive reading: linking is aggregation, not
 *   derivation, and only modifications to GPL code itself trigger
 *   obligations. Operationally, the reading functions as a wall protecting
 *   proprietary modules that sit on top of GPL components; users of those
 *   modules lose the source-availability guarantee the license's authors
 *   intended for combined works, and the steward's propagation goal goes
 *   unrealized wherever the reading governs. Per the epsilon-referent rule
 *   for kernel readings, the referent of the extractiveness score is the
 *   standing arrangement under contest (the ecosystem operating under this
 *   reading), assessed by this reading's own lights: from inside the narrow
 *   reading, most of what happens is ordinary copyright operation and
 *   legitimate composability, with one visible leak - value flowing from
 *   contributors who granted code under reciprocity expectations to vendors
 *   who return nothing. That yields a moderate base rate rather than the high
 *   rate a broad-reading author would assign to the same arrangement. The
 *   claim and the metrics are independent authored facts: the type is claimed
 *   from structure (a genuine bright-line coordination function plus
 *   asymmetric capture plus active defense), and the metrics are authored
 *   from the arrangement's observed operation.
 *
 * KEY AGENTS:
 *   - - proprietary_software_vendors: Primary beneficiary (powerful/arbitrage) - ships closed modules over GPL components, defends the reading, retains substitute-library exits
 *   - - hybrid_product_developers: Secondary beneficiary (moderate/constrained) - architectures assume the reading holds
 *   - - gpl_contributors: Primary target (moderate/constrained) - reciprocity expectations defeated, no practical individual recourse
 *   - - end_users_of_proprietary_modules: Secondary target (powerless/trapped) - lose source availability on deployed systems
 *   - - free_software_foundation: Contesting steward (institutional/identity_locked) - propagation goal frustrated, cannot exit its own position
 *   - - commercial_dual_license_vendors: Collateral payer (organized/mobile) - paid-license demand erodes under the reading
 *   - - national_courts: Agenda setter (institutional/analytical) - piecemeal adjudication, no apex settlement
 *   - - copyright_scholars: Analytical observer - maps the doctrinal terrain, collects and bears nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.55).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.42).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Narrow Linking Reading: Linking Is Aggregation, Only Modification Triggers GPL Obligations").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software licensing / copyright law / open source governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'd5690b3c-20e5-4881-9482-66e7af13a275').
narrative_ontology:cs_kernel_codification('d5690b3c-20e5-4881-9482-66e7af13a275', fixed_text).
narrative_ontology:cs_authority_grounding('d5690b3c-20e5-4881-9482-66e7af13a275', distributed).
narrative_ontology:cs_reading_relation('d5690b3c-20e5-4881-9482-66e7af13a275', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('d5690b3c-20e5-4881-9482-66e7af13a275', gpl_derivative_work_trigger__interface_boundary_reading, forecloses).
narrative_ontology:cs_axiom('d5690b3c-20e5-4881-9482-66e7af13a275', foundational, linking_is_aggregation_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_aggregation_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('d5690b3c-20e5-4881-9482-66e7af13a275', linking_is_aggregation_not_derivation, conventional).
narrative_ontology:cs_axiom('d5690b3c-20e5-4881-9482-66e7af13a275', secondary, copyleft_obligations_confined_to_modified_gpl_code).
narrative_ontology:cs_axiom_status(copyleft_obligations_confined_to_modified_gpl_code, holdable).
narrative_ontology:cs_axiom_grounding('d5690b3c-20e5-4881-9482-66e7af13a275', copyleft_obligations_confined_to_modified_gpl_code, conventional).
narrative_ontology:cs_reference_frame('d5690b3c-20e5-4881-9482-66e7af13a275', classical_copyright_expression_boundary).
narrative_ontology:cs_drift_state('d5690b3c-20e5-4881-9482-66e7af13a275', contemporary_copyleft_advocacy_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('d5690b3c-20e5-4881-9482-66e7af13a275', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, hybrid_product_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_contributors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_proprietary_modules).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_dual_license_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build commercial products that link against unmodified GPL libraries (compression, cryptography, device drivers, UI toolkits) and ship their own modules as closed binaries. Under this reading they owe no source disclosure for the combined product. They fund industry groups, file amicus briefs, and run compliance programs defending the reading when challenged, and they can switch to permissively licensed or commercially licensed substitute libraries if the legal environment turns against them.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, agenda_setter).

% Small and mid-size firms whose products combine GPL components with proprietary code. The reading lets them ship without source disclosure or license negotiation, and their architectures quietly assume it holds; a shift in the law would force rewrites, relicensing fees, or component removal on startup-scale budgets.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, hybrid_product_developers, beneficiary,
    moderate, biographical, constrained, global).

% Developers who publish code under the GPL expecting that downstream combined works will carry the same terms. Under this reading their code can be incorporated into closed products through linking with nothing returned. Grants already made cannot be retracted, individual litigation is prohibitively expensive, and recourse is limited to coordinating through foundations or relicensing only future versions.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_contributors, payer,
    moderate, biographical, constrained, global).

% Organizations and individuals running devices, appliances, and applications built on closed modules sitting atop GPL components. They receive binaries without the source availability the license's authors intended for combined works. Deployed systems are costly or impossible to replace, and they hold no seat in the licensing disputes that determine what they are entitled to.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_proprietary_modules, payer,
    powerless, immediate, trapped, global).

% Steward of the GPL. Publishes interpretive guidance asserting that linking brings obligations, funds and coordinates enforcement actions, and drafts license text aimed at closing gaps it perceives. Its institutional purpose is bound to the propagation goal prevailing; it cannot abandon that position without dissolving its reason for existing, yet it does not control the courts that decide the question.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, agenda_setter).

% Businesses offering the same code under both an open license and a paid commercial license. Their revenue model depends on customers believing that linking triggers obligations, so this reading erodes demand for their paid licenses. They respond by tightening their own license terms, adding attribution and termination clauses, or migrating flagship products to non-GPL licensing.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_dual_license_vendors, payer,
    organized, biographical, mobile, global).

% Decide infringement cases that turn on whether linked code forms one work. No apex court has settled the question for GPL linking; rulings arrive piecemeal across jurisdictions and mostly address modification rather than linkage, leaving the reading's fate to accumulated precedent rather than any single judgment.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, national_courts, agenda_setter,
    institutional, generational, analytical, national).

% Map the doctrinal terrain between aggregation and derivation, publish the analyses both camps cite, and testify in policy processes. They collect nothing and bear nothing from whichever reading wins.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyright_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, low-friction boundary rule for combining GPL and non-GPL code: developers know that linking against unmodified GPL libraries does not import copyleft obligations into their own code, which enables hybrid ecosystems, reduces legal diligence costs, and keeps software composition from chilling under doctrinal uncertainty.
% TRANSFER_FUNCTION: Moves the value of GPL library code (features, maintenance, stability, testing) into proprietary products without reciprocal source return, and moves legal certainty about combinability to vendors; correspondingly moves enforcement leverage away from license stewards and contributors.
% ABSENT_VOICES: End users of combined works have no seat in licensing disputes; individual GPL contributors who granted code under reciprocity expectations almost never litigate; downstream auditors and procurement officers who depend on source availability for security review are represented by no one in the interpretive contest. The Foundation speaks for propagation interests, but the diffuse user and contributor voices are structurally absent from the rooms where compliance norms get written.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight and the broad reading became settled law, thousands of shipping products would face sudden source-disclosure exposure, vendors would strip GPL dependencies or rush to buy commercial licenses, dual-licensing revenue models would surge, and embedded and enterprise stacks would reorganize around permissively licensed components within a few release cycles.
% FOUNDING_PROBLEM: Copyright's derivative-work category is radically indeterminate as applied to software linkage. In the GPL's early era this indeterminacy threatened to chill all software composition: no developer or vendor could tell whether connecting to a GPL library exposed their entire codebase. This reading resolves the uncertainty by drawing the line at modification of the GPL code itself.
% FOUNDING_PROBLEM_CORROBORATION: The indeterminacy is corroborated from outside the benefiting parties: no apex court in a major software-producing jurisdiction has settled the linking question, treatise-level copyright scholarship continues to describe the aggregation/derivation line as unsettled for software, and the license steward itself keeps publishing guidance precisely because the question remains open. The parties disputing the reading attest the underlying problem is live from opposite directions.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness ends at 0.55: the reading transfers real value (GPL library functionality embedded in closed products) from contributors and users to vendors, but from the reading's own lights much of the arrangement is unremarkable copyright operation, and the leak is partial rather than total. Suppression is 0.42 and is authored as a raw structural property, unscaled by power or scope: the reading persists not because participants prefer it but because keeping the broad reading from being judicially settled is an ongoing defensive project funded by the beneficiaries - litigation-cost asymmetry does the work that decrees do in other arrangements. Theater ratio is 0.30 and rising: vendor 'open source compliance programs' increasingly certify narrow-reading conformity while the certification itself is the product, a growing share of activity that performs cooperation without returning source. Accessibility collapse is low (0.30) because alternatives remain fully available - LGPL, AGPL, explicit linking exceptions, commercial licenses, and permissive-licensed substitutes all persist, and the rival readings remain live in court and scholarship. Resistance is moderately high (0.60): the steward runs sustained interpretive and enforcement campaigns, scholarship presses the other side, and several jurisdictions have produced rulings hostile to the reading's more aggressive applications. The measurement series share one time grid (t=0..30, roughly 1995-2025): extractiveness climbs as enterprise, mobile, and embedded adoption multiplied the value available to capture behind the wall; suppression requirement climbs as the stakes of leaving the question unsettled grew and the defensive apparatus professionalized; theater climbs as compliance programs proliferated. The trajectory is monotonic, not cyclical - no intermittent-reinforcement mechanism is present.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the vendor seat (beneficiary, arbitrage exit), the reading is the enabling condition of a hybrid software economy - a bright line that made composition lawful and insurable. From the contributor seat (constrained exit) and the end-user seat (trapped), the same line operates as a one-way valve: value crosses into closed products and nothing crosses back. From the steward's seat (identity_locked), the reading is an existential frustration of the license's constitutive purpose. The courts see an open question, not a settled rule. The engine derives these per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality for proprietary_software_vendors and hybrid_product_developers, pulled further toward the beneficiary end by the vendors' arbitrage-grade exit (substitute libraries, dual-license purchases). Victim declarations map to high directionality for gpl_contributors and end_users_of_proprietary_modules, pushed toward the full-target end by constrained and trapped exits respectively. The foundation's identity lock places it at the extreme target end despite its institutional power - power without exit reads as maximum exposure here. Commercial dual-license vendors are declared victims because the reading destroys their revenue lever, though their mobility moderates the damage. Courts and scholars sit at or near symmetric and observer positions; they are administered-by, not extracted-from. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - derivative-work indeterminacy chilling software composition - is still live, corroborated by the absence of apex-court settlement and by continued guidance publication from the steward itself, so no mandatrophy is declared and the status-times-verdict consumer finds no mismatch (live problem, world rearranges if the reading vanishes). The classification work this story does is preventing mislabeling in both directions. Calling the arrangement a pure coordination mechanism would hide the capture: the same bright line that enables composition also defeats reciprocity, and the beneficiaries actively fund the line's defense. Calling it pure extraction would erase the genuine coordination value that even its opponents implicitly concede - the steward's own creation of the Library GPL demonstrates that a permissive linking boundary is demanded in some form by nearly every participant, which is why the reading persists against well-funded opposition rather than collapsing. The tangled characterization holds both facts: real coordination function, asymmetric transfer through the same structure, active enforcement required to keep the rival reading from displacing it. Coalition dynamics are noted for the victim seats: contributors lack individual recourse but have begun coordinating through conservancies and foundation-backed enforcement, which is the principal channel through which the resistance metric could convert into settlement pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation_status,
    'This constraint is one reading of the gpl_derivative_work_trigger kernel - what structurally changes if a sibling reading (broad_copyleft_reading or interface_boundary_reading) displaces it?',
    'Comparative authoring of the sibling stories plus judicial settlement in a major software-producing jurisdiction. The disagreement is located in the criterion for derivative-work status: the act of linking (this reading), the fact of linking however implemented (broad reading), or the cleanliness of the API boundary (interface reading).',
    'Under the broad reading the wall becomes a disclosure gate and effective extraction concentrates on the vendor seats; under the interface-boundary reading protection becomes conditional and the beneficiary seat splits. Each displacement changes the victim set and the directionality profile of every named seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation_status, conceptual, 'Committer-frame omega recording that this story instantiates one reading of a contested kernel and naming the structural deltas the siblings would produce.').

omega_variable(
    judicial_settlement_trajectory,
    'Will any apex court settle the linking question for GPL purposes, and in which direction?',
    'Track infringement dockets and appellate outcomes across major jurisdictions; watch for a case with the facts (linkage without modification) and the standing (a motivated license steward or contributor consortium) to reach final appellate review.',
    'Settlement collapses the interpretive ambiguity that sustains the current arrangement. A broad ruling converts the arrangement rapidly toward enforced disclosure; a narrow ruling entrenches it and drops the suppression requirement as the defensive project becomes unnecessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_settlement_trajectory, empirical, 'Whether the reading''s persistence reflects durable doctrine or merely pending litigation risk.').

omega_variable(
    linking_technique_gradient,
    'Does the narrow reading hold uniformly across static linking, dynamic linking, plugin loading, and network invocation, or does derivativeness track technical closeness?',
    'Doctrinal analysis of emerging case law keyed to linkage technique, combined with engineering studies of coupling intensity across techniques.',
    'A technique gradient would split this reading into a family of constraints with distinct epsilon values per linkage mode; uniformity keeps it a single clean rule. The interface-boundary sibling is effectively a bet that the gradient is real and runs along API lines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_technique_gradient, conceptual, 'Whether the aggregation line is invariant across linkage techniques.').

omega_variable(
    contributor_reciprocity_net_effect,
    'Does defeating contributor reciprocity expectations leave the GPL commons worse off, or does expanded proprietary adoption grow the commons enough to compensate?',
    'Longitudinal study of contribution rates, maintainer funding, and library health for GPL projects before and after prominent proprietary adoptions enabled by this reading.',
    'Net harm to the commons pushes the arrangement toward pure extraction with concentrated gain and identifiable victims; net benefit supports a stronger coordination reading and lowers effective extraction below the authored base rate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributor_reciprocity_net_effect, empirical, 'Whether the reading''s costs to contributors are offset by adoption-driven growth of the commons.').

omega_variable(
    suppression_structural_vs_normative,
    'Is the reading''s hold on industry practice maintained by structural litigation-cost asymmetry alone, or also by internalized norms that treat linking as categorically unproblematic?',
    'Compare compliance officers'' stated legal reasoning against actual exposure analysis; observe behavioral latency after adverse rulings in single jurisdictions.',
    'If the hold is substantially normative, the reading persists in practice long after adverse precedent and resists formal correction; if structural, an adverse apex ruling flips vendor behavior within a release cycle and the suppression requirement collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_normative, empirical, 'Structural versus internalized maintenance of the permissive practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl__tr_t6, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(gpl__tr_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(gpl__tr_t18, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpl__be_t6, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 6, 0.36).
narrative_ontology:measurement(gpl__be_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(gpl__be_t18, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gpl__su_t6, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 6, 0.29).
narrative_ontology:measurement(gpl__su_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(gpl__su_t18, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 18, 0.37).
narrative_ontology:measurement(gpl__su_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, information_standard).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% The colloquial 'GPL linking question' decomposes into three structurally distinct constraints - one per reading of the gpl_derivative_work_trigger kernel - because assigning a single epsilon to the label conflates rival boundary criteria with different beneficiary/victim structures. This story (narrow_linking_permissive_reading) authors epsilon for the arrangement under the modification-only line as seen from that reading's own lights (moderate); the broad_copyleft_reading story authors epsilon for the same ecosystem as seen from the propagation reading (high, concentrated on vendors); the interface_boundary_reading story authors epsilon for the conditional-API arrangement. The upstream/downstream structure runs through citation practice: the steward's guidance cites the broad premises, vendor compliance positions cite this reading, and the interface reading borrows the technical vocabulary of both. Each member links the others via affects_constraints; orphaning any one would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
