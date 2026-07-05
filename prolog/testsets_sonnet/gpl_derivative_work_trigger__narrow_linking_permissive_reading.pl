% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: Narrow Linking-as-Aggregation Reading of the GPL Derivative Work Trigger
 *   domain: legal/software_licensing
 *
 * SUMMARY:
 *   Commercial software vendors routinely ship products that link against
 *   GPL-licensed libraries (glibc, GPL-licensed kernel modules, various
 *   utility libraries) without releasing the source of their own proprietary
 *   code. This reading treats that combination as legally permissible: the
 *   GPL's derivative-work trigger fires only on modification of the GPL code
 *   itself, not on the act of linking, which is characterized as mere
 *   aggregation — putting two independent works side by side at runtime. This
 *   is the dominant practical reading among commercial distributors and much
 *   of the enterprise legal community, though it departs from the FSF's own
 *   documented drafting intent (evidenced by the LGPL's existence as a
 *   deliberately weaker license specifically carving out a linking exception
 *   the plain GPL was not meant to have).
 *
 * KEY AGENTS:
 *   - proprietary_module_vendors: primary beneficiary (organized/arbitrage) — link without disclosing
 *   - commercial_linux_distributors: primary beneficiary and agenda-setter (institutional/arbitrage) — funds and propagates the reading
 *   - downstream_source_seeking_users: primary payer (powerless/constrained) — loses source-availability guarantee
 *   - gpl_contributor_community: secondary payer (organized/constrained) — propagation goal frustrated
 *   - free_software_foundation: excluded voice (organized/constrained) — drafting intent overridden in practice
 *   - software_licensing_courts: analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.58).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.35).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Narrow Linking-as-Aggregation Reading of the GPL Derivative Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "legal/software_licensing").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '62a2e914-022b-4f3e-93dc-d3d05bc0f02d').
narrative_ontology:cs_kernel_codification('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', fixed_text).
narrative_ontology:cs_authority_grounding('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', practice).
narrative_ontology:cs_interpretation_layer_present('62a2e914-022b-4f3e-93dc-d3d05bc0f02d').
narrative_ontology:cs_reading_relation('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', foundational, linking_alone_is_never_sufficient_for_derivation).
narrative_ontology:cs_axiom_status(linking_alone_is_never_sufficient_for_derivation, holdable).
narrative_ontology:cs_axiom_grounding('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', linking_alone_is_never_sufficient_for_derivation, conventional).
narrative_ontology:cs_axiom('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', secondary, modification_is_the_sole_dispositive_trigger).
narrative_ontology:cs_axiom_status(modification_is_the_sole_dispositive_trigger, holdable).
narrative_ontology:cs_axiom_grounding('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', modification_is_the_sole_dispositive_trigger, conventional).
narrative_ontology:cs_reference_frame('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', aggregation_doctrine_baseline).
narrative_ontology:cs_drift_state('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', post_lgpl_bifurcation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('62a2e914-022b-4f3e-93dc-d3d05bc0f02d', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_module_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_linux_distributors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, downstream_source_seeking_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_contributor_community).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, freedom_to_combine_software_of_different_licenses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ship closed-source modules that link against GPL libraries at runtime or build time. Under this reading, linking alone does not create a derivative work, so they distribute binaries without releasing source, capturing the coordination benefit of the GPL ecosystem (mature libraries, toolchains, community maintenance) while withholding their own modifications from that commons.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_module_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Package and ship systems combining GPL kernel/userland components with proprietary drivers and applications. They actively lobby for and litigate in favor of the narrow reading because their business model depends on the linking boundary holding; they fund legal opinions and standards bodies that formalize the aggregation framing.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_linux_distributors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_linux_distributors, agenda_setter).

% Receive binaries built from a mix of GPL and proprietary code and want to inspect, modify, or redistribute the whole running system, as the GPL's stated purpose promises. Under the narrow reading they get source for the GPL library but not for the linked proprietary module, so their practical ability to study or modify the software they run is unchanged from a fully proprietary system for the parts that matter to them. Their only exit is refusing the combined product, which is often not commercially available in a pure-GPL alternative.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, downstream_source_seeking_users, payer,
    powerless, biographical, constrained, global).

% Volunteer and paid developers who contribute code under the GPL expecting derivative works built on their labor to propagate the same freedoms (copyleft's viral intent). The narrow linking reading lets commercial actors build proprietary value on top of their contributions without reciprocation, diluting the propagation mechanism their license was drafted to enforce. They can relicense their own future contributions but cannot retroactively close the linking loophole for existing code.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_contributor_community, payer,
    organized, generational, constrained, global).

% Drafted the GPL with an explicit intent that linking create a derivative work (this is documented in FSF's own guidance and the LGPL's existence as a deliberately weaker alternative). Courts and corporate legal departments in major jurisdictions have not uniformly adopted the FSF's interpretation, so the FSF's stated intent is present in commentary but structurally excluded from binding authority over how the clause is actually enforced in this reading's dominant jurisdictions.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, excluded,
    organized, civilizational, constrained, global).

% Adjudicate specific linking disputes when litigated, applying national copyright derivative-work doctrine to the facts of a given technical architecture. Their rulings are scattered, technology-specific, and often settled before appeal, leaving the narrow reading dominant by default rather than by clear precedent.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, software_licensing_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_linux_distributors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows independently developed software components under different licenses to be combined into working systems (an OS kernel with proprietary drivers, a GPL library with a commercial application) without forcing every combining party into the GPL, which lets GPL code circulate more widely as infrastructure.
% TRANSFER_FUNCTION: Moves the commercial value of source-availability from the GPL contributor community and downstream users (who would otherwise receive source to the whole combined work) to the vendors who link against GPL code without releasing their own modules.
% ABSENT_VOICES: The FSF, which drafted the license with the opposite intent, is not a party to most individual linking disputes and has limited standing to enforce its own interpretation once distributors adopt the narrow reading; end users who cannot read a linking diagram have no seat in the technical argument at all.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished and the broad copyleft reading became binding everywhere, proprietary vendors would face a hard choice: relicense, isolate via clean interfaces, or exit the GPL ecosystem entirely — a real rearrangement of the commercial software landscape. Whether that counts as the world 'rearranging' or 'reverting to what the license always meant' is itself the dispute between the reading's beneficiaries and the FSF.
% FOUNDING_PROBLEM: The GPL's derivative-work trigger exists to answer a genuinely hard question: how tightly must two pieces of software be combined before the combination is one 'work' subject to the license's propagation terms, versus two separate works merely distributed together?
% FOUNDING_PROBLEM_CORROBORATION: Commercial distributors and their counsel attest the linking boundary is a workable, technically principled line consistent with aggregation doctrine in copyright law generally. The FSF and much of the historical contributor base attest the founding problem was intentionally resolved in the license text toward the broad reading, and that the narrow reading is a court-and-lobbying-driven departure from the drafters' documented intent, not a resolution of genuine ambiguity.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) is authored at a moderate-high level because real commercial value (unreleased proprietary source, avoided disclosure costs) flows from the GPL contributor commons to vendors without reciprocal contribution — but it is not maximal because the underlying GPL code itself remains available and the extraction is bounded to the linked module's value, not the whole combined system. Suppression (0.35) is comparatively low: no one is coerced into using GPL-linked proprietary combinations, and clean-room alternatives (pure GPL stacks, LGPL-only linking) exist, though they are often commercially disfavored. Theater ratio (0.22) is low: the aggregation-vs-derivation distinction is a genuine, actively litigated legal question, not mere performance, though its increasing entrenchment via standardized legal opinions has a growing theatrical component (boilerplate compliance memos citing the reading as settled when it remains contested).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (commercial distributors), the linking boundary is a stable, well-reasoned legal line that enables a mixed commercial/open ecosystem to function at all. From the payer seat (contributor community, source-seeking users), the identical structure is experienced as a loophole that lets commercial actors extract the benefits of a copyleft commons while opting out of the one obligation that commons was built to enforce. The engine's per-seat computation should reflect this asymmetry directly from the declared power/exit/beneficiary data, not from any authored reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and distributors sit near the full-beneficiary end: they capture commercial value from the coordination function (mature, well-maintained GPL infrastructure) while the narrow reading insulates their own additions from the reciprocal obligation the license was designed to impose. Downstream users and the contributor community sit near the target end: their structural expectation — that using or building on GPL code propagates the same freedoms forward — is systematically frustrated by a reading that carves the linking boundary out of the propagation mechanism. Exit is constrained rather than trapped for most: pure-GPL or pure-LGPL alternatives sometimes exist but are frequently commercially inferior or unavailable, which is why d sits high-but-not-maximal for the payer seats rather than at the trapped extreme.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function here is real and should not be mislabeled as pure extraction: allowing linking-as-aggregation genuinely enables broader circulation of GPL infrastructure into commercial and proprietary contexts that would otherwise refuse to touch GPL code at all, which is a coordination benefit for the GPL ecosystem's reach even as it frustrates the propagation goal. Classifying this as tangled_rope rather than snare preserves that dual reading: there IS a coordination story (interoperability, broader adoption) riding alongside the asymmetric extraction (value capture without reciprocal disclosure), and both must be true simultaneously for tangled_rope to be the correct engine-computed type rather than an authored convenience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_disagreement_location,
    'Where exactly does this reading''s premise diverge from the sibling readings, and what would each sibling change structurally?',
    'Compare the three readings'' treatment of the specific technical fact pattern: dynamic linking against an unmodified GPL library, distributed as a separate binary. The broad_copyleft_reading treats the linking act itself as sufficient to create a derivative work regardless of interface cleanliness, extending the disclosure obligation to the linking party''s own code. The interface_boundary_reading locates the dispositive fact not in whether linking occurred but in whether the interface between the components is a clean, narrow, standardized boundary (an ABI/API) versus tight, ad hoc coupling — under that reading, this same fact pattern could go either way depending on interface design, which is a genuinely different test from this reading''s bright-line ''linking is never enough'' rule. This reading (narrow_linking_permissive_reading) forecloses neither sibling from being held by other parties, but it does directly deny the broad reading''s core premise for any party who adopts it.',
    'If the broad_copyleft_reading were adopted instead, the beneficiary set here (proprietary_module_vendors, commercial_linux_distributors) would become victims of a source-disclosure obligation; if the interface_boundary_reading were adopted, the outcome for any given vendor would depend on interface architecture rather than on the bright-line linking rule, producing a substantially different and more heterogeneous stakeholder map than this story''s uniform beneficiary/victim split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_disagreement_location, conceptual, 'Structural location of disagreement between the three kernel readings and what each would change.').

omega_variable(
    drafting_intent_vs_operative_law,
    'Is the FSF''s documented drafting intent (that linking should trigger the derivative-work obligation, evidenced by the LGPL''s existence as a deliberate carve-out) legally dispositive, or does it carry no more weight than any other interpretive aid once courts and commercial practice settle on a different reading?',
    'A definitive appellate ruling squarely addressing linking-as-derivation in a major jurisdiction would resolve this; absent that, the question remains open and the narrow reading persists by commercial practice and settlement patterns rather than binding precedent.',
    'If drafting intent were held dispositive, this reading''s classification would likely shift toward snare (the coordination story would be seen as a post-hoc rationalization of a departure from the license''s actual terms); if commercial practice and case-by-case adjudication remain controlling, the tangled_rope classification (genuine coordination function coexisting with extraction) is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafting_intent_vs_operative_law, conceptual, 'Whether drafter intent or settled commercial practice controls the kernel''s operative meaning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 5, 0.24).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 15, 0.31).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 25, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_derivative_work_trigger kernel. broad_copyleft_reading and interface_boundary_reading are separate constraint stories with their own ε values, stakeholder sets, and classifications. This story's beneficiaries (proprietary vendors, commercial distributors) would become victims under broad_copyleft_reading's disclosure obligation. All three readings are linked bidirectionally via network.affects_constraints to preserve the kernel family structure for contamination/propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
