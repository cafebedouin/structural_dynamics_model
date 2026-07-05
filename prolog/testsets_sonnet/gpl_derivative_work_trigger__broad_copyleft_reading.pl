% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Linking-as-Derivative-Work Doctrine (Broad Copyleft Reading)
 *   domain: software_licensing/copyright_law
 *
 * SUMMARY:
 *   This story instantiates the broad copyleft reading of the GPL
 *   derivative-work kernel: linking, including dynamic linking, is treated as
 *   sufficient to create a derivative work, triggering source disclosure for
 *   the entire linked combination. This is a Rope from the maintainer/commons
 *   seat — it solves a genuine free-rider problem (proprietary absorption of
 *   shared code without reciprocation) — but it operates as a real
 *   cost-imposing structure on vendors from the payer seat, hence non-trivial
 *   extractiveness and suppression scores despite the rope claim. Two sibling
 *   readings of the same underlying kernel — the narrow
 *   linking-as-aggregation reading and the interface-boundary reading — are
 *   NOT part of this constraint; they are separate constraint stories with
 *   their own ε values and beneficiary/victim structures, linked via
 *   network.affects_constraints. This story does not average across them or
 *   hedge its ε to accommodate them.
 *
 * KEY AGENTS:
 *   - gpl_licensed_project_maintainers: agenda_setter/beneficiary (organized/arbitrage) — sets and benefits from the broad linking trigger
 *   - free_software_foundation: agenda_setter/observer (institutional/analytical) — articulates and defends the doctrine
 *   - proprietary_software_vendors: primary payer (powerful/constrained) — bears compliance or avoidance cost
 *   - embedded_systems_manufacturers: secondary payer (moderate/trapped) — bears compliance cost with least flexibility
 *   - downstream_source_recipients: beneficiary (moderate/mobile) — gains source access rights
 *   - software_licensing_courts: observer (institutional/analytical) — adjudicates but does not settle the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.42).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.55).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Linking-as-Derivative-Work Doctrine (Broad Copyleft Reading)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, 'c5c93761-b34a-45f9-8182-8560ff0385ff').
narrative_ontology:cs_kernel_codification('c5c93761-b34a-45f9-8182-8560ff0385ff', fixed_text).
narrative_ontology:cs_authority_grounding('c5c93761-b34a-45f9-8182-8560ff0385ff', lineage).
narrative_ontology:cs_interpretation_layer_present('c5c93761-b34a-45f9-8182-8560ff0385ff').
narrative_ontology:cs_reading_relation('c5c93761-b34a-45f9-8182-8560ff0385ff', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5c93761-b34a-45f9-8182-8560ff0385ff', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('c5c93761-b34a-45f9-8182-8560ff0385ff', foundational, functional_integration_constitutes_derivation).
narrative_ontology:cs_axiom_status(functional_integration_constitutes_derivation, holdable).
narrative_ontology:cs_axiom_grounding('c5c93761-b34a-45f9-8182-8560ff0385ff', functional_integration_constitutes_derivation, conventional).
narrative_ontology:cs_axiom('c5c93761-b34a-45f9-8182-8560ff0385ff', secondary, linking_mechanism_is_legally_immaterial_to_derivative_status).
narrative_ontology:cs_axiom_status(linking_mechanism_is_legally_immaterial_to_derivative_status, holdable).
narrative_ontology:cs_axiom_grounding('c5c93761-b34a-45f9-8182-8560ff0385ff', linking_mechanism_is_legally_immaterial_to_derivative_status, instrumental).
narrative_ontology:cs_reference_frame('c5c93761-b34a-45f9-8182-8560ff0385ff', fsf_original_linking_faq_doctrine).
narrative_ontology:cs_drift_state('c5c93761-b34a-45f9-8182-8560ff0385ff', post_saas_and_microservices_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c5c93761-b34a-45f9-8182-8560ff0385ff', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensed_project_maintainers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_source_recipients).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_foundation).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, embedded_systems_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and license core libraries under GPL, relying on the broad linking-as-derivative-work reading to pull any code that links against their library into the same disclosure regime. This maximizes the reach of copyleft: anyone who ships a linked binary must release corresponding source. They benefit from every downstream adopter's forced compliance or the compliance-avoidance workarounds that keep their library out of proprietary stacks entirely.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensed_project_maintainers, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensed_project_maintainers, beneficiary).

% End users and third-party developers who receive shipped binaries that link against GPL libraries. Under the broad reading they are entitled to the full corresponding source of the linked work, including proprietary glue code, enabling them to inspect, modify, and redistribute. They have no cost under this reading; they simply gain rights they would not have under a narrower interpretation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_source_recipients, beneficiary,
    moderate, biographical, mobile, global).

% Articulates and defends the broad linking-as-derivative-work doctrine in its licensing FAQ and enforcement guidance, treats dynamic linking as functionally equivalent to static linking for derivative-work purposes, and coordinates or supports litigation and compliance campaigns that enforce this reading against commercial adopters.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_foundation, observer).

% Want to link against widely-used GPL libraries for functionality (codecs, drivers, cryptographic primitives) but under this reading must either release their entire linked application's source, pay for a dual-licensed alternative, reimplement the functionality from scratch, or avoid the library entirely. Legal uncertainty about where 'linking' ends and 'mere aggregation' begins imposes ongoing compliance-review costs regardless of which path they choose.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Ship firmware that dynamically links against GPL components (commonly a Linux kernel and userspace utilities) inside consumer devices where the linked binary and hardware are tightly bundled. Under the broad reading, the derivative-work trigger extends to their proprietary application layer if the linking is not cleanly separable, forcing either source release of proprietary firmware logic or costly re-architecture to establish a defensible interface boundary. Product cycles and hardware lock-in make late compliance discovery expensive.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, embedded_systems_manufacturers, payer,
    moderate, biographical, trapped, national).

% Adjudicate disputes over whether a specific linking arrangement constitutes a derivative work, drawing on copyright doctrine that predates software linking as a technical practice. Their rulings are jurisdiction-specific and inconsistent, which is precisely why this reading remains contested rather than settled.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, software_licensing_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensed_project_maintainers).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a commons: contributors pool code under an expectation that anyone who builds functionally integrated software on top of it returns their modifications and integration code to the commons, sustaining a shared, inspectable software ecosystem instead of one where contributions are extracted into closed derivatives.
% TRANSFER_FUNCTION: Moves source-code disclosure obligations from GPL library authors onto any party whose binary links against that library, and correspondingly moves inspection/modification rights to whoever receives the resulting binary.
% ABSENT_VOICES: Proprietary vendors who settled quietly or abandoned GPL-adjacent functionality rather than litigate rarely appear in the public record; their compliance-avoidance decisions are invisible in case law even though they are a direct behavioral effect of this reading.
% DISAPPEARANCE_RATIONALE: If the broad linking-triggers-derivative-work reading vanished, proprietary vendors would freely link against GPL libraries without disclosure risk, GPL project maintainers would lose their principal enforcement lever for pulling integration code into the commons, and the commercial incentive to maintain permissively-licensed or dual-licensed alternatives to popular GPL libraries would sharply diminish.
% FOUNDING_PROBLEM: Early free-software authors needed a mechanism to prevent their code from being absorbed into proprietary products without reciprocal contribution — a company could take a GPL library, link it invisibly into a closed binary, and ship the combination without ever triggering the disclosure obligation that applied to direct modification.
% FOUNDING_PROBLEM_CORROBORATION: Independent software licensing scholars and several national courts (notably in Germany and the U.S. in library-linking disputes) corroborate that undisclosed linking-based circumvention of copyleft remains an active, litigated concern, not a historical artifact resolved by settled law — though the same courts disagree on where the derivative-work line sits, which is exactly the live contest this reading takes a side in.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).
:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises modestly over the interval (0.28→0.42) as the broad reading was progressively formalized in FSF guidance, enforcement actions, and a growing body of settled/litigated compliance cases — vendors increasingly internalize the cost of the doctrine rather than treating it as a paper risk. Suppression (0.55 at end) reflects that the reading persists partly through litigation threat and license-compliance audits (e.g., by compliance organizations and the FSF's own enforcement arm), not purely through voluntary agreement. Theater ratio stays low (0.18) because the disclosure obligation, where triggered, produces genuinely functional outcomes (real source code changes hands) rather than symbolic compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the maintainer/FSF seat this looks like a coordination mechanism sustaining a functioning commons — a Rope. From the vendor seat, especially embedded manufacturers who discover the trigger late in a hardware cycle, the same rule functions as an imposed cost with no negotiated consent, closer to a Tangled Rope. The engine computes these divergent seat classifications from the same structural data; the claimed_type of rope here reflects the coordination-function seat, not an averaged verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   GPL maintainers and the FSF sit at the beneficiary end: they administer the doctrine and their collective commons grows through its enforcement. Downstream source recipients are beneficiaries who bear no cost under this reading. Proprietary vendors and embedded manufacturers are targets: the disclosure trigger extracts source code or compliance cost from them, and their exit options differ — general-purpose vendors have more room to substitute alternative libraries (constrained) while embedded manufacturers, locked into hardware/firmware bundles with long product cycles, have far less (trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (silent absorption of shared code into closed products) remains live per court activity and scholarly consensus, so this is not a case of an obsolete mandate being defended by inertia — the doctrine's coordination function still answers a real, current problem, distinguishing it from a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_mechanism_derivative_status,
    'Does the technical mechanism of linking (static vs. dynamic vs. IPC/RPC boundary) map onto a legally coherent derivative-work test, or is ''linking'' too technically heterogeneous a category for a single copyright rule to track?',
    'A definitive appellate ruling (or harmonized international case law) that articulates a technical test for derivative-work status tied to linking mechanism, rather than treating all linking as functionally equivalent.',
    'If courts converge on treating dynamic linking as categorically different from static linking, the broad reading narrows sharply and much of its current suppression/extractiveness would migrate to the interface_boundary_reading sibling instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_mechanism_derivative_status, conceptual, 'Whether linking mechanism should determine derivative-work status under copyright doctrine.').

omega_variable(
    kernel_reading_which_prevails,
    'Among the three readings of the gpl_derivative_work_trigger kernel (broad_copyleft, narrow_linking_permissive, interface_boundary), which will settle as the dominant legal interpretation, and does divergence across jurisdictions mean no single reading will ever fully prevail?',
    'Track jurisdictional convergence or persistent divergence over a multi-decade window; a global software market may never produce a single settled reading, in which case all three readings remain simultaneously live as jurisdiction-dependent constraints.',
    'If broad_copyleft_reading is confirmed as the prevailing interpretation in major jurisdictions (EU, US), its extractiveness and suppression values understate its true global reach; if narrow or interface readings prevail, this story''s structural claims describe a minority position rather than the operative legal reality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_which_prevails, empirical, 'Whether one reading of the linking-derivative-work kernel will become dominant or all three persist as jurisdiction-dependent.').

omega_variable(
    compliance_avoidance_undercounts_extraction,
    'How much of the doctrine''s true extractive effect is invisible because vendors avoid GPL libraries entirely (rewriting functionality, paying for proprietary alternatives) rather than triggering and then contesting the disclosure obligation?',
    'Industry surveys of engineering decisions to avoid GPL dependencies specifically due to linking-derivative-work risk, compared against documented litigation/settlement cases.',
    'If avoidance behavior is large relative to litigated cases, the authored extractiveness score understates the doctrine''s real economic footprint, since most of its effect never surfaces as a measurable dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_avoidance_undercounts_extraction, empirical, 'Whether avoidance behavior hides the doctrine''s true extractive reach from visible enforcement data.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t6, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(gpl__tr_t12, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(gpl__tr_t18, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 18, 0.16).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 30, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpl__be_t6, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(gpl__be_t12, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(gpl__be_t18, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl__su_t6, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(gpl__su_t12, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(gpl__su_t18, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(gpl__su_t24, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the gpl_derivative_work_trigger kernel. narrow_linking_permissive_reading treats linking as mere aggregation (low extraction, permissive for vendors); interface_boundary_reading treats clean API separation as defeating derivative-work status regardless of coupling (a middle position turning on architectural discipline rather than linking mechanism). Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure; they are not merged or averaged. The upstream causal direction runs from this broad reading (the FSF's original and most litigated position) toward influencing the legitimacy conditions under which the narrower siblings are argued in court and in vendor compliance practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
