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
 *   human_readable: GPL Derivative-Work Trigger — Narrow Linking-as-Aggregation Reading
 *   domain: software_licensing_copyright_law
 *
 * SUMMARY:
 *   This story generates ONE reading of the contested GPL derivative-work
 *   kernel: the narrow linking-permissive reading, under which linking
 *   (including dynamic linking) is treated as mere aggregation rather than
 *   derivation, so that only direct modification or copying of GPL source
 *   code triggers copyleft obligations. Under this reading, vendors build a
 *   durable architectural wall — dynamic linking against GPL utility code —
 *   that lets them ship proprietary modules commercially without extending
 *   source availability to those modules. This frustrates the FSF's
 *   propagation goal for the code in question but does so through legally
 *   administrable, widely adopted commercial practice, not through overt
 *   breach. The coordination function (a workable bright-line rule for
 *   licensing compliance) is real; the extraction (commercial capture of
 *   copyleft-protected infrastructure without reciprocal disclosure) is also
 *   real and grows as the practice becomes normalized. Sibling readings
 *   (broad_copyleft_reading, interface_boundary_reading) are separate
 *   constraint stories with their own ε and stakeholder structures — this
 *   file does not describe or average over them; the contest between readings
 *   is routed to omega variables per the committer frame.
 *
 * KEY AGENTS:
 *   - proprietary_module_vendors: beneficiary (powerful/arbitrage) — captures GPL utility without reciprocal disclosure
 *   - commercial_platform_integrators: beneficiary/agenda_setter (organized/arbitrage) — institutionalizes and defends the reading
 *   - downstream_users_of_linked_binaries: payer (powerless/trapped) — loses source-availability guarantee for the combined product
 *   - gpl_contributor_community: payer (organized/constrained) — sees propagation goal frustrated for code they licensed to prevent exactly this
 *   - free_software_foundation: excluded (organized/constrained) — authored the license and the contrary interpretation but cannot bind the reading
 *   - courts_and_licensing_counsel: observer (institutional/analytical) — adjudicates and issues opinions favoring administrability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.58).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.42).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative-Work Trigger — Narrow Linking-as-Aggregation Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software_licensing_copyright_law").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '5a50da30-6271-416b-9d8a-0c339a264c13').
narrative_ontology:cs_kernel_codification('5a50da30-6271-416b-9d8a-0c339a264c13', fixed_text).
narrative_ontology:cs_authority_grounding('5a50da30-6271-416b-9d8a-0c339a264c13', distributed).
narrative_ontology:cs_reading_relation('5a50da30-6271-416b-9d8a-0c339a264c13', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a50da30-6271-416b-9d8a-0c339a264c13', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('5a50da30-6271-416b-9d8a-0c339a264c13', foundational, derivative_work_requires_modification_or_copying).
narrative_ontology:cs_axiom_status(derivative_work_requires_modification_or_copying, holdable).
narrative_ontology:cs_axiom_grounding('5a50da30-6271-416b-9d8a-0c339a264c13', derivative_work_requires_modification_or_copying, conventional).
narrative_ontology:cs_axiom('5a50da30-6271-416b-9d8a-0c339a264c13', secondary, runtime_linking_produces_two_independent_works).
narrative_ontology:cs_axiom_status(runtime_linking_produces_two_independent_works, holdable).
narrative_ontology:cs_axiom_grounding('5a50da30-6271-416b-9d8a-0c339a264c13', runtime_linking_produces_two_independent_works, conventional).
narrative_ontology:cs_reference_frame('5a50da30-6271-416b-9d8a-0c339a264c13', gplv2_copyleft_propagation_intent).
narrative_ontology:cs_drift_state('5a50da30-6271-416b-9d8a-0c339a264c13', contemporary_commercial_linking_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a50da30-6271-416b-9d8a-0c339a264c13', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_module_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_platform_integrators).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, downstream_users_of_linked_binaries).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_contributor_community).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyright_derivative_work_requires_modification_or_copying_not_mere_linking).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ship closed-source components that dynamically link against GPL-licensed libraries at runtime. Under this reading, linking is mere aggregation, so they owe no source disclosure for their own code. They structure builds specifically around dynamic linking (rather than static linking or code copying) to sit on the permissive side of the line, and they lobby for and cite this reading in licensing opinions and vendor contracts.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_module_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Build platforms and SDKs that bundle GPL utilities alongside proprietary application logic, relying on the aggregation reading in their published compliance policies. They actively promote and defend this reading in industry consortia, legal guidance documents, and against FSF-aligned compliance challenges, because their business model depends on the wall it builds.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_platform_integrators, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_platform_integrators, agenda_setter).

% Receive compiled software that links proprietary modules against GPL components. Under the broad reading they would be entitled to source for the whole combined work; under this narrow reading they get source only for the GPL pieces, not the proprietary logic layered on top. They have no practical way to know, from the binary alone, whether the vendor's linking choice reflects genuine architecture or licensing arbitrage, and no standing to compel disclosure.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, downstream_users_of_linked_binaries, payer,
    powerless, biographical, trapped, global).

% Wrote and licensed code under GPL specifically to ensure that derivative works built on it stay free (copyleft propagation). This reading lets proprietary vendors capture the utility of their contributions while defeating the propagation mechanism they relied on. Their recourse is relicensing future contributions under stricter terms (e.g. AGPL) or litigation, both slow and only partially effective against the installed base of already-linked binaries.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_contributor_community, payer,
    organized, generational, constrained, global).

% Authored the GPL and the FAQ position that dynamic linking creates a derivative work, explicitly to prevent the aggregation loophole. Its interpretive guidance is not binding law; courts and vendors are free to and increasingly do adopt the narrower reading this story describes, which the FSF has no direct mechanism to override outside of litigation or license-text amendment (which does not touch already-released code).
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, excluded,
    organized, civilizational, constrained, global).

% Adjudicate derivative-work disputes and issue compliance opinions. Jurisdictional splits exist; this reading has gained ground in commercial legal opinions (particularly in the U.S.) partly because it is more administrable and more favorable to commercial deployment, even where the FSF's own drafting intent points the other way.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, courts_and_licensing_counsel, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_module_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, mechanically testable rule (did you modify or copy GPL source code, yes or no) that lets developers predict their licensing obligations from build-system choices rather than from an unresolvable metaphysical question about what counts as a 'derivative work' under copyright law.
% TRANSFER_FUNCTION: Moves the practical benefit of copyleft's propagation guarantee away from downstream users and the GPL contributor community, and toward vendors who can now commercially exploit GPL components without extending the same freedoms to the combined proprietary product they ship.
% ABSENT_VOICES: The Free Software Foundation and the broader free-software movement, whose drafting intent is that linking should propagate the license, are not the deciding voice in this reading — courts and commercial counsel, who favor administrability and vendor flexibility, are. End users who receive binaries have no voice in which reading their vendor adopted and no way to contest it after the fact.
% DISAPPEARANCE_RATIONALE: If this reading were displaced by the broad copyleft reading tomorrow, every vendor currently relying on dynamic linking as a compliance strategy would face a wave of source-disclosure obligations or relicensing/rearchitecture costs; commercial products built on this architecture would require significant remediation, and the value of the proprietary layer built atop GPL infrastructure would be substantially reduced or exposed.
% FOUNDING_PROBLEM: Copyright law's 'derivative work' concept was not written with software linking in mind, and the GPL's own text does not unambiguously resolve whether dynamic linking at runtime creates a single combined work or two separate works that merely coexist in memory. This reading was built to give that ambiguity a workable, predictable answer for commercial software development.
% FOUNDING_PROBLEM_CORROBORATION: Commercial licensing counsel and platform vendors attest the ambiguity is real and that a bright-line modification/copying test is the only administrable resolution. The FSF and copyleft-aligned legal scholars (e.g. in FSF's own compliance guidance and amicus positions) attest the ambiguity was already resolved by the license text's intent and that this reading is a vendor-favorable erosion, not a genuine interpretation — no court has definitively settled the question across all jurisdictions, so no single source outside the interested parties has closed it.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects substantial but not extreme capture: vendors gain commercial use of copyleft infrastructure without full reciprocal disclosure, but the GPL components themselves remain available and modifiable — only the proprietary layer escapes. Suppression (0.42) is moderate: the reading is enforced less by coercion than by the fact that once a build architecture is chosen and shipped, downstream users have no practical mechanism to contest which reading applies. Accessibility collapse (0.35) is low-moderate because the interface_boundary and broad_copyleft readings remain live alternatives in other jurisdictions and contexts — this reading has not achieved uncontested dominance. Resistance (0.62) is substantial: the FSF, copyleft-aligned developers, and some courts actively contest this reading, which is precisely why it functions as a tangled rope (coordination value plus contested extraction) rather than a clean rope.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor/integrator seat, this reading is a legitimate, predictable coordination rule that resolves real interpretive ambiguity in copyright law as applied to software linking. From the GPL contributor and downstream-user seats, the identical rule operates as an extraction mechanism that lets commercial actors free-ride on copyleft-licensed infrastructure while defeating the propagation guarantee the license was written to secure. The engine computes these as structurally different seat experiences from the same positional data — the claim (tangled_rope) is authored to reflect that both a genuine coordination function AND asymmetric extraction are present, not to resolve which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary module vendors and platform integrators are beneficiaries with arbitrage-grade exit: they choose their linking architecture specifically to land on the favorable side of this reading, and can restructure builds if the reading were challenged. Downstream users are trapped — they receive a compiled binary and have no visibility into or leverage over the vendor's licensing architecture choice. The GPL contributor community sits at organized/constrained: they can relicense future code (e.g., to AGPL) but cannot retroactively alter the licensing status of already-linked, already-shipped binaries built under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine ambiguity in how copyright's derivative-work doctrine applies to software linking — remains partially live (courts have not uniformly resolved it), which cuts against treating this as pure mandatrophy. But the practice has hardened past the point of resolving ambiguity in good faith: architectural choices are now made deliberately to exploit the interpretive gap rather than merely to build software efficiently, which is why founding_problem_status is authored as contested rather than dead or live outright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_as_derivative_work_unsettled_law,
    'Does dynamic linking against a GPL library constitute creation of a derivative work under copyright law, or is it aggregation of independent works that happen to execute together?',
    'A definitive appellate or supreme court ruling squarely addressing software linking as derivative-work formation, ideally across multiple major jurisdictions (US, EU) to establish convergent or divergent precedent.',
    'If courts converge on the broad reading, this constraint''s extractiveness would rise sharply and its coordination-function defense would collapse, likely reclassifying it toward snare; if courts converge on this narrow reading, the constraint stabilizes as a durable tangled_rope or even shifts toward rope as the FSF''s contrary position loses practical force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_as_derivative_work_unsettled_law, empirical, 'Whether courts will ultimately treat linking as derivation or aggregation is unresolved and jurisdiction-dependent.').

omega_variable(
    reading_selection_as_committer_structure,
    'Given that the GPL derivative-work kernel supports at least three structurally distinct readings (narrow linking-permissive, broad copyleft, interface boundary), is the selection among readings best understood as legal interpretation converging on a correct answer, or as an ongoing contest between parties with irreconcilable interests in which reading prevails?',
    'Track whether reading adoption correlates more strongly with jurisdiction/precedent (interpretation model) or with which party is asserting the reading in litigation (interest-contest model) across a corpus of licensing disputes.',
    'If interpretation converges over time, treat this constraint''s extractiveness as provisional and likely to be corrected by future case law. If reading selection tracks party interest rather than doctrine, the three readings should be understood as a stable, ongoing three-way contest rather than a transitional ambiguity awaiting resolution — which would argue against ever expecting convergence and for treating each reading''s constraint as a durable, independently persisting structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_committer_structure, conceptual, 'Whether the kernel''s multiple readings represent transitional legal ambiguity or a stable multi-party interpretive contest with no expected convergence.').

omega_variable(
    fsf_intent_vs_license_text_gap,
    'Does the GPL''s actual license text support the FSF''s stated broad-copyleft intent regarding linking, or does the text''s silence on the linking question leave room for the narrower reading this constraint describes regardless of drafting intent?',
    'Close textual analysis of GPLv2/v3 language (including the ''System Libraries'' exception and the LGPL''s existence as a separate instrument) compared against FSF''s own historical FAQ statements and drafting commentary.',
    'If the text itself is genuinely silent or ambiguous, this reading has independent textual legitimacy beyond mere commercial convenience, softening the extraction framing. If the text clearly supports the broad reading and this narrow reading depends entirely on judicial reluctance to enforce it as written, the extraction framing strengthens and founding_problem_status should be read closer to dead-but-persisting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsf_intent_vs_license_text_gap, conceptual, 'Whether this reading has independent textual grounding or depends on divergence between license text and drafting intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_derivative_work_trigger kernel, decomposed per the ε-invariance principle because the three readings produce structurally distinct beneficiary/victim sets and distinct ε values from the same underlying license text. broad_copyleft_reading treats dynamic linking as triggering disclosure (low ε, favors GPL contributor community); interface_boundary_reading conditions the trigger on API coupling tightness (intermediate ε, technical middle ground); this narrow_linking_permissive_reading treats linking as aggregation only (higher ε, favors commercial vendors). All three are linked bidirectionally in principle; this file declares the forward edges to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
