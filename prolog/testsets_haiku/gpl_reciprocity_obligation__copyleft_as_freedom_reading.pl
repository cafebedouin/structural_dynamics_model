% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Freedom Reading)
 *   domain: software/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL's reciprocal obligation—viral licensing requiring any derivative
 *   of GPL-licensed code to be released under the same license—is here read
 *   as a constraint preserving downstream-user freedoms by preventing
 *   proprietary capture of the free-software commons. Under this reading, the
 *   GPL is NOT a restriction on proprietary integrators' business models
 *   (that is the restriction reading); rather, it is a structural guarantee
 *   that communities can build on their own work without having improvements
 *   locked away. Beneficiaries are downstream users and the free-software
 *   community; victims are proprietary integrators who cannot bundle GPL code
 *   into proprietary products without releasing the derivative. High
 *   suppression reflects the enforcement machinery (community legal action,
 *   license stewardship, copyright law) that prevents alternative licensing
 *   paths for GPL derivatives. This reading is ONE OF THREE kernel readings,
 *   distinguished by beneficiary/victim structure and the frame through which
 *   the constraint's effect is assessed.
 *
 * KEY AGENTS:
 *   - downstream_users: powerless, mobile exit — benefit from the GPL guarantee but cannot enforce it themselves
 *   - free_software_community: organized, mobile exit — maintain the constraint and enforce it through legal and norm-setting action
 *   - proprietary_software_integrators: institutional, constrained exit — must choose between reciprocal release or incompatible alternatives
 *   - intellectual_property_regimes: institutional, analytical — provide the legal foundation (copyright law) that makes the constraint enforceable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.72).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'bb2f71b3-af95-4796-9f21-fb63ca609617').
narrative_ontology:cs_kernel_codification('bb2f71b3-af95-4796-9f21-fb63ca609617', fixed_text).
narrative_ontology:cs_authority_grounding('bb2f71b3-af95-4796-9f21-fb63ca609617', lineage).
narrative_ontology:cs_interpretation_layer_present('bb2f71b3-af95-4796-9f21-fb63ca609617').
narrative_ontology:cs_reading_relation('bb2f71b3-af95-4796-9f21-fb63ca609617', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb2f71b3-af95-4796-9f21-fb63ca609617', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('bb2f71b3-af95-4796-9f21-fb63ca609617', foundational, downstream_user_freedom_as_primary_good).
narrative_ontology:cs_axiom_status(downstream_user_freedom_as_primary_good, holdable).
narrative_ontology:cs_axiom_grounding('bb2f71b3-af95-4796-9f21-fb63ca609617', downstream_user_freedom_as_primary_good, deontological).
narrative_ontology:cs_axiom('bb2f71b3-af95-4796-9f21-fb63ca609617', secondary, proprietary_integration_without_reciprocal_freedom_violates_commons_rights).
narrative_ontology:cs_axiom_status(proprietary_integration_without_reciprocal_freedom_violates_commons_rights, holdable).
narrative_ontology:cs_axiom_grounding('bb2f71b3-af95-4796-9f21-fb63ca609617', proprietary_integration_without_reciprocal_freedom_violates_commons_rights, deontological).
narrative_ontology:cs_reference_frame('bb2f71b3-af95-4796-9f21-fb63ca609617', software_freedom_as_individual_right).
narrative_ontology:cs_drift_state('bb2f71b3-af95-4796-9f21-fb63ca609617', contemporary_proprietary_cloud_dominance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bb2f71b3-af95-4796-9f21-fb63ca609617', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_community).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_software_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive GPLv3-licensed software with guaranteed rights to inspect, modify, and redistribute source code. Their freedom depends on the GPL's reciprocal obligation: any proprietary modifications or integrations must either remain proprietary (closed to them) or release the derivative back as open source. They benefit from the constraint because it prevents a derivative of free software from being locked behind proprietary walls that would deny them access to improvements built on their own community's work.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    powerless, biographical, mobile, global).

% Maintains the GPL toolchain and enforces the reciprocal license through community norm-setting, legal action when needed, and license stewardship. They benefit from the constraint because it ensures that improvements made to free software remain free, building a growing commons of shared code rather than allowing private capture of community-funded improvements. The constraint sustains their legitimacy and resource flow.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_community, beneficiary,
    organized, generational, mobile, global).

% Face a choice: either release proprietary integrations of GPL code as open source (accepting the reciprocal obligation), or develop incompatible alternatives at cost. They cannot bundle GPL code into proprietary products without triggering the reciprocal obligation. From their perspective, the GPL reciprocity is a constraint on their business model and intellectual property strategy—it prevents them from using free software as a cost-reducing input while keeping their own derivatives proprietary.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_software_integrators, payer,
    institutional, generational, constrained, global).

% Projects using MIT, Apache 2.0, or BSD licenses explicitly do NOT require reciprocal release of derivative works. They remain outside the constraint's scope, but would argue that the GPL's reciprocal obligation inappropriately restricts their own freedom to choose permissive licensing. Their exclusion reflects the GPL's architectural choice: not all open-source licenses reciprocate.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, permissive_licensed_projects, excluded,
    organized, generational, constrained, global).

% Copyright law provides the legal foundation for the GPL's enforceability. The GPL exploits copyright law to mandate reciprocal release—it uses the same legal mechanism proprietary software uses for exclusion (copyright), but inverts it to mandate inclusion. IP regimes both enable and constrain the GPL; changes to copyright law would directly affect the constraint's enforceability.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, intellectual_property_regimes, agenda_setter,
    institutional, generational, analytical, national).

% Experiences the constraint as a landscape of licensing choices, not a monolith. Some actors adopt GPL and benefit from reciprocity; others adopt permissive licenses and avoid it; still others develop closed-source alternatives entirely. They observe that the constraint's enforcement creates pressure for transparency and reciprocal contribution but does not eliminate proprietary development.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_development_ecosystem, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% FOUNDING_PROBLEM: In the early 1990s, proprietary software companies could incorporate free-software components into proprietary systems without returning improvements to the community, effectively harvesting the commons while denying downstream users (and the community itself) access to enhancements. The GPL was designed to prevent this capture: by using copyright law to mandate reciprocal release, it ensures that any derivative of GPL code remains subject to the same freedom guarantees.
% FOUNDING_PROBLEM_CORROBORATION: The free-software community and downstream-user advocates attest the founding problem remains live, citing proprietary capture of permissive-licensed code (e.g., Android/Linux, TensorFlow as near-misses). Proprietary integrators attest the founding problem is solved (they can now develop alternatives efficiently) and the GPL persists as a restriction on business models. Ecosystem analysis from independent researchers (e.g., Linux Foundation reports) shows that GPL reciprocity has measurably slowed proprietary enclosure of certain software categories while concentrating open-source development around GPL'd infrastructure.
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28) under this reading because the constraint does not extract wealth from downstream users—it PREVENTS proprietary integrators from extracting commons value by capturing free software. The beneficiary gains (access to transparent code, guaranteed freedom to modify and redistribute) are genuine coordination benefits, not rents. Suppression is HIGH (0.72) because enforcement depends on actively blocking alternative licensing paths for GPL derivatives: the constraint persists only because copyright law is weaponized to mandate reciprocity, and the free-software community enforces this through legal action and community exclusion of non-compliant actors. Theater is low-moderate (0.18): the coordination function (ensuring improvements stay free) is real, and enforcement is genuine, not theatrical. Accessibility collapse is high (0.81): once GPL code is incorporated, the alternatives (proprietary integration without reciprocal release, permissive licensing) collapse—integrators face a binary choice. Resistance is moderate (0.58): proprietary integrators actively resist GPL enforcement and push for permissive licenses; however, they do not fundamentally challenge the GPL's legitimacy as an institution (they work around it), so resistance is real but bounded. The measurement series shows extractiveness stabilizing and suppression plateauing mid-interval: the constraint's enforceability hardened as legal precedent accumulated (GPL enforceability was tested in court in the 2000s–2010s and confirmed), and as a result both metrics rose sharply early then leveled. Theater remains stable and low, indicating genuine functional enforcement rather than performative gate-keeping.
 *
 * PERSPECTIVAL GAP:
 *   From the free-software-community and downstream-user perspective, the GPL is a freedom-preserving coordination mechanism that prevents commons enclosure. From the proprietary-integrator perspective, it is a constraint on their right to choose their own licensing and integrate freely. The engine computes both seats' types from the same structural data (the reciprocal obligation, copyright law backing, enforcement machinery). The constraint-setter (free-software community) and the constraint-target (proprietary integrators) will compute different types from the same metrics because the structural asymmetry is real: one actor benefits from the reciprocal obligation, the other bears its cost. The claim (tangled_rope) reflects the structural reality: genuine coordination function (preventing commons fragmentation) coupled with asymmetric extraction (proprietary integrators cannot integrate freely without reciprocal obligation). This reading's perspectival gap is doctrinal: it asserts that the freedom-preserving function (not the restriction on proprietary models) is the constraint's primary character.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users and the free-software community are beneficiaries (d near 0.0): the constraint guarantees their freedom and prevents proprietary capture. They have mobile exit (they can switch to permissive licenses or closed-source alternatives), but they choose GPL because the reciprocal obligation serves their interest. Proprietary integrators are victims (d near 1.0): they bear the cost of the reciprocal obligation (must release derivatives or develop alternatives). They have constrained exit (they can adopt permissive licenses, but GPL code is locked to them; they can develop closed-source alternatives, but at cost). The asymmetry is structural: the same rule benefits one actor (downstream user gets transparency), harms another (integrator cannot integrate proprietary-ly), and activates a coordination function (ensuring improvements stay free). This is tangled_rope by definition: genuine coordination + asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The GPL's founding problem (preventing proprietary capture of the free-software commons) remains contested but demonstrably live. Permissive-licensed projects (MIT, Apache 2.0) remain outside the constraint's scope, and some have been captured proprietary-ly (Android/Linux, TensorFlow). The constraint persists because the free-software community actively enforces it and because the coordination benefit (accumulated shared improvements) creates ongoing resource advantage. There is no mandatrophy: the constraint's original function has not been displaced by a new function, and the original beneficiary (downstream users and the free-software community) remains the primary actor maintaining it. The founding problem is CONTESTED because proprietary integrators and some economists argue that permissive licensing achieves the same coordination benefits without the reciprocal obligation; however, empirical analysis suggests GPL derivatives accumulate improvements faster, supporting the constraint's functional case. The constraint is maintained by active choice, not inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_vs_restriction_frame_contestation,
    'Is the GPL reciprocal obligation fundamentally a freedom-preserving mechanism (the copyleft_as_freedom_reading) or a restriction-of-freedom mechanism (the copyleft_as_restriction_reading), or is the frame-choice itself the open question?',
    'The question is conceptually unresolvable by empirical test alone: both readings capture real structural facts (the GPL does prevent proprietary integration AND it does guarantee downstream-user transparency). Resolution requires a normative judgment: which freedom is more fundamental—the proprietary integrator''s freedom to choose their own licensing, or the downstream user''s freedom to inspect code built on their commons? Different political philosophies will resolve this differently.',
    'If the freedom-preserving frame is accepted, the constraint computes as tangled_rope with genuine coordination function. If the restriction frame is accepted, the same constraint computes as snare (pure extraction in the form of licensing restriction). The reading-choice determines the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_vs_restriction_frame_contestation, conceptual, 'Whether the reciprocal obligation is a freedom-protection mechanism or a freedom-restriction mechanism—or both, depending on whose freedom is the reference point.').

omega_variable(
    commons_enclosure_empirical_rate,
    'How much proprietary capture of the free-software commons would occur without GPL reciprocity? Is the GPL''s empirical protective effect (preventing enclosure) large or small?',
    'Natural experiment from high-value permissive-licensed projects that have been captured proprietary-ly (Android/Linux, TensorFlow) versus GPL-licensed projects that have remained free. Analysis of derivative work licensing patterns over time to measure the rate of proprietary-to-open-source conversion for GPL vs. permissive derivatives.',
    'If proprietary capture of permissive-licensed code is widespread and severe, the GPL''s protective effect is empirically real and substantial, supporting the freedom-as-protection frame. If permissive-licensed projects remain largely free despite permissive licensing, the GPL''s protective effect is smaller and the restriction frame gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_enclosure_empirical_rate, empirical, 'Empirical magnitude of proprietary enclosure prevented by GPL reciprocity.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (copyright law enforces the reciprocal obligation, integrators face external legal barriers) or internalized (proprietary integrators have adopted permissive-licensing ideology that makes GPL integration unthinkable, independent of legal risk)?',
    'Post-integration behavioral test: if GPL enforcement were removed but copyright law remained, would proprietary integrators continue to release GPL derivatives, or would they lock them proprietary-ly? If enforcement were weakened, what happens to derivative licensing patterns?',
    'If suppression is purely structural, the constraint''s persistence depends on active enforcement machinery (legal threat, community surveillance). If internalized, proprietary integrators carry the suppression ideology even after exit, affecting their future licensing choices and suggesting deeper norm adoption. Most likely: mixed structural and internalized, with the proportion shifting over time as GPL becomes more institutionalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is enforced externally or internalized as ideology.').

omega_variable(
    reading_kernel_philosophical_grounding,
    'What is the kernel''s true philosophical grounding—individual liberty (downstream-user freedom to inspect and modify), collective stewardship (commons preservation as an institutional good), or something else—and does that grounding privilege one reading over others?',
    'This question is unresolvable by empirical test. It requires engagement with the GPL''s original design documents (Stallman''s Free Software Definition), subsequent legal opinions, and philosophical analysis of software freedom. The grounding is normative, not empirical.',
    'The copyleft_as_freedom_reading assumes individual liberty as the grounding (downstream users'' freedom to inspect and modify is the primary good). A commons_reading assumes institutional stewardship (the commons as a self-perpetuating entity is the primary good). A restriction_reading prioritizes proprietary innovation freedom as the reference point. Different groundings produce different classifications without contradiction—they are alternative coherent readings of the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_philosophical_grounding, preference, 'The GPL kernel''s foundational philosophical grounding, which reading it privileged, and whether that is a matter of objective fact or normative choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t4, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t4, observed).
narrative_ontology:measurement(gpl__tr_t8, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(gpl__tr_t8, observed).
narrative_ontology:measurement(gpl__tr_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement_basis(gpl__tr_t12, observed).
narrative_ontology:measurement(gpl__tr_t16, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement_basis(gpl__tr_t16, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t24, observed).
narrative_ontology:measurement(gpl__tr_t28, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 28, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t28, observed).
narrative_ontology:measurement(gpl__tr_t32, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t4, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 4, 0.18).
narrative_ontology:measurement_basis(gpl__be_t4, observed).
narrative_ontology:measurement(gpl__be_t8, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement_basis(gpl__be_t8, observed).
narrative_ontology:measurement(gpl__be_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement_basis(gpl__be_t12, observed).
narrative_ontology:measurement(gpl__be_t16, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(gpl__be_t16, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 24, 0.28).
narrative_ontology:measurement_basis(gpl__be_t24, observed).
narrative_ontology:measurement(gpl__be_t28, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 28, 0.28).
narrative_ontology:measurement_basis(gpl__be_t28, observed).
narrative_ontology:measurement(gpl__be_t32, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement_basis(gpl__be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t4, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(gpl__su_t4, observed).
narrative_ontology:measurement(gpl__su_t8, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement_basis(gpl__su_t8, observed).
narrative_ontology:measurement(gpl__su_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(gpl__su_t12, observed).
narrative_ontology:measurement(gpl__su_t16, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(gpl__su_t16, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(gpl__su_t24, observed).
narrative_ontology:measurement(gpl__su_t28, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 28, 0.72).
narrative_ontology:measurement_basis(gpl__su_t28, observed).
narrative_ontology:measurement(gpl__su_t32, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement_basis(gpl__su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% The GPL reciprocal obligation (the kernel) decomposes into three structurally distinct constraint stories sharing the same referent (GPLv3 text) but assigning different beneficiary/victim structures and frames. The copyleft_as_freedom_reading instantiates the constraint through a lens of individual downstream-user freedoms: beneficiary = downstream users (whose freedom to inspect/modify is guaranteed), victim = proprietary integrators (unable to integrate without reciprocal obligation). The copyleft_as_restriction_reading instantiates the same constraint through a lens of proprietary-model restriction: beneficiary = GPL authors (maintaining control over derivatives), victim = proprietary integrators (restricted from proprietary integration). The copyleft_as_commons_reading instantiates it through institutional commons preservation: beneficiary = the free-software commons as an entity, victim = enclosers (those seeking to lock away commons-derived improvements). All three readings have identical ε-reference (the standing GPLv3 reciprocal obligation, assessed by each reading's own lights) but different beneficiary/victim declarations and thus different per-seat classifications. The three readings are linked via network.affects_constraints to enable comparison of how framing shapes classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
