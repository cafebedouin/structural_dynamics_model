% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Software Control as Pragmatic Methodology Choice (Open Source Coordination Reading)
 *   domain: software engineering / political economy of technology
 *
 * SUMMARY:
 *   This constraint instantiates the pragmatic-openness reading of the
 *   software-control-legitimacy kernel. Under this reading, software control
 *   (open vs. proprietary) is a development methodology choice, not a
 *   question of freedom, property rights, or commons governance. Open-source
 *   development produces high-quality software through peer review and
 *   distributed collaboration; proprietary models are also legitimate
 *   alternatives that enable focused investment and specialized support. No
 *   party is a victim; the arrangement benefits developers (choice of
 *   methodology), users (access to multiple high-quality solutions), and
 *   enterprises (optionality across the spectrum). The constraint's function
 *   is coordination—normalizing the coexistence of multiple development
 *   models as acceptable within a single software ecosystem. Low
 *   extractiveness reflects that no party must pay for the privilege of
 *   choosing their preferred model; the difference between this reading and
 *   its siblings is not metric-based (the other readings would author
 *   different ε values for the SAME structural situation), but rather which
 *   structural situation is being evaluated. This reading treats 'coexistence
 *   of models' as the referent; the freedom-imperative reading treats
 *   'proprietary software existence' as the referent and would author higher
 *   ε for that target.
 *
 * KEY AGENTS:
 *   - open_source_developers — participate in voluntary peer-review and community-driven development; benefit from distributed feedback and reputation-building
 *   - software_users — choose between open-source and proprietary software based on technical needs and risk tolerance; not locked into either model
 *   - enterprises — adopt both models in their infrastructure; use optionality as negotiating leverage with vendors and as risk management
 *   - proprietary_software_vendors — develop under licensing models; legitimate business model within this reading's frame
 *   - academic_and_research_communities — contribute to open-source; use both proprietary and open-source tools
 *   - intellectual_property_authorities — enforce law that permits both models; external arbiters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.28).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.15).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Software Control as Pragmatic Methodology Choice (Open Source Coordination Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software engineering / political economy of technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'b9937b8a-db23-4016-814b-46fb8fe082ed').
narrative_ontology:cs_kernel_codification('b9937b8a-db23-4016-814b-46fb8fe082ed', distributed).
narrative_ontology:cs_authority_grounding('b9937b8a-db23-4016-814b-46fb8fe082ed', distributed).
narrative_ontology:cs_reading_relation('b9937b8a-db23-4016-814b-46fb8fe082ed', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9937b8a-db23-4016-814b-46fb8fe082ed', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9937b8a-db23-4016-814b-46fb8fe082ed', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('b9937b8a-db23-4016-814b-46fb8fe082ed', foundational, contextual_methodology_legitimacy).
narrative_ontology:cs_axiom_status(contextual_methodology_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b9937b8a-db23-4016-814b-46fb8fe082ed', contextual_methodology_legitimacy, instrumental).
narrative_ontology:cs_axiom('b9937b8a-db23-4016-814b-46fb8fe082ed', foundational, coexistence_as_coordination_frame).
narrative_ontology:cs_axiom_status(coexistence_as_coordination_frame, holdable).
narrative_ontology:cs_axiom_grounding('b9937b8a-db23-4016-814b-46fb8fe082ed', coexistence_as_coordination_frame, instrumental).
narrative_ontology:cs_reference_frame('b9937b8a-db23-4016-814b-46fb8fe082ed', multiple_legitimate_control_models).
narrative_ontology:cs_drift_state('b9937b8a-db23-4016-814b-46fb8fe082ed', contemporary_software_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b9937b8a-db23-4016-814b-46fb8fe082ed', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, enterprises_adopting_oss).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, academic_and_research_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in open-source projects where code is reviewed, iterated, and shared. They benefit from distributed peer review, rapid feedback, and reputation building within communities. They can fork, contribute to competing projects, or switch to proprietary development; their participation is voluntary and driven by technical interest and professional credibility.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_developers, beneficiary,
    moderate, biographical, mobile, global).

% Access software—open-source and proprietary—and benefit from both models. Open-source users gain auditability, customization, and community support; proprietary-software users gain integrated development, vendor support, and simplified deployment. They choose based on their technical needs and risk profile; no structural lock-in forces them into one model.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Deploy open-source software to reduce licensing costs, avoid vendor lock-in, and access transparent code for compliance and security auditing. They also maintain proprietary software where competitive advantage or integration complexity justifies it. Their ability to mix models gives them optionality and negotiating leverage with vendors.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, enterprises_adopting_oss, beneficiary,
    powerful, generational, arbitrage, global).

% Develop and distribute software under restrictive licenses, funding development through licensing fees and support contracts. They invest in user experience, integrated platforms, and long-term maintenance. Their business model is legitimate within this reading's framework; their software coexists with open-source alternatives in a competitive landscape.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Contribute to open-source software development (publishing algorithms, libraries, tools) while also using both proprietary and open-source tools for research. They benefit from the transparency and reproducibility that open-source enables, and from the advanced proprietary tools commercial vendors provide for specialized domains.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, academic_and_research_communities, beneficiary,
    moderate, biographical, mobile, global).

% Advocates for absolutist positions (software must be free, or software ownership is sacred) are not centered in this reading's framing. This reading treats their positions as live alternatives rather than as legitimate stakeholders in the pragmatic-coexistence conversation. They would argue for categorical rules; they are excluded from the negotiation table where methodology choices are treated as domain-specific rather than universal.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, competing_development_models, excluded,
    powerful, biographical, constrained, global).

% Enforce copyright, patent, and trade secret law in ways that permit both open-source and proprietary models. They observe the constraint's operation and can intervene (e.g., via patent reform, copyleft license enforcement, anti-circumvention rulings) but are positioned as external arbiters, not direct participants.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, intellectual_property_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of how software development and distribution can proceed when creators have different preferences about code visibility, modification rights, and commercialization. Open-source solves this by creating transparent, community-driven development paths; proprietary models solve it by centralizing control and funding development through licensing. Both are legitimate solutions to the core coordination problem: how to build and deploy software sustainably.
% TRANSFER_FUNCTION: Under open-source licensing, volunteer effort and community labor flow into software development, which users and enterprises access freely (time and attention cost, community obligation). Under proprietary licensing, payment flows from users and enterprises to vendors in exchange for code, support, and integrated platforms. This reading accepts both transfer structures as legitimate.
% ABSENT_VOICES: Absolutist freedom advocates (those who deny proprietary software any legitimacy) and absolutist property-rights advocates (those who deny open-source commons any legitimacy) are structurally excluded from this reading's table. They would reject the premise that both models are coequal alternatives and would argue for categorical prohibition of one or the other.
% DISAPPEARANCE_RATIONALE: If this reading (pragmatic coexistence of models) disappeared and one of the sibling readings (freedom imperative, property rights absolute, or commons mandate) became hegemonic, software development would reorganize: either all software would be open-source (under freedom or commons readings), or all would be proprietary (under property-rights reading). The current mixed landscape depends on this reading's acceptance of multiple legitimate models. If it vanished, the distribution of software models would change drastically; whether the world as a whole rearranges depends on which sibling reading replaces it.
% FOUNDING_PROBLEM: Software development requires coordination around how code is shared, modified, and distributed. Early computing had no established norms; property-law default, copyright automaticity, and the rise of open-source communities created a fragmented landscape. The founding problem was: can a single legitimacy frame unify this diversity, or must multiple coexisting frames be accepted?
% FOUNDING_PROBLEM_CORROBORATION: The pragmatic-coexistence answer is attested by: active participation of enterprises in both models (Linus Torvalds and RedHat on open-source; Microsoft and Google on both; Apple on proprietary+selective-open-source); academic literature on comparative software development productivity (both models produce high-quality software under different conditions); and user choice data (different domains and risk profiles favor different models). The founding problem remains live because absolutist positions on either side continue to argue for categorical resolution rather than coexistence.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, contested).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 at interval end) because the constraint's function—normalizing coexistence of development models—does not require one party to pay or lose access. Both models flourish; users choose based on fit. Suppression is very low (0.15) because the arrangement does not suppress alternatives; it enables them. Theater is minimal (0.08) because the coordination function is genuine and not performative; the constraint does not require performative maintenance. Accessibility collapse (0.35) reflects that alternatives are always visible—developers and users can always see that other models exist and can switch; the choice is available but may be bounded by technical fit or existing dependencies, not by deliberate suppression. Resistance (0.42) is moderate: some absolutist advocates resist the pragmatic frame, but the constraint persists because the majority of practitioners benefit from optionality rather than categorical rules. The measurement series shows extractiveness remaining nearly flat (slight drift upward, projected to stabilize), indicating the constraint's fundamental character is stable—no accumulation of hidden extraction, no shift toward hidden rent-seeking. Theater ratio similarly flat: no increase in performative maintenance, no sign of underlying function decay.
 *
 * PERSPECTIVAL GAP:
 *   From the open-source-developer seat, the constraint is genuine coordination—a framework that legitimizes community-driven development and attracts peer review. From the proprietary-vendor seat, it is also coordination—a framework that legitimizes investment-backed development and specialized support. From the software-user seat, it is pure benefit: choice without coercion. From the absolutist-freedom seat (excluded), the constraint would read as illegitimate cover for proprietary extraction. From the absolutist-property seat (excluded), it would read as illegitimate threat to ownership rights. The engine should compute the same constraint type (rope or light-extraction) from the included seats' structural data; the excluded seats' perspectives are not fed to the directionality derivation. The committer frame (this reading vs. its siblings) explains the excluded positions: the sibling readings would foreclose or heavily pressure this one if they became hegemonic.
 *
 * DIRECTIONALITY LOGIC:
 *   No victim set: the beneficiaries (open-source developers, users, enterprises, proprietary vendors) all genuinely benefit from the normalization of multiple coexisting models. Open-source developers gain legitimacy and access to distributed review. Proprietary vendors gain legitimacy to pursue their business model. Users gain optionality. Enterprises gain leverage and risk distribution. The absence of victims is the structural signature of this reading: it treats the coordination problem as symmetric rather than extractive. Directionality should compute near 0.5 (symmetric) or slightly toward beneficiary side for all named seats because all benefit from the choice-enabling frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to coordinate software development under diverse control preferences) remains live. The pragmatic-coexistence reading answers it by normalizing multiple models rather than mandating one. This prevents mandatrophy by refusing to declare the founding problem solved—it remains a live question that different actors answer differently through their methodology choices. The constraint persists because practitioners find value in not resolving the question categorically. If extractiveness increased sharply (indicating one model was suppressing the others) or if theater ratio spiked (indicating the coexistence frame was becoming performative cover for actual dominance), mandatrophy would appear. The flat metrics indicate genuine coordination function, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    model_coexistence_stability,
    'Can pragmatic coexistence of open-source and proprietary models remain stable long-term, or will one model eventually achieve dominance?',
    'Longitudinal data on market share, enterprise adoption ratios, and new software-project license choices over 15+ years. Drift in licensing distribution would signal pressure toward one model; stability would support the coexistence hypothesis.',
    'If one model is structurally dominant (e.g., open-source software captures 70%+ of deployed systems), the pragmatic-coexistence reading may be describing an aspirational frame that actual practice has already displaced. If stability persists across domains, the reading is descriptively accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_coexistence_stability, empirical, 'Whether pragmatic coexistence is structurally sustainable or historically transient.').

omega_variable(
    committer_frame_boundary,
    'Is the distinction between this reading (pragmatic-coexistence) and the commons-reading (negotiated collective management) a genuine structural difference or a difference of emphasis on the same underlying arrangement?',
    'Comparative analysis of policy recommendations each reading would endorse: the pragmatic reading accepts unilateral proprietary choices by individuals; the commons reading would impose collective negotiation constraints on those choices. Empirical case studies of disputes over software governance would reveal whether the readings diverge in practice.',
    'If the readings often recommend the same policies, they may be near-synonymous and the distinction is rhetorical rather than structural. If they diverge on specific governance questions (e.g., license interoperability, monopoly remedies), they are genuinely distinct constraint readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_boundary, conceptual, 'Whether pragmatic-openness and commons-governance are distinct constraint readings or interpretive variants of the same underlying structure.').

omega_variable(
    absolutist_pressure_dynamics,
    'Why do absolutist positions (freedom-imperative, property-rights) persist despite the pragmatic reading''s wider institutional adoption?',
    'Qualitative analysis of absolutist advocacy communities (GNU Project, Free Software Foundation, proprietarian IP advocates) and their resource bases, social composition, and influence on policy outcomes. Do they persist through institutional capture, ideological conviction, or genuine grievance with specific practices?',
    'If absolutist positions persist through institutional capture (embedded in policy, law, corporate strategy), the pragmatic reading may be a thin legitimacy frame for power imbalance. If they persist through ideological conviction with minimal policy power, the reading''s account of coexistence is more accurate. If they persist through genuine grievance (one model is suppressing the other in specific domains), the pragmatic reading is partially descriptive of the problem but may miss important asymmetries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_pressure_dynamics, empirical, 'Why absolutist framings persist despite pragmatic reading''s wider adoption.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Does the pragmatic-openness axiom (contextual-methodology-legitimacy) genuinely preclude the freedom-imperative and property-rights axioms, or are all three holdable within one framework?',
    'Logical analysis: can a developer simultaneously believe that (1) all software should be free (freedom imperative), AND (2) developers have legitimate authority to restrict use (property rights), AND (3) methodology choice is contextual (pragmatic openness)? If the axioms are logically compatible, none forecloses the others; if one is logically negated by the others, there is a genuine foreclosure relation.',
    'If axioms are compatible, the readings coexist within a single broader framework and the relation is ''coexists_with''. If one axiom is logically negated (e.g., ''legitimate authority to restrict'' directly contradicts ''all software should be free''), the stronger axiom forecloses the weaker one''s coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Logical compatibility of the pragmatic reading''s axioms with those of sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(soft_tr_t20, projected).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement_basis(soft_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(soft_be_t20, projected).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(soft_be_t25, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__pragmatic_openness_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__pragmatic_openness_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% The software_control_legitimacy kernel has four constraint stories corresponding to four readings: pragmatic_openness_reading (this story), freedom_imperative_reading (software must be free), property_rights_reading (creators own their code), and commons_reading (software is collective infrastructure). Each reading instantiates a different constraint because each selects a different structural referent and authority frame. The pragmatic reading treats coexistence-of-models as the referent (low ε); the freedom-imperative reading treats proprietary-software-existence as the referent (high ε); the property-rights reading treats commons-restrictions as the referent (high ε); the commons-reading treats unilateral-control-without-collective-negotiation as the referent (high ε). The family link is epistemic: the pragmatic reading INFLUENCES all three absolutist readings by creating institutional pressure toward coexistence, but does not foreclose them—they remain live alternatives held by different parties. Each sibling reading would influence this one if it became hegemonic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
