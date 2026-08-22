% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Clause 39 — Universal Due Process Reading
 *   domain: constitutional/legal_history/political_theory
 *
 * SUMMARY:
 *   Under the universal rights reading, Clause 39 of the 1215 charter is not
 *   a feudal contract term but a transhistorical emission: 'free men' denotes
 *   all persons, and the clause binds every exercise of state power over
 *   individuals in every era — no imprisonment, dispossession, outlawry,
 *   exile, or destruction except by lawful judgment. The standing arrangement
 *   this story measures is that universal due process guarantee as it has
 *   actually operated across eight centuries: judicial review of detention,
 *   habeas corpus, fair-trial requirements, and the professional apparatus
 *   that maintains them. KEY AGENTS (by structural relationship): -
 *   crown_executive_authority: Primary target (institutional/identity_locked)
 *   — bears the guarantee's taking of discretionary power while administering
 *   the courts that enforce it - persons_facing_state_detention: Primary
 *   beneficiary (powerless/trapped) — the universalized protected class -
 *   criminal_defendants: Beneficiary with residual costs (moderate/trapped) —
 *   shelter scaled by resources - judiciary: Enforcing beneficiary
 *   (institutional/identity_locked) — administers the guarantee it embodies -
 *   legal_profession: Secondary beneficiary (organized/constrained) —
 *   collects fees through maintenance of the machinery - crime_victims:
 *   Cost-bearing bystander (powerless/trapped) — absorbs delay from outside
 *   the design conversation - legislature: Amendment pathway
 *   (institutional/constrained) — expands or erodes the guarantee by statute
 *   - constitutional_historians: Analytical observer — traces the reading's
 *   transmission
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.32).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.3).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Clause 39 — Universal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '9629e7c2-33df-47f9-bb65-a49e03418462').
narrative_ontology:cs_kernel_codification('9629e7c2-33df-47f9-bb65-a49e03418462', fixed_text).
narrative_ontology:cs_authority_grounding('9629e7c2-33df-47f9-bb65-a49e03418462', lineage).
narrative_ontology:cs_interpretation_layer_present('9629e7c2-33df-47f9-bb65-a49e03418462').
narrative_ontology:cs_reading_relation('9629e7c2-33df-47f9-bb65-a49e03418462', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('9629e7c2-33df-47f9-bb65-a49e03418462', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('9629e7c2-33df-47f9-bb65-a49e03418462', foundational, universal_entitlement_to_lawful_judgment).
narrative_ontology:cs_axiom_status(universal_entitlement_to_lawful_judgment, holdable).
narrative_ontology:cs_axiom_grounding('9629e7c2-33df-47f9-bb65-a49e03418462', universal_entitlement_to_lawful_judgment, deontological).
narrative_ontology:cs_axiom('9629e7c2-33df-47f9-bb65-a49e03418462', foundational, clause39_transhistorical_binding_force).
narrative_ontology:cs_axiom_status(clause39_transhistorical_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('9629e7c2-33df-47f9-bb65-a49e03418462', clause39_transhistorical_binding_force, conventional).
narrative_ontology:cs_reference_frame('9629e7c2-33df-47f9-bb65-a49e03418462', charter_as_transhistorical_lawful_judgment_guarantee).
narrative_ontology:cs_drift_state('9629e7c2-33df-47f9-bb65-a49e03418462', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9629e7c2-33df-47f9-bb65-a49e03418462', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, persons_facing_state_detention).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, criminal_defendants).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, legal_profession).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, judiciary).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, crown_executive_authority).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, crime_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the power the clause restrains: imprisons, prosecutes, pardons, and commands the machinery that adjudicates. Administers the courts that enforce the guarantee against its own conduct — appointing judges, funding the system, litigating as a party. Repeatedly seeks exception routes in emergencies (suspension, internment, derogation) and is turned back by the judiciary it appointed. It cannot leave its own legal order without dissolving the legitimacy that constitutes it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, crown_executive_authority, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, crown_executive_authority, payer).

% Anyone the state moves to imprison, remove, commit, or punish. The guarantee is theirs without purchase or election — they cannot decline it and cannot leave the jurisdiction's reach while subject to its power. For them the clause is the difference between a hearing and a cell; they rely on it most at the moments they have least capacity to invoke it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, persons_facing_state_detention, beneficiary,
    powerless, biographical, trapped, national).

% Formal holders of the guarantee in its busiest venue. The protection's quality tracks resources: well-funded defendants convert procedure into advantage and delay; indigent ones receive compressed versions through overworked counsel. They bear the guarantee's time and expense alongside its shelter.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, criminal_defendants, beneficiary,
    moderate, biographical, trapped, national).

% Administers and enforces the guarantee: issues writs, quashes unlawful detention, strikes executive excess. Gains jurisdiction, independence, and public standing from the role; the guarantee is the core of judicial identity and the source of the courts' leverage over the other branches. Judges are appointed by the power they restrain and removed by processes they oversee.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, judiciary, beneficiary).

% Practices inside the guarantee: argues motions, challenges detentions, builds the doctrinal detail that keeps the clause operative. Fee income scales with procedural complexity, and the profession's market existence presupposes the adjudicative machinery the guarantee requires. Its advocacy is the guarantee's daily maintenance crew.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Bear the guarantee's costs from outside its design conversation: investigation, trial, appeal, and collateral attack stretch years past the injury, and some cases collapse on procedural defect. They hold no seat in setting the procedure whose delays they absorb, and their preference for swift certain punishment is structurally outvoted by the architecture of judgment.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, crime_victims, payer,
    powerless, biographical, trapped, local).

% Writes the statutes that implement, amend, or erode the guarantee: habeas reform, detention statutes, emergency powers acts, derogation instruments. Historically the site of both expansion (Petition of Right, Habeas Corpus Acts) and suspension. Its products are subject to judicial review, bounding how far it can reshape the guarantee's reach.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% Study the clause's transmission: what 'liber homo' denoted in 1215, how Coke universalized it, which invocations were ceremonial and which operative. Hold no stake in the guarantee's operation beyond scholarly reputation; their findings feed both the universal reading and its challengers.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__universal_rights_reading, legal_profession).
narrative_ontology:fixing_cost_class(magna_carta_1215__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts the problem of restraining state violence from a standing power contest into a rule-governed procedure: every person subject to detention or punishment can invoke a known adjudicative path, and officials internalize that deprivations require judgment first. It also coordinates elite self-protection — today's rulers are tomorrow's detained — giving even the powerful a stake in the shield.
% TRANSFER_FUNCTION: Moves discretionary power from executives to adjudicative bodies; moves fees, salaries, and time from litigants and taxpayers to courts and the legal profession; moves liberty-risk from persons onto procedure, with delay and formality as the price paid for insulation from arbitrary will.
% ABSENT_VOICES: Those who bear the guarantee's costs without shaping it: crime victims who would trade process speed for certainty of punishment; executives facing emergencies who would suspend judgment for dispatch; and, historically, the colonized and enslaved, excluded from 'free men' while the charter was cited as proof of civilized restraint. Their objection — that process is itself the punishment, or that the shield was never meant for them — sits outside the courtroom that administers the promise.
% DISAPPEARANCE_RATIONALE: Detention practice would drift toward executive convenience within a generation: the historical record of habeas suspensions, wartime internments, and emergency decrees shows what fills the vacuum whenever enforcement slackens. Courts, plea bargaining, and the professional bar would lose their organizing spine, and the expectation that power answers to judgment — load-bearing for liberal order — would need rebuilding from scratch.
% FOUNDING_PROBLEM: King John's arbitrary disseisin, imprisonment without judgment, and destruction of persons and lands at unaccountable will — a grievance first articulated by barons protecting their estates, which this reading generalizes into a universal shield: no free person shall be imprisoned, dispossessed, outlawed, exiled, or destroyed except by lawful judgment of peers and the law of the land.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the UN Working Group on Arbitrary Detention and Amnesty International document tens of thousands of arbitrary-detention cases annually, attesting that the problem the guarantee answers remains live; domestic records show executives repeatedly attempting process-bypass (habeas suspensions, internment orders, emergency decrees) and courts blocking them — the guarantee's continuing work is visible in the attempts it defeats. Legal-profession attestations are discounted as self-interested.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.32): the guarantee's dominant flow is protective, but real costs accumulate — procedural delay, litigation expense, and fee income that scales with complexity and concentrates in the profession. Suppression (0.30) reflects enforcement that is real but not participant-coercive: courts actively block executive evasion, yet citizens are not held in the arrangement against their will, and the alternatives closed off are chiefly the executive's arbitrary shortcuts. Theater (0.28) is meaningful but minority: ceremonial invocations of the charter, symbolic citations by officials eroding process, and pro-forma hearings coexist with a blocking function that visibly operates. Accessibility collapse (0.48) is partial — within liberal-legal frameworks the arbitrary-detention alternative is largely closed, but emergency derogations, military commissions, and administrative-detention regimes keep alternatives alive at the margins. Resistance (0.58) is sustained and structural: eight centuries of executive attempts to route around judgment, from clause-61 collapse through habeas suspensions to modern derogation instruments. The claimed type is rope — my authored belief that this is a genuine coordination mechanism whose participants are net beneficiaries — while the declared victims and active enforcement give the engine grounds to compute tangled_rope; that divergence, if it computes, is the datum, not an error. The measurement series run on one shared eight-point grid so every tracked metric is authored at every examined time point. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine's directionality and scope arithmetic.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the crown's position the guarantee is expropriation of prerogative — a constraint it must administer against itself, endured because its legitimacy is constituted by the legal order it bounds; its exit is identity-locked, since a sovereign that leaves its own legality dissolves the authority it would retreat into. From the detainee's position the same structure is a shield that arrives without purchase. The judiciary's seat fuses vocation and interest: enforcing lawful judgment is both its duty and the source of its standing, an identity lock that makes the guarantee self-maintaining so long as courts retain independence. Crime victims experience the arrangement as cost without voice — the architecture of judgment outvotes their preference for swiftness. Same-power divergence appears between criminal_defendants (moderate) and persons_facing_state_detention (powerless): nominally equal beneficiaries whose realized protection differs with resources, which is the formal-versus-substantive universality question carried in the omegas.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place persons_facing_state_detention and criminal_defendants near the subsidy end (d low), with defendants slightly less subsidized because they bear procedural time and expense directly. The judiciary and legal_profession derive low d as declared beneficiaries — the guarantee subsidizes jurisdiction, standing, and fee income. The victim declarations place crown_executive_authority near the full-target end (d high): the guarantee's entire operation consists of taking discretion from it, and its identity-locked exit removes the damping that mobility would provide. Crime_victims derive high d from their victim declaration, though their true position is mid-range — they also gain from convictions legitimated by process. No directionality_overrides are authored: the override mechanism keys on power atoms, and crime_victims share the powerless atom with persons_facing_state_detention, so any correction aimed at the victims' mid-range position would corrupt the detainee seat's correctly low d. The residual imprecision is accepted and documented here rather than patched with a blunt instrument.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is indexed to this reading: generalized arbitrary state power, which remains live — corroborated externally by documented detention abuses worldwide and by the executive bypass attempts courts defeat domestically. The charter's original enforcement machinery (the clause-61 committee of twenty-five barons) is long dead, but it was replaced rather than mourned: judicial enforcement succeeded baronial enforcement, so the mandate transformed instead of atrophying, and mandatrophy_resolved is authored false. The classification discipline guards against both symmetrical errors: reading the guarantee as pure coordination misses the professional rent layer and the wealth-stratified access (tangled-rope pressure the engine can register from the declared victims); reading it as extraction misses that the protected class includes the guarantee's own critics, that its costs fall chiefly on the powerful, and that its disappearance would be filled by exactly the arbitrary practices it forbids. The rising theater_ratio series tracks the growing share of ceremonial invocation — Whig mythology, political citation amid erosion — which is the observable symptom if the guarantee ever begins sliding from maintained function toward maintained performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Within the magna_carta_1215 kernel, does ''liber homo'' in Clause 39 denote all persons (this reading) or the contracting landholding barons (baronial_privilege_reading)?',
    'Philological analysis of 1215 usage, the reception history of the 1225 and 1297 confirmations, and the earliest judicial glosses; whichever extension is adopted determines this constraint''s beneficiary and victim sets wholesale.',
    'If the baronial reading is adopted, this constraint''s universal beneficiary set collapses to a medieval elite, the universal due process emission never existed as authored, and the story reclassifies around a dead contractual term rather than a living guarantee.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the liber homo extension governs the shared kernel.').

omega_variable(
    transhistorical_emission_vs_reception,
    'Does the universal due process constraint actually emanate from the 1215 clause itself, or from later doctrine (Coke''s glosses, the Petition of Right, bills of rights, human-rights instruments) that merely cites the charter as ancestor?',
    'Doctrinal genealogy tracing each operative element of modern due process to its first authoritative statement; elements first appearing after 1215 are receptions, not emissions.',
    'If reception dominates, this story''s epsilon-referent shifts from the charter clause to the interpretive tradition — making the universal reading partially parasitic on the living_document_reading''s machinery and weakening its claim to transhistorical binding force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transhistorical_emission_vs_reception, conceptual, 'Whether the constraint is an emission of the 1215 text or a product of its reception.').

omega_variable(
    coverage_extraction_coupling,
    'As the guarantee''s coverage universalized, did per-capita cost fall (protection diluting professional rents) or rise (professionalization concentrating fee income and procedural burden)?',
    'Historical comparison of procedural cost burden and legal-market concentration across the interval''s endpoints.',
    'Rising per-capita cost under universal coverage would push the computed classification toward tangled_rope despite the protective core; falling cost supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_extraction_coupling, empirical, 'How universalization interacted with the guarantee''s cost structure.').

omega_variable(
    emergency_derogation_status,
    'Are emergency mechanisms (habeas suspension, internment, treaty derogation clauses) the guarantee''s designed relief valve or its erosion channel?',
    'Comparative analysis of emergency episodes: whether process protections restored fully after each crisis or ratcheted downward.',
    'If an erosion channel, effective suppression exceeds the measured scalar at crisis frequency, and persistence depends on inter-crisis intervals rather than structural stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_derogation_status, empirical, 'Whether emergency powers are safety valve or decay path.').

omega_variable(
    formal_vs_substantive_universality,
    'Is the guarantee universal in substance, or universal in form while access scales with wealth?',
    'Outcome and resource-disparity studies across defendant wealth strata within jurisdictions operating the guarantee.',
    'Substantive stratification would raise the burden on low-resource defendants above the aggregate measure and complicate the universal beneficiary declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_substantive_universality, empirical, 'Whether universality holds in substance or only in form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 0, 810).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__universal_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magn_tr_t120, magna_carta_1215__universal_rights_reading, theater_ratio, 120, 0.12).
narrative_ontology:measurement(magn_tr_t260, magna_carta_1215__universal_rights_reading, theater_ratio, 260, 0.14).
narrative_ontology:measurement(magn_tr_t400, magna_carta_1215__universal_rights_reading, theater_ratio, 400, 0.17).
narrative_ontology:measurement(magn_tr_t520, magna_carta_1215__universal_rights_reading, theater_ratio, 520, 0.2).
narrative_ontology:measurement(magn_tr_t640, magna_carta_1215__universal_rights_reading, theater_ratio, 640, 0.23).
narrative_ontology:measurement(magn_tr_t740, magna_carta_1215__universal_rights_reading, theater_ratio, 740, 0.26).
narrative_ontology:measurement(magn_tr_t810, magna_carta_1215__universal_rights_reading, theater_ratio, 810, 0.28).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__universal_rights_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(magn_be_t120, magna_carta_1215__universal_rights_reading, base_extractiveness, 120, 0.19).
narrative_ontology:measurement(magn_be_t260, magna_carta_1215__universal_rights_reading, base_extractiveness, 260, 0.21).
narrative_ontology:measurement(magn_be_t400, magna_carta_1215__universal_rights_reading, base_extractiveness, 400, 0.25).
narrative_ontology:measurement(magn_be_t520, magna_carta_1215__universal_rights_reading, base_extractiveness, 520, 0.27).
narrative_ontology:measurement(magn_be_t640, magna_carta_1215__universal_rights_reading, base_extractiveness, 640, 0.29).
narrative_ontology:measurement(magn_be_t740, magna_carta_1215__universal_rights_reading, base_extractiveness, 740, 0.31).
narrative_ontology:measurement(magn_be_t810, magna_carta_1215__universal_rights_reading, base_extractiveness, 810, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_1215__universal_rights_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(magn_su_t120, magna_carta_1215__universal_rights_reading, suppression_requirement, 120, 0.3).
narrative_ontology:measurement(magn_su_t260, magna_carta_1215__universal_rights_reading, suppression_requirement, 260, 0.27).
narrative_ontology:measurement(magn_su_t400, magna_carta_1215__universal_rights_reading, suppression_requirement, 400, 0.32).
narrative_ontology:measurement(magn_su_t520, magna_carta_1215__universal_rights_reading, suppression_requirement, 520, 0.29).
narrative_ontology:measurement(magn_su_t640, magna_carta_1215__universal_rights_reading, suppression_requirement, 640, 0.26).
narrative_ontology:measurement(magn_su_t740, magna_carta_1215__universal_rights_reading, suppression_requirement, 740, 0.28).
narrative_ontology:measurement(magn_su_t810, magna_carta_1215__universal_rights_reading, suppression_requirement, 810, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, living_document_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Magna Carta' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel: baronial_privilege_reading (contract among landholding elites; negligible modern operation), living_document_reading (adaptive constitutional substrate; extraction located in interpretive authority), and this universal_rights_reading (universal due process emission; protective core with a professional-rent overlay). Their epsilon values diverge widely because their beneficiary and victim sets diverge: the baronial reading protects a dead elite, the living-document reading subsidizes interpretive institutions, and this reading shields all persons while taking discretion from executives. This file links both siblings; the baronial reading is the upstream-historical term, and the living-document reading supplies the interpretive machinery this reading's universality rides on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
