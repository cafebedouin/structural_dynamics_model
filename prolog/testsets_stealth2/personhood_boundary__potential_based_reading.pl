% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Potential-Based Personhood Boundary (Rational-Agency Reading)
 *   domain: moral philosophy / bioethics / commitment systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the personhood_boundary kernel:
 *   the potential-based reading, on which moral standing attaches to entities
 *   with potential for rational agency, and severely disabled infants may
 *   therefore fall outside it. The standing arrangement under contest is the
 *   operating boundary itself: a criterion administered by clinical-ethics
 *   institutions, exercised as delegated guardian discretion, and securing
 *   unconditional standing for everyone above the capacity line. Its
 *   extraction falls on a class that cannot speak (the excluded infants) and,
 *   by devaluation externality, on the wider disabled community; its
 *   coordination function is real, because every legal system needs a
 *   determinate standing line. Constraint family note: the colloquial label
 *   'when does human life acquire moral standing' decomposes into three
 *   linked stories with different victim sets and different epsilon values.
 *   The birth-threshold reading (all born humans have standing) has an
 *   essentially empty victim set and negligible extraction; the
 *   fitness-contingent reading (standing requires demonstrated fitness) has a
 *   larger victim set than this one, since pre-fitness healthy infants are
 *   also excluded; this potential-based reading sits between them, excluding
 *   fewer than fitness-contingent but far more than birth-threshold. Each is
 *   authored as its own file with its own epsilon; they are linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   severely_disabled_infants: Primary target (powerless/trapped) — bears the
 *   full withdrawal of standing - parental_guardians: Primary beneficiary
 *   with dual cost-bearing (moderate/constrained) — holds delegated exclusion
 *   judgment - medical_ethics_committees: Agenda-setter
 *   (institutional/identity_locked) — operationalizes the criterion into
 *   protocol - able_bodied_persons: Structural beneficiary (organized/mobile)
 *   — standing secured at zero cost - disabled_rights_advocates: Secondary
 *   target via devaluation externality (organized/constrained) -
 *   sanctity_of_life_traditions: Excluded objector (organized/trapped) —
 *   barred from operative deliberation - constitutional_courts: Analytical
 *   observer (institutional/analytical) — sees the full structure
 *
 * KEY AGENTS:
 *   - severely_disabled_infants: Primary target (powerless/trapped) — bears the full withdrawal of standing
 *   - parental_guardians: Primary beneficiary with dual cost-bearing (moderate/constrained) — holds delegated exclusion judgment
 *   - medical_ethics_committees: Agenda-setter (institutional/identity_locked) — operationalizes the criterion into protocol
 *   - able_bodied_persons: Structural beneficiary (organized/mobile) — standing secured at zero cost
 *   - disabled_rights_advocates: Secondary target via devaluation externality (organized/constrained)
 *   - sanctity_of_life_traditions: Excluded objector (organized/trapped)
 *   - constitutional_courts: Analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.72).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.7).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Potential-Based Personhood Boundary (Rational-Agency Reading)").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral philosophy / bioethics / commitment systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, '67d62807-63f6-4e70-8747-877a57b4b999').
narrative_ontology:cs_kernel_codification('67d62807-63f6-4e70-8747-877a57b4b999', distributed).
narrative_ontology:cs_authority_grounding('67d62807-63f6-4e70-8747-877a57b4b999', expertise).
narrative_ontology:cs_interpretation_layer_present('67d62807-63f6-4e70-8747-877a57b4b999').
narrative_ontology:cs_reading_relation('67d62807-63f6-4e70-8747-877a57b4b999', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('67d62807-63f6-4e70-8747-877a57b4b999', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_axiom('67d62807-63f6-4e70-8747-877a57b4b999', foundational, potential_suffices_for_moral_standing).
narrative_ontology:cs_axiom_status(potential_suffices_for_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('67d62807-63f6-4e70-8747-877a57b4b999', potential_suffices_for_moral_standing, deontological).
narrative_ontology:cs_axiom('67d62807-63f6-4e70-8747-877a57b4b999', secondary, guardian_assessment_delegation).
narrative_ontology:cs_axiom_status(guardian_assessment_delegation, holdable).
narrative_ontology:cs_axiom_grounding('67d62807-63f6-4e70-8747-877a57b4b999', guardian_assessment_delegation, conventional).
narrative_ontology:cs_reference_frame('67d62807-63f6-4e70-8747-877a57b4b999', potential_rational_agency_criterion).
narrative_ontology:cs_drift_state('67d62807-63f6-4e70-8747-877a57b4b999', contemporary_disability_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('67d62807-63f6-4e70-8747-877a57b4b999', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parental_guardians).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, able_bodied_persons).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, medical_ethics_committees).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, disabled_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, parental_guardians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Born with impairments that eliminate expected potential for rational agency. Under this reading they fall outside moral standing, so their protection reduces to the discretionary choices of guardians and clinicians rather than to a right. They cannot object, exit, or advocate: the framework defines them out of the conversation before it begins.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, biographical, trapped, local).

% Parents of newborns with profound impairments. Under this reading they hold delegated discretion, with clinician concurrence, over whether the infant's life is sustained. When exclusion is chosen they are relieved of a lifetime care burden; they carry grief either way. They cannot decline the framing itself once an infant is born into the category, and relinquishing the child does not return the judgment to neutral hands.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parental_guardians, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parental_guardians, payer).

% Hospital ethics boards and professional bodies that operationalize the criterion into working protocol: which diagnoses count as eliminating potential, what concurrence is required, what documentation protects the deciders. They collect jurisdiction, staffing, and professional purpose from administering the boundary; their guidelines are what determine which infants enter the exclusion zone. Their institutional identity has become the administration of this criterion.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, medical_ethics_committees, agenda_setter,
    institutional, generational, identity_locked, national).

% The broad class whose moral and legal standing the boundary secures unconditionally. Because they possess the relevant capacities, their membership never comes up for review; they pay nothing for the security and mostly never encounter the boundary's operation at all.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, able_bodied_persons, beneficiary,
    organized, generational, mobile, global).

% Disability movements and scholars who bear the devaluation externality of the reading: public argument that standing tracks rational-agency capacity renders their own standing rhetorically contingent. They campaign against capacity-based criteria, litigate protocol outcomes, and document cases. They cannot exit the discourse, because the criterion is applied to people like them wherever it travels.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disabled_rights_advocates, payer,
    organized, generational, constrained, continental).

% Religious and sanctity-of-life ethicists who ground standing in imago dei or sentience rather than capacity-potential. They object that the criterion manufactures a killable class of humans. They are loud in public debate but largely outside the clinical-ethics deliberations where exclusion judgments are actually made, and the professional bodies running those deliberations do not admit their premises as inputs.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, sanctity_of_life_traditions, excluded,
    organized, civilizational, trapped, global).

% Courts adjudicating whether capacity-based distinctions can carry legal consequences: wrongful-life actions, end-of-life liability, neonatal care standards. They take the philosophical criterion as input and emit binding boundaries, seeing the whole structure without collecting from its operation.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, parental_guardians).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Every legal and medical system needs a determinate line for when protective duties attach to a human organism: when killing is homicide, when treatment is obligatory, when grief is socially recognized. The potential-based reading supplies a secular, capacity-anchored criterion that does not depend on contested theological premises, allowing law, neonatology, and family life to coordinate on a single answer.
% TRANSFER_FUNCTION: Moves moral and legal standing away from infants lacking potential for rational agency, and moves decision authority over life and death to guardians and clinicians; simultaneously moves the security of unconditional standing to everyone above the capacity line, at no cost to them.
% ABSENT_VOICES: The excluded infants themselves would object if they could, but the framework constitutively silences them: lacking standing is precisely lacking a voice in the conversation about one's standing. Sanctity-of-life traditions and disability scholars object from outside the operative deliberation; their premises are heard in public but are not admissible inputs to the committee protocols where exclusion judgments are made.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, neonatal intensive-care protocols, guardian-discretion doctrines, wrongful-life litigation, and the protected status of nearly all born humans would lose their current grounding and would have to be re-litigated under a different criterion. Arrangements demonstrably depend on this specific line: which infants receive maximal treatment, which deaths are investigated, which judgments are shielded as tragedy rather than prosecuted as killing.
% FOUNDING_PROBLEM: The arrangement was built to solve the ancient problem of drawing the line of moral standing without appeal to contested revelation: homicide law, medical obligation, and family practice all required a criterion that secular pluralistic institutions could administer. The argument from potential offered a capacity-anchored answer that promised objectivity.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's liveness is corroborated from outside the benefiting parties: disability scholarship engages it adversarially and continuously; constitutional courts keep confronting it in wrongful-life and end-of-life dockets; sanctity-of-life traditions attest its liveness by organizing against particular answers to it. No party claims the problem is solved; they dispute only which criterion answers it.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the boundary withdraws basic protection from an entire class of sentient humans and concentrates life-and-death discretion in the seats above the line; the withdrawal is total for anyone classified out, not a marginal cost. Suppression (0.70) is structural first: the excluded cannot object because lacking standing IS lacking standing to object, and dissenting traditions are kept outside the protocol-writing rooms; part is internalized in the disabled community as devaluation carried inward (see the suppression-mechanism omega). Theater (0.40) reflects an apparatus that increasingly performs difficult-case deliberation and neutrality while outcome distributions stay stable. Accessibility collapse is LOW (0.30): alternative readings remain fully live and accessible — this is a contested normative space, not a natural law, and the sibling readings persist as usable positions. Resistance is substantial (0.62): disability movements, religious traditions, and many jurisdictions refuse the criterion's practical conclusions. Coordination type is declared identity_coordination because the constraint's dominant function is membership maintenance for the moral community — boundary upkeep and membership adjudication. FNL gaming alert: this is exactly the configuration the identity_coordination warning targets — strong Power x Scope coupling concentrating extraction on maximally powerless agents (newborns) at population scale; the complexity offset accommodates genuine boundary-maintenance complexity and must not be read as excusing the asymmetry. Coalition check: the primary victim class cannot self-organize by definitional exclusion; the viable coalition is proxy-based (advocates plus families), but it is structurally weakened because the families most affected sit INSIDE the decision seat, splitting the constituency. Measurement series run on one shared time grid (t=0..55) with every tracked metric authored at every point; the interval spans the modern articulation of the reading (early analytic bioethics) through institutionalization in neonatal ethics committees and formal protocols, to the contemporary contested plateau. Extractiveness rises as neonatal technology widens the decision surface (more infants survive into the zone where the judgment applies); suppression_requirement rises with the deliberate build-out of enforcement infrastructure (ethics committees, documentation regimes, protocol adoption) — that machinery-building is the dynamic being traced, hence the series is authored; theater rises as public controversy forces performative defense of the criterion.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the guardian seat the boundary appears as tragic discretion — a mercy framework that spares suffering, with the parent bearing grief as the price of authority. From the committee seat it appears as orderly governance: criteria, concurrence, documentation. From the excluded infant's computed position it is total exposure: no right, no voice, no exit, protection wholly contingent on others' judgment. From the advocate seat it is a standing-contingency threat: a public theory under which people like them could be reclassified. From the able-bodied majority seat it is invisible — the security it provides never announces itself. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Parental guardians derive near the beneficiary end: they receive the discretion and the burden-relief, and their costs (grief) are borne within the arrangement rather than against it. Able-bodied persons sit nearest the full-beneficiary pole: unconditional standing, zero payment, mobile indifference. Medical ethics committees collect jurisdictional rents from administration, pulling their derived directionality below symmetric despite their agenda-setter role. Severely disabled infants derive at the full-target pole: maximum extraction, trapped exit, no recourse. Disabled rights advocates are victims only through devaluation externality — they retain their own standing — so their true exposure is lower than a direct victim's; the structural derivation from the victim declaration approximates this adequately at story scale, and no per-agent override is warranted given the override mechanism keys on power atoms rather than agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a determinate, secularly administrable standing line — is live, so this is not a piton and mandatrophy is not resolved. The tangled_rope classification is what prevents mislabeling in both directions: a pure-snare reading would erase the genuine coordination problem (every legal system must draw the line somewhere, and the capacity-anchored answer solved a real problem theology-dominated frameworks could not solve pluralistically); a pure-rope reading would erase the identifiable victim class and the asymmetric enforcement that maintains the line. The hybrid holds both truths: real coordination function, real extraction through the same structure. The measurement series shows the extraction accumulating on top of the coordination function over the interval — the classic tangled-rope drift signature — without the function itself dying, which is why the series plateaus rather than collapsing into theater dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the personhood_boundary kernel (reading: potential_based_reading). What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three linked stories: hold the referent fixed (the standing arrangement) and observe how the victim set and epsilon move when the criterion swaps between birth, demonstrated fitness, and potential. The disagreement is located in the standing-criterion element alone; all three readings share the coordination problem and the enforcement architecture.',
    'If the birth_threshold_reading prevails, this reading''s victim set empties and its extraction collapses toward zero. If the fitness_contingent_reading prevails, the victim set enlarges to include pre-fitness healthy infants and extraction rises further. The potential-based reading''s classification is therefore conditional on a criterion choice, not on any fact this story can settle internally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which kernel reading this is, what siblings would change, where the readings diverge.').

omega_variable(
    potential_criterion_indeterminacy,
    'Is ''potential for rational agency'' a determinate property that can be assessed clinically, or an evaluative judgment that smuggles guardian and physician values into the exclusion decision?',
    'Inter-rater reliability studies of exclusion judgments across institutions: if committees agree on which infants lack the relevant potential at rates comparable to settled medical classifications, the criterion is determinate; if agreement tracks the values profile of the deciding institution, it is evaluative.',
    'If indeterminate, exclusion judgments reduce to discretionary value calls wearing a scientific vocabulary, effective extraction rises above the authored value, and the theater component grows correspondingly; if determinate, part of the measured extraction is the unavoidable price of any administrable criterion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_criterion_indeterminacy, conceptual, 'Whether the exclusion criterion is a measurement or a disguised value judgment.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of the disabled community structural (protocols, legal doctrine, exclusion from deliberation) or internalized (devaluation carried inward by disabled persons themselves)?',
    'Post-exit suppression trajectory: track self-reported standing-confidence and life-project formation among disabled cohorts in jurisdictions that formally repudiated capacity-based criteria. If devaluation effects persist after the structural mechanism is removed, a substantial fraction is internalized.',
    'If largely internalized, the constraint''s effective suppression exceeds the structural measure — the targets carry the suppression with them even where the reading loses official force, and remediation requires more than doctrinal change. Author''s working estimate: roughly 60% structural, 40% internalized, held at low confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the devaluation externality.').

omega_variable(
    exclusion_class_scope_drift,
    'How large is the class actually excluded in practice — confined to lethal-anomaly cases near the theoretical core, or drifting outward toward broader disability categories as protocols accumulate precedents?',
    'Longitudinal audit of neonatal protocol applications: diagnosis distribution of exclusion judgments over time, compared against the reading''s theoretical extension. Outward drift in diagnosis categories indicates the criterion''s practical reach exceeding its stated scope.',
    'Victim-set size drives the magnitude of effective extraction. If the practiced class is drifting outward, the reading operates closer to the fitness-contingent sibling than its theory admits, and the correct comparison constraint shifts within the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_class_scope_drift, empirical, 'Whether the practiced exclusion class matches the reading''s theoretical scope or drifts beyond it.').

omega_variable(
    proxy_coalition_viability,
    'Can the excluded class acquire effective coalition power through proxies (disability advocates, unaffected family members), given that the most affected families sit inside the decision seat?',
    'Compare policy outcomes and protocol revisions in jurisdictions with strong organized disability movements against jurisdictions without them, controlling for medical culture: differential revision rates indicate proxy coalition efficacy.',
    'If proxy coalitions are efficacious, the constraint''s persistence depends on ongoing contest rather than settled dominance, supporting the tangled_rope reading over snare. If proxies systematically fail — because families are split and advocates are outside the room — the extraction approaches the pure-snare profile and the resistance metric is overstated as a check.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_coalition_viability, empirical, 'Whether proxy representation can substitute for the excluded class''s constitutive voicelessness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pers_tr_t11, personhood_boundary__potential_based_reading, theater_ratio, 11, 0.2).
narrative_ontology:measurement(pers_tr_t22, personhood_boundary__potential_based_reading, theater_ratio, 22, 0.26).
narrative_ontology:measurement(pers_tr_t33, personhood_boundary__potential_based_reading, theater_ratio, 33, 0.33).
narrative_ontology:measurement(pers_tr_t44, personhood_boundary__potential_based_reading, theater_ratio, 44, 0.38).
narrative_ontology:measurement(pers_tr_t55, personhood_boundary__potential_based_reading, theater_ratio, 55, 0.4).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pers_be_t11, personhood_boundary__potential_based_reading, base_extractiveness, 11, 0.52).
narrative_ontology:measurement(pers_be_t22, personhood_boundary__potential_based_reading, base_extractiveness, 22, 0.6).
narrative_ontology:measurement(pers_be_t33, personhood_boundary__potential_based_reading, base_extractiveness, 33, 0.68).
narrative_ontology:measurement(pers_be_t44, personhood_boundary__potential_based_reading, base_extractiveness, 44, 0.71).
narrative_ontology:measurement(pers_be_t55, personhood_boundary__potential_based_reading, base_extractiveness, 55, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(pers_su_t11, personhood_boundary__potential_based_reading, suppression_requirement, 11, 0.38).
narrative_ontology:measurement(pers_su_t22, personhood_boundary__potential_based_reading, suppression_requirement, 22, 0.48).
narrative_ontology:measurement(pers_su_t33, personhood_boundary__potential_based_reading, suppression_requirement, 33, 0.58).
narrative_ontology:measurement(pers_su_t44, personhood_boundary__potential_based_reading, suppression_requirement, 44, 0.65).
narrative_ontology:measurement(pers_su_t55, personhood_boundary__potential_based_reading, suppression_requirement, 55, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'the personhood boundary' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. The label conflates claims with different victim sets and different epsilon: birth_threshold_reading (victim set empty; negligible extraction), this potential_based_reading (victim set = entities lacking potential for rational agency; substantially extractive), and fitness_contingent_reading (victim set additionally includes pre-fitness healthy infants; most extractive of the three). The upstream member (birth threshold, highest legal entrenchment) influences the downstream members because its doctrinal security is what the capacity-based readings argue against; each story links the others via affects_constraints. The confusion lives in the shared label, not in any single story's mathematics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
