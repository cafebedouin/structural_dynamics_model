% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__copenhagen_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Settlement: Collapse as Physical Process, Measurement as Primitive
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This story authors the Copenhagen reading of the shared quantum_formalism
 *   kernel as a single, ε-invariant constraint: the interpretive settlement,
 *   codified from the 1927 Solvay congress onward, under which wavefunction
 *   collapse is a physical process, measurement is a primitive ontological
 *   category, the observer's role is non-eliminable, and determinism is
 *   abandoned at measurement events. The ε referent is the standing
 *   arrangement under contest — the settlement's governance of
 *   quantum-foundations practice, assessed as it stands — not the formalism
 *   itself (upstream of all readings) and not any sibling reading's endorsed
 *   alternative. The settlement presents itself as natural law: 'irreducible'
 *   indeterminism, an 'absolute' epistemic boundary, features of nature
 *   rather than choices. That self-presentation is authored as the
 *   claimed_type (mountain, emerges_naturally: true) under FSM authoring —
 *   the canonical case of a discipline benefiting from a contested empirical
 *   claim treated as settled. The authored metrics describe the settlement's
 *   actual operation: a genuine coordination achievement (a century of
 *   uniform formalism deployment, shared pedagogy, predictive practice)
 *   carrying a documented extraction record (the marginalization of
 *   hidden-variable and measurement-problem research from roughly 1930 to the
 *   1990s, with Bohm's exile and Everett's silencing as the extreme cases),
 *   held in place by active enforcement whose machinery rose through the
 *   mid-century and decayed after the quantum-information rehabilitation of
 *   foundations. Sibling readings (quantum_formalism__many_worlds_reading,
 *   quantum_formalism__pilot_wave_reading) are separate constraint stories
 *   with their own ε, beneficiaries, and victims, linked through
 *   network.affects_constraints; the disagreement among readings is located
 *   at measurement's ontological status.
 *
 * KEY AGENTS:
 *   - - orthodox_interpretive_lineage: agenda-setter and primary beneficiary (institutional / identity_locked) — administers the settlement, collects its interpretive authority; exit would dissolve the tradition itself
 *   - - working_quantum_physicists: coordinated beneficiary with payer exposure (organized / constrained) — inherits uniform practice, pays a closed question-set
 *   - - hidden_variable_researchers: primary target (moderate / identity_locked) — bore enforcement directly; the marginalized program is their life's technical work
 *   - - quantum_foundations_researchers: secondary target (moderate / constrained) — measurement-problem work long classified out of physics
 *   - - philosophy_of_physics_community: excluded voice (moderate / mobile) — objects from a seat whose standing the settlement itself denies
 *   - - quantum_information_theorists: analytical observer (institutional / analytical) — decoherence and information tools reshaped the enforcement environment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.45).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.35).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, mountain).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Settlement: Collapse as Physical Process, Measurement as Primitive").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).
domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '2e5fa360-8ba6-420f-9399-dbbcb3993171').
narrative_ontology:cs_kernel_codification('2e5fa360-8ba6-420f-9399-dbbcb3993171', formalized).
narrative_ontology:cs_authority_grounding('2e5fa360-8ba6-420f-9399-dbbcb3993171', lineage).
narrative_ontology:cs_interpretation_layer_present('2e5fa360-8ba6-420f-9399-dbbcb3993171').
narrative_ontology:cs_reading_relation('2e5fa360-8ba6-420f-9399-dbbcb3993171', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_reading_relation('2e5fa360-8ba6-420f-9399-dbbcb3993171', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('2e5fa360-8ba6-420f-9399-dbbcb3993171', foundational, measurement_indeterminism_irreducible).
narrative_ontology:cs_axiom_status(measurement_indeterminism_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('2e5fa360-8ba6-420f-9399-dbbcb3993171', measurement_indeterminism_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('2e5fa360-8ba6-420f-9399-dbbcb3993171', foundational, classical_description_boundary_indispensable).
narrative_ontology:cs_axiom_status(classical_description_boundary_indispensable, holdable).
narrative_ontology:cs_axiom_grounding('2e5fa360-8ba6-420f-9399-dbbcb3993171', classical_description_boundary_indispensable, conventional).
narrative_ontology:cs_reference_frame('2e5fa360-8ba6-420f-9399-dbbcb3993171', collapse_completed_formalism).
narrative_ontology:cs_drift_state('2e5fa360-8ba6-420f-9399-dbbcb3993171', post_bell_decoherence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2e5fa360-8ba6-420f-9399-dbbcb3993171', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, orthodox_interpretive_lineage).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, working_quantum_physicists).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, hidden_variable_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, quantum_foundations_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, working_quantum_physicists).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, complementarity_principle).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, wavefunction_completeness_thesis).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, irreducible_indeterminism_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians of the interpretive settlement descending from Bohr and Heisenberg: textbook authors, institute traditions, and senior figures who administer what counts as settled physics. Their pronouncements carry the weight of the founding generation, and the settlement's closure is the ground of their interpretive authority — defending it is indistinguishable from maintaining their own standing. Leaving the settlement would mean surrendering the tradition they embody; the lineage and the settlement are the same thing.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, orthodox_interpretive_lineage, agenda_setter,
    institutional, generational, identity_locked, global).

% Practicing researchers who use the formalism daily in condensed matter, optics, atomic physics, and beyond. They inherit a shared stance that lets them compute, publish, and build without settling what measurement is — a substantial professional benefit. They pay by accepting a question-set in which 'what happens at measurement' is not a physics question; most never notice the payment, and those who do face a choice between the mainstream's terms and the margins of the field. The formalism runs through every subfield, so leaving the settlement's terms means leaving the field's center.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, working_quantum_physicists, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, working_quantum_physicists, payer).

% Researchers pursuing definite-ontology completions of the formalism in the lineage of de Broglie and Bohm. They bore the settlement's enforcement directly: publication refusal, funding denial, appointment barriers, and the exile of the program's founder. Their expertise is the marginalized program itself, so leaving means abandoning a life's technical work; the program's persistence across generations has been carried by a small number of researchers at sustained professional cost.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, hidden_variable_researchers, payer,
    moderate, generational, identity_locked, global).

% Researchers working on the measurement problem, collapse models, and the foundations of quantum probability. For decades their questions were classified as philosophy rather than physics, with the career consequences that classification carried; the quantum-information era partially rehabilitated the field, but entry still costs a departure from the mainstream research agenda, and senior mentors still steer students away.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_foundations_researchers, payer,
    moderate, biographical, constrained, global).

% Philosophers and philosophically trained physicists who argue the measurement problem is a real open problem rather than a dissolved one. Their objection is structurally discounted: the settlement's closure reclassifies their contribution as outside physics, so they argue from a seat whose standing the settlement itself denies. They can work in philosophy proper, which softens the cost of exclusion but does not admit them to the conversation they are excluded from.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophy_of_physics_community, excluded,
    moderate, biographical, mobile, global).

% The community that built decoherence theory, quantum computing, and quantum information science from the 1980s onward. Their technical tools reopened questions the settlement had closed — decoherence in particular explains how definite outcomes appear without collapse — and their standing gave foundations research a respectability it had lacked. They assess the settlement from outside its enforcement history while reshaping the environment it enforces in.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_information_theorists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, orthodox_interpretive_lineage).
narrative_ontology:fixing_cost_class(quantum_formalism__copenhagen_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the worldwide quantum-physics community a single shared stance from which the formalism can be deployed uniformly: measurement outcomes are treated as definite single events, probabilities as objective Born-rule chances, and no further account of measurement is required to compute, publish, or build. It solved the practice problem of using a formalism whose measurement postulate had no physical account.
% TRANSFER_FUNCTION: Moves interpretive authority and research legitimacy from foundational inquirers to the custodial lineage: questions about what measurement physically is were reclassified from open physics to closed 'interpretation,' and with them went publication access, funding eligibility, hiring prospects, and standing. Attention and career resources flowed toward formalism-deploying work; the measurement problem itself was moved out of the research agenda.
% ABSENT_VOICES: Hidden-variable researchers were absent from the settlement's codification and its enforcement: Bohm's 1952 pilot-wave proposal was dismissed by leading orthodox figures without technical engagement, and Bohm himself left for Brazil amid the loyalty climate. Everett's 1957 relative-state formulation was met with near-silence and he left academic physics. Philosophy of physics was excluded as 'mere interpretation' rather than engaged. The settlement's apparent unanimity partly reflects who was never admitted to the conversation.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, practice would not collapse — the formalism's predictions stand on their own — but the community would rearrange around explicit interpretive pluralism: pedagogy would lose its unifying narrative, journals and funding panels would need interpretive-neutrality rules, and the measurement problem would have returned to the open research agenda decades earlier than it did. The post-1990s loosening shows the rearrangement is survivable, which is itself evidence the settlement's persistence is maintained rather than necessary.
% FOUNDING_PROBLEM: In the late 1920s quantum theory predicted superpositions while every experiment yielded single definite outcomes, and the theory had no account of measurement at all. The settlement was built to make the theory usable: a working stance that licenses prediction and communication without first solving what measurement physically is.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historical scholarship on the 1920s-30s interpretive crisis (the Solvay debates, the EPR exchange) documents both the founding problem and its resolution-by-fiat, and foundations researchers outside the lineage attest that the crisis-era justification lapsed while the settlement persisted. The lineage itself attests the problem as permanently live — a pedagogical claim that each generation must be trained into the settlement — and no party outside the lineage attests that the original usability crisis still exists.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, ExtMetricName, E),
    domain_priors:suppression_score(quantum_formalism__copenhagen_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45 as the settlement's current level, with the temporal series documenting a mid-century peak of 0.62: the settlement's extraction is epistemic and professional rather than material — publication refusal, funding denial, appointment barriers, and the reclassification of foundational questions as non-physics — and it partially lifted after the 1990s rehabilitation. Suppression is 0.35: the enforcement machinery has largely decayed, but persistence is now carried partly by internalized professional norms, which the suppression-mechanism omega tracks; suppression is authored as the raw structural property and is not scaled by scope or power — only extractiveness is scaled, by directionality and scope in the engine's computation. Theater is 0.45 and rising: as the founding crisis recedes, an increasing share of the settlement's maintenance is ritual — 'Copenhagen' invoked as heritage, settledness asserted in pedagogy while practice pluralizes — with 0.5 as the watch-line. Accessibility_collapse is 0.40: alternatives exist, are published, and are now taught in foundations courses, but mainstream training still collapses onto the settlement first, so the alternatives remain effectively invisible unless sought — far from the ~0.85+ a genuine natural law would show. Resistance is 0.55: decades of sustained foundational resistance (Bohm, Everett, Bell, the philosophical critique, the quantum-information reopening) that partially succeeded — a profile no genuine natural law exhibits. The claimed_type records the settlement's self-presentation and is deliberately not reconciled to these metrics; the divergence is the false-summit signal the FSM signature exists to take. Coordination type is authored as identity_coordination: the predictive protocol the settlement wraps could be served by any shared stance — the reading-specific function is boundary maintenance, deciding what counts as physics rather than metaphysics — so the identity floor default is used without override.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the settlement is three different structures from three positions. From the custodial lineage it is physics completed: Bohr's answer to EPR, the closure that let a century of practice proceed — the naturality claim is sincere, not cynical. From working physicists it is invisible infrastructure: a benefit so thoroughly absorbed that its cost (a closed question-set) is mostly unnoticed. From the target seats it is a gate: Bohm's exile, Everett's silencing, Bell's decade of neglect, a generation of careers spent establishing that the margins were physics. Same formalism, same settlement, three constraints. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The lineage sits at the beneficiary end: it collects the settlement's yield — interpretive authority, pedagogical control, the standing of settled physics — and its identity-lock means the settlement's persistence and its own persistence are the same fact. Working quantum physicists sit near symmetric: the declared beneficiary role is real (uniform practice, shared stance), but the secondary payer exposure (the closed question-set, the delayed foundations agenda) pulls them toward the middle; their constrained exit — the formalism is everywhere in their fields — keeps them inside either way. Hidden-variable researchers sit at the target end: they bear the enforcement directly, and their identity-lock (the program is their life's work) places them nearer the full-target end than moderate power alone would. Foundations researchers are high-target with constrained exit: pivoting to mainstream agendas means abandoning their expertise. The philosophy community is excluded rather than coordinated — its objection is structurally discounted by the settlement's own closure, which is why exclusion, not payer status, is its authored role. Quantum information theorists observe analytically while reshaping the environment the settlement enforces in.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — make a theory with no measurement account usable — was solved by the settlement's own fiat, and the R5 interview records the status as dead while the disappearance verdict is world_rearranges: the mismatch is the zombie flag, and here it is honest. What persists is not the crisis response but the settlement's authority and pedagogy. The classification prevents mislabeling in both directions: the settlement is not pure extraction, because the coordination function is genuine (uniform deployment of the formalism, shared pedagogy, predictive practice — none of which requires Copenhagen specifically, but all of which required some shared stance); and it is not a mountain, because the naturality claim is contested, the enforcement history is documented, and identifiable parties collect from the closure. The temporal series shows the drift the mandatrophy lens predicts: extraction declining from its mid-century peak, theater rising as function recedes, enforcement decaying while internalized norms carry persistence. If theater crosses 0.5 while enforcement stays low, the settlement completes the drift toward a heritage maintained performatively over a question-set nobody any longer enforces. The FSM evaluation of the mountain claim is the entry point for that verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_settlement,
    'Is irreducible indeterminism at measurement a genuine feature of nature, or a constructed interpretive settlement that benefits identifiable parties?',
    'Loophole-free Bell tests already exclude local hidden variables; what remains decisive is experimental: spontaneous-collapse model tests bounding objective reduction, any deterministic ontology yielding distinct predictions, or continued empirical equivalence of all readings across every testable regime.',
    'If collapse is a genuine primitive physical process, the settlement approaches a natural limit and the enforcement record reads as the price of nature''s structure; if it is a constructed settlement, the false-summit reclassification stands and the enforcement history is extraction from identifiable targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_settlement, empirical, 'The natural-law versus constructed-settlement ambiguity the mountain claim rests on.').

omega_variable(
    measurement_problem_status,
    'Is the measurement problem actually solved — by decoherence plus the formalism, or by the settlement''s completeness doctrine — or does the settlement''s closure conceal an open problem?',
    'Technical assessment of whether decoherence derives definite single outcomes or merely suppresses interference between outcomes; progress on objectivization thresholds and collapse-model phenomenology.',
    'If solved, the settlement''s persistence is inertia and its maintenance trends theatrical; if open, the settlement''s closure actively withholds a live problem from the research agenda and the extraction assessment rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_problem_status, empirical, 'Whether the founding closure dissolved a real problem or declared it out of bounds.').

omega_variable(
    sibling_empirical_equivalence,
    'Are the sibling readings empirically equivalent to this settlement, such that its historical enforcement reflected authority rather than evidence?',
    'A demonstrated empirical divergence among readings — collapse-model parameter space, Bohmian nonequilibrium signatures, Everettian probability derivations — or their continued equivalence under every testable regime.',
    'Continued equivalence supports reading the enforcement record as authority maintenance rather than evidence-based closure, raising the extraction assessment; demonstrated divergence with the settlement vindicated would lower it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_empirical_equivalence, empirical, 'Whether the settlement''s closure of alternatives had evidentiary warrant.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the copenhagen_reading of the kernel quantum_formalism — how would instantiating quantum_formalism__many_worlds_reading or quantum_formalism__pilot_wave_reading change the constraint''s structure?',
    'Authoring the sibling stories: many-worlds removes measurement as a primitive category (decoherence-induced branching, no collapse, determinism retained), eliminating the observer''s non-eliminable role; pilot-wave restores definite positions under a deterministic guiding field, making collapse apparent-only. The disagreement is located at measurement''s ontological status: primitive physical process (this reading) versus emergent or derivative (both siblings).',
    'Under either sibling there is no primitive measurement event and hence no absolute epistemic boundary for a settlement to guard; the closure warrant collapses, the victim structure changes, and the extraction assessment of the orthodox arrangement rises — but that structure belongs to the sibling stories, not this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame delta: what the sibling readings of the shared kernel would structurally change.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the settlement''s suppression structural (journal gatekeeping, funding denial, career penalties) or internalized (professional norms that make foundational questions feel illegitimate without any enforcer)?',
    'The post-1990s natural experiment: gatekeeping relaxed while many practitioners still treat interpretation work as disreputable; if the disrepute persists without enforcement machinery, it is internalized.',
    'Internalized suppression means the settlement can persist as enforcement decays — matching the measured divergence of falling suppression_requirement against rising theater_ratio — and keeps effective suppression above what the structural measure alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the settlement''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t15, quantum_formalism__copenhagen_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(quan_tr_t15, observed).
narrative_ontology:measurement(quan_tr_t30, quantum_formalism__copenhagen_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(quan_tr_t30, observed).
narrative_ontology:measurement(quan_tr_t45, quantum_formalism__copenhagen_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement_basis(quan_tr_t45, observed).
narrative_ontology:measurement(quan_tr_t60, quantum_formalism__copenhagen_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(quan_tr_t60, observed).
narrative_ontology:measurement(quan_tr_t75, quantum_formalism__copenhagen_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(quan_tr_t75, observed).
narrative_ontology:measurement(quan_tr_t90, quantum_formalism__copenhagen_reading, theater_ratio, 90, 0.45).
narrative_ontology:measurement_basis(quan_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t15, quantum_formalism__copenhagen_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(quan_be_t15, observed).
narrative_ontology:measurement(quan_be_t30, quantum_formalism__copenhagen_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(quan_be_t30, observed).
narrative_ontology:measurement(quan_be_t45, quantum_formalism__copenhagen_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement_basis(quan_be_t45, observed).
narrative_ontology:measurement(quan_be_t60, quantum_formalism__copenhagen_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement_basis(quan_be_t60, observed).
narrative_ontology:measurement(quan_be_t75, quantum_formalism__copenhagen_reading, base_extractiveness, 75, 0.48).
narrative_ontology:measurement_basis(quan_be_t75, observed).
narrative_ontology:measurement(quan_be_t90, quantum_formalism__copenhagen_reading, base_extractiveness, 90, 0.45).
narrative_ontology:measurement_basis(quan_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__copenhagen_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t15, quantum_formalism__copenhagen_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement_basis(quan_su_t15, observed).
narrative_ontology:measurement(quan_su_t30, quantum_formalism__copenhagen_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(quan_su_t30, observed).
narrative_ontology:measurement(quan_su_t45, quantum_formalism__copenhagen_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement_basis(quan_su_t45, observed).
narrative_ontology:measurement(quan_su_t60, quantum_formalism__copenhagen_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(quan_su_t60, observed).
narrative_ontology:measurement(quan_su_t75, quantum_formalism__copenhagen_reading, suppression_requirement, 75, 0.4).
narrative_ontology:measurement_basis(quan_su_t75, observed).
narrative_ontology:measurement(quan_su_t90, quantum_formalism__copenhagen_reading, suppression_requirement, 90, 0.35).
narrative_ontology:measurement_basis(quan_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Copenhagen interpretation' is one reading of the shared quantum_formalism kernel. Per the ε-invariance principle, each reading is authored as its own constraint story with its own ε, beneficiaries, and victims: this story authors the orthodox settlement (collapse physical, measurement primitive); the many-worlds and pilot-wave stories author their respective arrangements. The kernel itself — the formalism's predictive apparatus — is upstream of all three and is not authored here. This story links to both siblings because the settlement's enforcement history shaped their marginalization (upstream influence on their operating environment) even as their frameworks are mutually exclusive with it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
