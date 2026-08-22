% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Collapse Orthodoxy — Measurement as Primitive Epistemic Boundary
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The Copenhagen reading of the quantum formalism — collapse as a physical
 *   process marking an absolute epistemic boundary, measurement as a
 *   primitive ontological category, the observer non-eliminable, determinism
 *   abandoned at measurement events — has operated since the 1927 Solvay
 *   congress not merely as an interpretation but as an administered
 *   settlement: textbook presentation, funding classification, hiring, and
 *   referee gatekeeping all enforce the boundary it declares. This story is
 *   ONE reading of the kernel quantum_formalism (one reading, one constraint,
 *   one epsilon); the sibling readings many_worlds_reading and
 *   pilot_wave_reading are separate stories with their own epsilon values,
 *   victim sets, and enforcement profiles, linked through the network block.
 *   The epsilon referent here is the standing arrangement under contest — the
 *   Copenhagen-governed discipline as this reading actually operates — not
 *   the arrangements the siblings would install; sibling stories author their
 *   own epsilon over their own referents, and the cross-reading comparison is
 *   carried by the kernel_reading_epsilon_indexicality omega. This reading's
 *   epsilon (0.48, moderate and declining from a mid-century peak) differs
 *   from what a rival reading's story would author because the referents and
 *   the reading-indexed assessments differ. Claim and metrics are
 *   independent: claimed_type is my structural judgment; the metrics describe
 *   the arrangement's operation as the historical record shows it.
 *
 * KEY AGENTS:
 *   - physics_establishment_gatekeepers: agenda setter (institutional/arbitrage) — administers the epistemic boundary through refereeing, hiring, and curriculum; collects the boundary-authority rents
 *   - instrumentalist_working_physicists: primary beneficiary (organized/constrained) — inherits a settled framework, spared metaphysical dispute
 *   - physics_funding_agencies: secondary beneficiary (institutional/arbitrage) — gains a principled-seeming criterion for excluding interpretive work from physics budgets
 *   - textbook_publishers: beneficiary and enforcement arm (institutional/mobile) — perpetuates the measurement-postulate-as-brute-fact presentation
 *   - quantum_foundations_researchers: primary payer (moderate/constrained) — bears referee friction, hiring discount, funding exclusion
 *   - alternative_interpretation_programs: payer (powerless/identity_locked) — Bohmian and Everettian communities whose research identity is the targeted alternative
 *   - philosophy_of_physics_community: excluded voice (moderate/mobile) — would contest the boundary's absoluteness; kept outside the rooms where it is administered
 *   - interpretive_epistemology_analysts: analytical observer (analytical/analytical) — maps the boundary-maintenance from outside the enforcement chain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.48).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.55).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Collapse Orthodoxy — Measurement as Primitive Epistemic Boundary").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '5b8b9dae-ff2a-48df-9075-bf66a851e336').
narrative_ontology:cs_kernel_codification('5b8b9dae-ff2a-48df-9075-bf66a851e336', formalized).
narrative_ontology:cs_authority_grounding('5b8b9dae-ff2a-48df-9075-bf66a851e336', lineage).
narrative_ontology:cs_interpretation_layer_present('5b8b9dae-ff2a-48df-9075-bf66a851e336').
narrative_ontology:cs_reading_relation('5b8b9dae-ff2a-48df-9075-bf66a851e336', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_reading_relation('5b8b9dae-ff2a-48df-9075-bf66a851e336', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('5b8b9dae-ff2a-48df-9075-bf66a851e336', foundational, collapse_is_physical_process).
narrative_ontology:cs_axiom_status(collapse_is_physical_process, holdable).
narrative_ontology:cs_axiom_grounding('5b8b9dae-ff2a-48df-9075-bf66a851e336', collapse_is_physical_process, empirically_contingent).
narrative_ontology:cs_axiom('5b8b9dae-ff2a-48df-9075-bf66a851e336', foundational, measurement_indeterminism_irreducible).
narrative_ontology:cs_axiom_status(measurement_indeterminism_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('5b8b9dae-ff2a-48df-9075-bf66a851e336', measurement_indeterminism_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('5b8b9dae-ff2a-48df-9075-bf66a851e336', secondary, unmeasured_reality_outside_physics_scope).
narrative_ontology:cs_axiom_status(unmeasured_reality_outside_physics_scope, holdable).
narrative_ontology:cs_axiom_grounding('5b8b9dae-ff2a-48df-9075-bf66a851e336', unmeasured_reality_outside_physics_scope, conventional).
narrative_ontology:cs_reference_frame('5b8b9dae-ff2a-48df-9075-bf66a851e336', measurement_primitive_orthodoxy).
narrative_ontology:cs_drift_state('5b8b9dae-ff2a-48df-9075-bf66a851e336', post_bell_decoherence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5b8b9dae-ff2a-48df-9075-bf66a851e336', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, instrumentalist_working_physicists).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, physics_funding_agencies).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, textbook_publishers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, quantum_foundations_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, alternative_interpretation_programs).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, complementarity_principle).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, wavefunction_completeness_doctrine).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_postulate_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Journal editors, department hiring committees, and curriculum committees decide which interpretive questions count as physics and which are redirected to philosophy. They administer the peer review, hiring, and course requirements through which the collapse orthodoxy is maintained. Their authority over the discipline's epistemic boundary is constituted by the arrangement they administer; adopting interpretive pluralism would mean surrendering the gatekeeping role itself. They can redirect their own attention freely, but the seat's authority exists only inside the arrangement.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, physics_establishment_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% Apply the formalism to spectroscopy, condensed matter, and quantum devices without engaging ontology. The settlement licenses this division of labor: they inherit a settled framework, are spared metaphysical dispute, and are never required to defend the measurement postulate. Taking interpretive questions seriously would cost research time and expose them to boundary disputes their training taught them to avoid.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, instrumentalist_working_physicists, beneficiary,
    organized, biographical, constrained, global).

% National and supranational agencies allocate quantum research money. The settlement supplies a principled-seeming criterion: applications work is physics, interpretive work is philosophy, and only the former is fundable from physics budgets. They can redirect funds between portfolios at will; the arrangement costs them nothing and simplifies allocation decisions.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, physics_funding_agencies, beneficiary,
    institutional, generational, arbitrage, continental).

% Produce the canonical presentation in which the measurement postulate appears as a brute axiom and collapse as a settled fact. The chapter structure is stable across editions and markets; a pluralist rewrite would be costly, unproven with adoption committees, and unsettling to the instructor base. Their perpetuation of the presentation is itself part of the enforcement chain.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, textbook_publishers, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, textbook_publishers, agenda_setter).

% Work on the measurement problem, collapse models, and interpretation. They face referee friction ('is this physics?'), hiring panels that discount the specialty, and funding calls written to exclude them. Exit to application work is possible but abandons accumulated expertise and mid-career momentum; many stay because the measurement problem is the reason they entered physics at all.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_foundations_researchers, payer,
    moderate, biographical, constrained, global).

% The Bohmian and Everettian research communities — small groups whose professional identity is constituted by the alternative program itself. The orthodoxy's enforcement (no-go folklore in teaching, 'hidden variables are dead' pedagogy, marginalization in hiring) falls hardest on them; their generational horizon reflects programs sustained across decades by a few departments and individuals. Leaving the program would dissolve the research identity; there is no neutral seat for them to occupy.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, alternative_interpretation_programs, payer,
    powerless, generational, identity_locked, global).

% Would contest both the absoluteness of the epistemic boundary and the physics/philosophy demarcation through which it is enforced. They are structurally outside the rooms where the boundary is administered — joint appointments are rare, and philosophy-venue citations are discounted in physics evaluation. They publish and argue, but in venues the gatekeeping does not count.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophy_of_physics_community, excluded,
    moderate, biographical, mobile, global).

% Analytical seat: historians and philosophers of science and science-studies scholars who map how the boundary is maintained without holding a stake in its defense or removal. They see the full structure — the coordination the settlement provides and the costs it distributes — from outside the enforcement chain.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, interpretive_epistemology_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, physics_establishment_gatekeepers).
narrative_ontology:fixing_cost_class(quantum_formalism__copenhagen_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the discipline a shared, teachable protocol for applying the formalism and a settled demarcation of which questions are physics: experiment design, reproducibility, and communication proceed without each practitioner first settling ontology.
% TRANSFER_FUNCTION: Moves epistemic legitimacy, funding eligibility, and career security away from interpretive and foundational inquiry toward formalism-application work; moves boundary-defining authority itself to the gatekeeping seats (journal editors, hiring committees, the textbook tradition).
% ABSENT_VOICES: Philosophers of physics and alternative-interpretation researchers are structurally outside the rooms where the boundary is administered (department hiring, funding panels, physics-journal referee pools). Present, they would contest the absoluteness of the epistemic boundary and the physics/philosophy demarcation the enforcement rides on; they currently argue in venues the gatekeeping does not count.
% DISAPPEARANCE_RATIONALE: The formalism's predictions would not change — that is the kernel, not this reading — but the interpretive economy would rearrange within a generation: pedagogy would have to teach an explicit interpretive stance, funding agencies would lose their principled-seeming criterion for excluding foundations work, measurement-problem research would normalize in legitimacy, and the gatekeeping seats' boundary authority would dissolve with the boundary itself. The physics/philosophy demarcation in quantum foundations would be renegotiated from scratch.
% FOUNDING_PROBLEM: The 1920s crisis: deterministic unitary evolution yields superpositions, but experiments yield single definite outcomes. The arrangement was built to give practitioners a usable predictive rule (Born rule plus collapse on measurement) without a settled ontology, and to close off metaphysical dispute that threatened the young discipline's coherence.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by: continuing publication of measurement-problem and interpretation research in mainstream physics venues; funded rival programs (Everettian quantum cosmology, Bohmian mechanics, objective-collapse models with proposed experimental tests); and Bell-test experimental groups whose research program presupposes the interpretive stakes are real. No source outside the benefiting parties attests that the founding problem has been dissolved.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.48: moderate and declining from a mid-century peak. At peak (von Neumann no-go folklore treated as settled, McCarthy-era politics ending Bohm's US career, 'Copenhagen or unemployment' hiring), the arrangement taxed interpretive careers severely; after Bell (1964) made the interpretive stakes empirically testable, and decoherence plus quantum information legitimized foundations work, the tax fell to a persistent but lower level — referee friction, hiring discount, funding-call exclusion, pedagogical foreclosure of the question. Suppression 0.55 is the raw structural coercive force, authored unscaled (the engine scales only extractiveness, by directionality and scope): the boundary is actively administered and partly internalized as professional socialization. Theater_ratio 0.38 and rising slowly: the predictive apparatus is fully functional, but as decoherence absorbs the explanatory load 'collapse' once carried, a growing share of the reading's maintenance is invocation without operational content. Accessibility_collapse 0.40: alternatives are taxed, not eliminated — Everettian, Bohmian, QBist, and objective-collapse programs all publish and argue. Resistance 0.55: a century of organized resistance (EPR 1935, Bohm 1952, Bell 1964, the contemporary foundations community) that the arrangement must continuously defend against. All three metric series share one time grid (1927-2026, eight points); the trajectory is an enforcement ratchet to ~1955, decline after Bell, plateau at moderate enforcement. Claim and metrics were authored independently; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the gatekeeper seat the boundary is the discipline's own good sense — legitimate epistemology that keeps physics predictive; from the foundations-researcher seat the same boundary is enforced question-illegitimacy that taxes careers through refereeing, hiring, and funding. The reading itself occupies no constrained seat: it is the frame within which every other seat's options are defined, which is why its beneficiaries experience no constraint at all while its targets experience it as total. Same-level dynamics: instrumentalist physicists and foundations researchers hold identical nominal professional standing (doctorate, faculty track) yet face opposite option sets — the arrangement differentiates them by research program, not rank, which is what makes the cost asymmetric at equal power. Inter-institutionally, the three institutional beneficiary seats differ in exit: funders arbitrage across portfolios at will, publishers move across markets, departments are bound by curriculum inertia — same nominal power, different exposure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (instrumentalist_working_physicists, physics_funding_agencies, textbook_publishers) derive low directionality — the arrangement subsidizes them — with funders' arbitrage exit pushing them furthest toward the beneficiary pole. The gatekeepers (agenda_setters with arbitrage exit) sit near the beneficiary end while collecting the boundary-authority rents; that concentration is why gain_flow names them. Declared victims (quantum_foundations_researchers, constrained exit; alternative_interpretation_programs, identity_locked) derive high directionality, with identity lock pushing the alternative programs nearest the full-target pole. Global spatial scope makes verification of the 'no deeper ontology' claim a discipline-wide matter, amplifying effective extraction for targets while beneficiary gains stay concentrated in a few seats. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling deterministic unitary evolution with definite single outcomes well enough to practice — is live: decoherence explains the appearance of collapse, not single outcomes, and no source outside the benefiting parties attests the problem dissolved. Mandatrophy is not resolved. The classification guards against both mislabelings: a coordination-only reading would miss the career and question-legitimacy costs borne by foundations researchers through the same structure that coordinates prediction for everyone; an extraction-only reading would miss the genuine coordination (shared formalism application, reproducible experiment design, teachable pedagogy) the reading provides even to its critics. The atrophy signal to watch is the theater trajectory: if decoherence and quantum information fully absorb the reading's explanatory function, what remains is boundary maintenance — the arrangement would drift toward the inertial cell (theater above 0.5) unless enforcement is still doing coordination work. Identity-lock dynamics: the alternative programs' professional identity is fused with the alternative itself; if that frame broke — programs absorbed into a pluralist mainstream — their directionality would fall and the victim structure would thin. Suppression here is both structural (panels, refereeing, hiring) and internalized (socialized aversion to 'philosophy'); the internalized share now carries more of the load as formal barriers fell, which the suppression-mechanism omega tracks. The victims' coalition power grew after Bell — part of why suppression declined — and remains the main check on further ratcheting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_epsilon_indexicality,
    'This story instantiates the copenhagen_reading of kernel quantum_formalism; the sibling readings (many_worlds_reading, pilot_wave_reading) are separate constraints over the same formalism. Is the measured extraction a property of collapse-orthodoxy specifically, or would any enforced interpretive orthodoxy over this kernel extract similarly?',
    'Generate the sibling reading stories and compare per-seat classifications, victim sets, and epsilon over the shared formalism; the divergence localizes extraction to the reading versus the enforcement pattern.',
    'If siblings show low extraction under comparable enforcement, the extraction is specific to the collapse boundary''s claims; if all readings extract similarly when enforced, the extraction lives in enforcement-of-interpretation as such, and kernel-level remedies (not reading-level ones) are indicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_epsilon_indexicality, conceptual, 'Cross-reading comparison within the quantum-formalism kernel family.').

omega_variable(
    collapse_postulate_naturality,
    'Is the collapse postulate a forced consequence of the formalism plus no-go results (an epistemic necessity presenting as natural law), or a historically contingent institutional settlement that benefits identifiable actors?',
    'Progress on dynamical collapse models (GRW/CSL parameter bounds), decoherence-completion programs, and macroscopic-superposition experiments; if a collapse-free account of single outcomes stabilizes, the postulate''s necessity fails.',
    'If contingent, the reading''s mountain-like presentation (''just what quantum mechanics says'') is a false summit benefiting the gatekeeping seats, and classification shifts toward the constructed end of the natural/constructed axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_postulate_naturality, empirical, 'Whether the reading''s central claim is necessary or constructed.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of interpretive work structural (funding panels, hiring committees, referee gatekeeping) or internalized (professional socialization that trains physicists to treat foundational questions as illegitimate, persisting after formal barriers fall)?',
    'Post-exit trajectory: track physicists who move from gatekept to pluralist environments; if foundational curiosity revives, suppression was structural; if the aversion persists, it is internalized.',
    'If internalized, formal liberalization overstates freedom — suppression stays elevated even as institutional barriers drop, explaining the post-2000 plateau and meaning removal of gatekeeping alone would not release the suppressed inquiry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of the reading''s suppressive force.').

omega_variable(
    observer_role_eliminality,
    'The reading''s structural delta makes the observer non-eliminable — is the observer a primitive ontological category (Wigner''s friend as stated), or a placeholder for any decohering macroscopic system?',
    'Extended Wigner''s-friend protocols (Frauchiger-Renner-type experiments, multi-observer superpositions) and the community''s interpretation of their outcomes.',
    'If observers are eliminable in principle, the reading''s core delta collapses toward decoherence-based readings, the ''absolute'' epistemic boundary becomes derivable rather than primitive, and the constraint''s victim structure would thin substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(observer_role_eliminality, empirical, 'Status of the observer in the collapse postulate.').

omega_variable(
    demarcation_legitimacy,
    'Is the physics/philosophy boundary the reading enforces a legitimate methodological discipline (protecting predictive practice from unproductive metaphysics) or a self-serving demarcation that protects gatekeeping authority?',
    'Comparative analysis of fields that relaxed the boundary (quantum information, quantum gravity): did foundational engagement damage predictive output or feed it?',
    'If legitimate, part of the measured suppression is the price of disciplinary focus (coordination-side); if self-serving, it is pure extraction riding on the formalism''s success (extraction-side) — the same structure classifies differently depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demarcation_legitimacy, preference, 'Normative status of the enforced demarcation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1927, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1927, quantum_formalism__copenhagen_reading, theater_ratio, 1927, 0.15).
narrative_ontology:measurement(quan_tr_t1940, quantum_formalism__copenhagen_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(quan_tr_t1955, quantum_formalism__copenhagen_reading, theater_ratio, 1955, 0.25).
narrative_ontology:measurement(quan_tr_t1970, quantum_formalism__copenhagen_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(quan_tr_t1985, quantum_formalism__copenhagen_reading, theater_ratio, 1985, 0.33).
narrative_ontology:measurement(quan_tr_t2000, quantum_formalism__copenhagen_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(quan_tr_t2015, quantum_formalism__copenhagen_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(quan_tr_t2026, quantum_formalism__copenhagen_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.3).
narrative_ontology:measurement(quan_be_t1940, quantum_formalism__copenhagen_reading, base_extractiveness, 1940, 0.5).
narrative_ontology:measurement(quan_be_t1955, quantum_formalism__copenhagen_reading, base_extractiveness, 1955, 0.62).
narrative_ontology:measurement(quan_be_t1970, quantum_formalism__copenhagen_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(quan_be_t1985, quantum_formalism__copenhagen_reading, base_extractiveness, 1985, 0.54).
narrative_ontology:measurement(quan_be_t2000, quantum_formalism__copenhagen_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(quan_be_t2015, quantum_formalism__copenhagen_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(quan_be_t2026, quantum_formalism__copenhagen_reading, base_extractiveness, 2026, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1927, quantum_formalism__copenhagen_reading, suppression_requirement, 1927, 0.42).
narrative_ontology:measurement(quan_su_t1940, quantum_formalism__copenhagen_reading, suppression_requirement, 1940, 0.6).
narrative_ontology:measurement(quan_su_t1955, quantum_formalism__copenhagen_reading, suppression_requirement, 1955, 0.72).
narrative_ontology:measurement(quan_su_t1970, quantum_formalism__copenhagen_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement(quan_su_t1985, quantum_formalism__copenhagen_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(quan_su_t2000, quantum_formalism__copenhagen_reading, suppression_requirement, 2000, 0.57).
narrative_ontology:measurement(quan_su_t2015, quantum_formalism__copenhagen_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(quan_su_t2026, quantum_formalism__copenhagen_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'interpretation of quantum mechanics' covers three structurally distinct constraints (this copenhagen_reading, many_worlds_reading, pilot_wave_reading) over the shared kernel quantum_formalism. Per the epsilon-invariance principle they are authored as separate stories — each with its own epsilon, beneficiary/victim structure, and enforcement profile — and linked as a constraint family. This reading is the historically dominant downstream instantiation whose enforcement shaped the others' marginalization; its network edges carry that structural influence. The BGS pattern is the model: the kernel's uncontested formal core stands upstream, and the contested interpretive claims decompose downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
