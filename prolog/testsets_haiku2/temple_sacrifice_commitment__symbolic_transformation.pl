% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment — Symbolic Transformation Reading
 *   domain: religious_law/commitment_systems
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple in 70 CE, Jewish
 *   communities faced a crisis of commitment: the divine law requiring Temple
 *   sacrifice could no longer be materially instantiated. The rabbinic
 *   authority structure that emerged in the centuries following this
 *   catastrophe made a structural decision: the sacrifice commitment would be
 *   transformed, not suspended. Prayer and study would become the new form of
 *   occupying the divine command — not as temporary substitutes pending
 *   restoration, but as the legitimate instantiation itself. This reading
 *   (symbolic_transformation) represents the mainstream Jewish theological
 *   position: the transformation was authorized by rabbinic authority, is
 *   itself a living mode of divine engagement, and makes the commitment
 *   accessible to diaspora communities without Temple infrastructure. This
 *   reading is contested by literalist and karaite communities, which hold
 *   that the symbolic transformation is an unauthorized drift that preserves
 *   the letter of the law while abandoning its material substance. The
 *   extractiveness emerges from the asymmetry: rabbinic authority claims the
 *   power to redefine what counts as occupying divine law; literalist
 *   communities experience this as having religious agency extracted from
 *   them and subjected to institutional interpretation.
 *
 * KEY AGENTS:
 *   - rabbinic_authority_structure: Institutional agenda-setter; claims power to authorize the transformation and enforce it through halakhic ruling and educational authority
 *   - literalist_halakhic_communities: Payers (victims); hold that material sacrifice is non-negotiable; experience transformation as unauthorized reinterpretation; identity-locked resistance
 *   - mainstream_jewish_communities: Beneficiaries and secondary payers; benefit from accessibility of the transformed commitment; accept rabbinic authority; bear diffuse cost of deferring to institutional interpretation
 *   - karaite_and_separatist_movements: Trapped payers; reject rabbinic authority; institutionally foreclosed from mainstream discourse
 *   - individual_believers_with_uncertainty: Excluded voices; experience internal tension unaddressed by institutional theology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.68).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.72).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Temple Sacrifice Commitment — Symbolic Transformation Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/commitment_systems").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, '5091aed3-f6e1-4e6e-a84d-f551922a8f33').
narrative_ontology:cs_kernel_codification('5091aed3-f6e1-4e6e-a84d-f551922a8f33', fixed_text).
narrative_ontology:cs_authority_grounding('5091aed3-f6e1-4e6e-a84d-f551922a8f33', lineage).
narrative_ontology:cs_interpretation_layer_present('5091aed3-f6e1-4e6e-a84d-f551922a8f33').
narrative_ontology:cs_reading_relation('5091aed3-f6e1-4e6e-a84d-f551922a8f33', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('5091aed3-f6e1-4e6e-a84d-f551922a8f33', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('5091aed3-f6e1-4e6e-a84d-f551922a8f33', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_axiom('5091aed3-f6e1-4e6e-a84d-f551922a8f33', foundational, transformation_constitutes_true_occupancy).
narrative_ontology:cs_axiom_status(transformation_constitutes_true_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('5091aed3-f6e1-4e6e-a84d-f551922a8f33', transformation_constitutes_true_occupancy, deontological).
narrative_ontology:cs_axiom('5091aed3-f6e1-4e6e-a84d-f551922a8f33', foundational, rabbinical_authority_to_reinterpret_is_divinely_sanctioned).
narrative_ontology:cs_axiom_status(rabbinical_authority_to_reinterpret_is_divinely_sanctioned, holdable).
narrative_ontology:cs_axiom_grounding('5091aed3-f6e1-4e6e-a84d-f551922a8f33', rabbinical_authority_to_reinterpret_is_divinely_sanctioned, conventional).
narrative_ontology:cs_reference_frame('5091aed3-f6e1-4e6e-a84d-f551922a8f33', material_temple_sacrifice_as_binding).
narrative_ontology:cs_drift_state('5091aed3-f6e1-4e6e-a84d-f551922a8f33', post_temple_destruction_diaspora_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5091aed3-f6e1-4e6e-a84d-f551922a8f33', '2026-08-03T14:32:18Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, literalist_halakhic_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, mainstream_jewish_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, mainstream_jewish_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, karaite_and_separatist_movements).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, authority_to_reinterpret_divine_command).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, rabbinical_succession_as_living_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the interpretive framework that authorized the transformation of Temple sacrifice law into prayer and study. Argues this transformation is itself a divinely-sanctioned instantiation of the original commitment, not a suspension pending restoration. Derives legitimacy from continuous lineage of interpretation and the claim that reinterpretation is an authorized mode of occupying divine law. Enforces the transformation through halakhic ruling, liturgical standardization, and educational authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, analytical, global).

% Hold that material Temple sacrifice is the only true occupancy of the divine command. They experience the transformation as an unauthorized drift that abandons rather than reinterprets the commitment. For them, prayer and study are noble but structurally different — they lack the material instantiation and functional completeness of actual sacrifice. Exit for this community means either doctrinal heresy (accepting the rabbinically-authorized transformation as legitimate) or geographic/institutional withdrawal to parallel communities that reject the symbolic transformation. Identity fusion with the literalist position makes exit costly even when theoretically available.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, literalist_halakhic_communities, payer,
    moderate, civilizational, identity_locked, global).

% Benefit from the transformation because it makes the divine commitment occupiable without Temple facilities, geographic access, or material resources. They participate fully in the rabbinically-authorized prayer and study regime and derive legitimacy from rabbinic teaching that this is true performance, not substitution. They also bear a diffuse cost: the acceptance of rabbinical authority to redefine divine command; the foreclosure of literalist readings as live options within the tradition; the need for continuous rabbinical interpretation to justify why this reading is the binding one.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, mainstream_jewish_communities, beneficiary,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, mainstream_jewish_communities, payer).

% Reject rabbinical authority to authorize the transformation and maintain that the original divine command remains binding in its literal form. They treat the symbolic transformation as an illegitimate drift imposed by institutional power, not as a legitimate reinterpretation. They are trapped by institutional marginalization and historical dispersion; their theological objection has been structurally foreclosed within mainstream Jewish institutions through rabbinic enforcement.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, karaite_and_separatist_movements, payer,
    moderate, civilizational, trapped, regional).

% Experience internal tension between accepting the rabbinical authorization of the transformation and honoring impulses toward material restoration. This group's perspective — whether the transformation is truly occupying the commitment or merely deferring it — is largely absent from institutional discourse. They lack institutional voice to contest the binding authority of the transformation, and their doubts are typically addressed through pastoral instruction rather than theological engagement as peers.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, individual_believers_with_uncertainty, excluded,
    powerless, biographical, identity_locked, local).

% Examines the structural features of how the transformation was authorized, what counts as occupying a divine commitment when material conditions change, and how authority structures claim the power to redefine inherited law. This seat brings no institutional stake but holds the perspective that the authorization mechanism itself — not the content of the decision — is the analytically interesting fact.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous link between the Jewish people and the divine commitment to sacrifice, despite the absence of Temple facilities (destroyed in 70 CE) and the impossibility of material implementation. The transformation solves the problem: how does a diaspora community occupy a law whose material conditions of performance have been destroyed? Prayer and study as substitutes for sacrifice keep the commitment alive in collective memory and individual piety.
% TRANSFER_FUNCTION: Moves interpretive authority from the literal text and its observable conditions to the rabbinical institution that claims the power to redefine what counts as occupying divine law. The material economy of sacrifice (animals, ritual objects, a functioning priesthood) is replaced by the institutional economy of interpretation (authority to declare prayer equivalent to sacrifice, legitimacy to teach the transformation as binding). Communities that reject rabbinical authority experience this as extraction of religious legitimacy and agency.
% ABSENT_VOICES: Literalist and karaite communities object that the transformation is unauthorized drift, but this objection has been institutionally foreclosed. Individual believers who experience genuine uncertainty about whether prayer truly occupies the original commitment are not represented in rabbinic discourse as theological peers — they are treated as objects of instruction. A hypothetical voice asking 'do we have authority to redefine this commitment at all?' is structurally excluded from the framework that presupposes rabbinical authority to do exactly that.
% DISAPPEARANCE_RATIONALE: If the symbolic transformation and its enforcement vanished, mainstream Jewish communities would face an immediate mandate to restore material sacrifice, which is practically impossible. This would either force doctrinal revision back to literalism (acknowledging the original command is superseded) or create permanent non-compliance. The transformation itself cannot disappear without forcing this choice — its disappearance is not a return to a neutral state but a rearrangement to one of the alternative readings (performance_only or study_as_exercise). Whether this counts as 'rearrangement' depends on whether one sees the transformation as occupying the commitment or suspending it.
% FOUNDING_PROBLEM: The Temple was destroyed and material sacrifice became impossible. Jewish communities had to answer: does the commitment to sacrifice persist as binding law even without conditions of performance? If it persists, how is it now to be occupied?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources from the 2nd century CE onward document the explicit deliberation: the commitment is treated as continuously binding; prayer and study are authorized as its new occupancy. Literalist and karaite sources from subsequent centuries corroborate that the transformation was contested — they reject it as unauthorized. Contemporary Jewish scholarship (outside the benefiting rabbinic establishment) confirms both the historical fact of authorization and the continuing dispute over its legitimacy. The founding problem remains live because the absence of Temple conditions persists, and the authority to redefine the commitment remains contested.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because the rabbinic transformation claims the power to redefine the boundary between true occupancy and false occupancy of a divine command. This is not a low-cost coordination move — it restructures who gets to interpret the law and what counts as faithful practice. Suppression is high (0.72) because the transformation is actively enforced: literalist readings are marginalized, karaite communities are institutionally isolated, and individual doubt is addressed through instruction rather than theological debate as peers. Theater is moderate (0.48) because the transformation includes genuine piety and intellectual engagement, but an increasing share of its maintenance cost is spent defending the authorization itself rather than serving the substantive commitment. The measurement series show the trajectory: in the first centuries after the Temple's destruction, extractiveness and suppression are lower because the transformation is still being actively negotiated. As institutional rabbinic authority consolidates (centuries 2-10), extractiveness rises as the power to authorize becomes less contested. From century 10 onward, the metrics plateau — the transformation is institutionally secure, which means suppression and extraction reach their stable level (the cost of maintaining the authorization against ongoing literalist objection).
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority seat and the literalist victim seats compute radically differently. From the rabbinical position, the transformation is genuine theological innovation — the commitment itself is made more intelligible and more widely accessible by reinterpreting its instantiation. Prayer and study occupy the commitment authentically because the rabbinic reading holds that the divine command was always about maintaining the relationship between God and Israel, and Temple sacrifice was the form of that relationship given certain historical conditions; when those conditions changed, the form changed but the relationship persists. From the literalist position, this reasoning is exactly the problem: it subordinates the explicit material command to a rabbinically-reinterpreted spiritual essence, which means the rabbinical authority has claimed the power to decide what the divine command 'really' meant all along. The literalist seat experiences this as the extraction of religious agency — the ability to read the law directly is subordinated to the institutional interpretation. The mainstream Jewish seat bridges this but at cost: they benefit from accessibility but must accept that their occupancy of the commitment depends on rabbinic legitimacy claims. The engine's per-seat classification computation should show: rabbinic authority as rope or beneficiary (low extraction from their own seat); literalist communities as snare or high-extraction tangled rope (actively suppressed, identity-locked, no exit without heresy); mainstream communities as moderate-extraction rope (coordination benefit offset by institutional dependence).
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic authority structure occupies the beneficiary position: they gain interpretive power, institutional control of pedagogy, and the authority to define orthodoxy. Their directionality is low (near 0.0 — the constraint subsidizes their position by centralizing authority in their hands). The literalist communities are the victims: they lose the ability to claim that their literal reading occupies the commitment; they are institutionally marginalized; their position is treated as archaic rather than as a live theological option. Their directionality is high (near 1.0 — the constraint extracts religious legitimacy from their position). Mainstream communities sit near d=0.5: they benefit from accessibility (low directionality component), but they depend on accepting rabbinic authority (high directionality component). The identity-locked exit for literalist communities pushes their d higher than mere constrained exit would — accepting the transformation means abandoning foundational identity, which makes exit not just costly but identity-annihilating.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how does the commitment persist without Temple conditions) is live: the Temple remains absent 2000 years later. The declared transformation solves it by redefining what counts as occupancy — but this solution is controversial precisely because it transfers definitional power to the rabbinic authority structure. A non-extractive rope reading would require the transformation to be accepted as legitimate by all parties; the high suppression (0.72) and resistance (0.59) indicate this is not the case. The constraint is tangled rope, not rope: it provides genuine coordination function (keeps the commitment alive in diaspora conditions) but does so through asymmetric extraction (rabbinical authority claims power to redefine; literalist communities are foreclosed from contesting this as peers). Without the suppression and marginalization of literalist readings, this would be pure rope — a coordination solution everyone accepts. With the suppression, it is tangled rope: the coordination function rides on institutional power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorization_legitimacy_contest,
    'What is the source and scope of rabbinical authority to redefine the meaning of a divine command? Is this authority itself divinely sanctioned, or is it a human institutional claim that lacks explicit textual grounding?',
    'Textual-historical analysis of rabbinic sources claiming the authority; examination of whether this authority is grounded in explicit biblical/textual mandate or derived from interpretive lineage; comparison to other traditions'' claims about authority to reinterpret sacred law.',
    'If rabbinical authority to redefine is itself divinely sanctioned (the standard rabbinic claim), then the transformation is authorized and occupies the commitment. If rabbinical authority is a human institutional claim without explicit grounding, then the transformation may be unauthorized drift, and the extractiveness would be even higher (the authority structure is extracting legitimacy from the law itself). This resolution would determine whether the constraint remains tangled_rope or reclassifies as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_legitimacy_contest, conceptual, 'Whether rabbinical authority to redefine divine law is itself divinely sanctioned or a human institutional claim.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural — external institutional mechanisms (educational control, denunciation of literalism as heretical) — or internalized — literalist believers have absorbed the rabbinical framing and experience their own position as marginal and indefensible?',
    'Ethnographic observation of literalist communities'' own theological discourse; examination of whether opposition to the transformation persists as active theological contestation or has been internalized as a private doubt; analysis of exit trajectories (do individuals who leave literalist communities do so because they cannot afford the structural costs, or because they have become convinced the transformation is correct?).',
    'If suppression is primarily structural, the constraint''s effective extractiveness may be lower than measured once the structural barriers relax (e.g., internet access to competing theological resources). If suppression is internalized, the extractiveness is higher — the constraint has shaped what people believe is even possible to believe. This affects whether the transition from century 0 to century 2000 represents a stable equilibrium or a metastable state where suppression is doing more work than the metrics show.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural (external institutional barriers) or internalized (doubts absorbed into believers'' own framework).').

omega_variable(
    coordination_vs_extraction_boundary,
    'How much of the measured extractiveness (0.68) is necessary overhead for the coordination function (keeping the commitment alive in diaspora conditions), and how much is institutional rent collection (authority structure claiming power for its own legitimacy)?',
    'Counterfactual analysis: what would a minimally-extractive version of the transformation look like? Could the commitment be kept alive in diaspora through a less centralized rabbinical authority? Could prayer and study be authorized without also authorizing the rabbinical institution to define all interpretations as binding?',
    'If most of the extractiveness is necessary coordination cost, the constraint is legitimately tangled_rope — real coordination that requires some institutional authority. If much of it is institutional rent collection, the true extractiveness of the underlying coordination (prayer/study keeping the commitment alive) is lower, and the constraint should be reclassified as snare (pure extraction in institutional form). This affects whether the transformation is a genuine solution to the founding problem or a power grab disguised as a solution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'How much measured extractiveness is necessary coordination cost vs. institutional rent collection.').

omega_variable(
    kernel_reading_authority_grounding,
    'This reading holds that the rabbinic authority to authorize the transformation is itself divinely granted. But does the reading ground this claim in explicit textual warrant (biblical or early rabbinic sources), or does the authority rest on the claim that the lineage of rabbinic interpretation IS the warrant?',
    'Genealogical tracing of when the claim to redefine divine law first appears; examination of whether it is explicitly justified in early sources or emerges as an implicit assumption in later tradition; analysis of whether the reading can separate the transformation''s theological legitimacy from the rabbinical authority that authorized it.',
    'If the authority is grounded in explicit textual warrant, the reading is robust to challenges about institutional power. If the authority is self-justifying (the lineage IS the warrant), then the reading is more vulnerable to the omega#1 challenge — it may be extracting legitimacy from the kernel itself. This affects whether the transformation is genuinely authorized or is an exercise of institutional authority that claims post-hoc to be authorized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_authority_grounding, conceptual, 'Whether rabbinical authority to transform is grounded in explicit textual warrant or is self-justifying through lineage.').

omega_variable(
    transformation_finality_vs_suspension,
    'Does this reading hold the transformation as permanent and consummatory (the commitment''s true form in diaspora), or as final-pending-restoration (the commitment is fully occupied by prayer/study now, but material sacrifice will be restored in messianic times)?',
    'Close reading of rabbinic and contemporary Jewish theological sources on whether the transformation is treated as the commitment''s permanent reinterpretation or as a holding pattern. Examination of liturgical language: do prayers for restored Temple sacrifice indicate the transformation is deemed temporary?',
    'If the transformation is consummatory, it cannot coexist with hybrid_preparatory (which treats it as suspension). If it is final-pending-restoration, it occupies a middle position where the commitment is fully lived now but its ultimate form includes material restoration. This affects the reading''s foreclosure structure and its coexistence relations with siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformation_finality_vs_suspension, empirical, 'Whether the transformation is treated as permanent reinterpretation or temporary suspension pending restoration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(temp_tr_t0, observed).
narrative_ontology:measurement(temp_tr_t250, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 250, 0.31).
narrative_ontology:measurement_basis(temp_tr_t250, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 500, 0.38).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1000, 0.44).
narrative_ontology:measurement_basis(temp_tr_t1000, observed).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1500, 0.48).
narrative_ontology:measurement_basis(temp_tr_t1500, observed).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 2000, 0.48).
narrative_ontology:measurement_basis(temp_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(temp_be_t0, observed).
narrative_ontology:measurement(temp_be_t250, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 250, 0.48).
narrative_ontology:measurement_basis(temp_be_t250, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 500, 0.58).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1000, 0.64).
narrative_ontology:measurement_basis(temp_be_t1000, observed).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement_basis(temp_be_t1500, observed).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement_basis(temp_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.41).
narrative_ontology:measurement_basis(temp_su_t0, observed).
narrative_ontology:measurement(temp_su_t250, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 250, 0.54).
narrative_ontology:measurement_basis(temp_su_t250, observed).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 500, 0.63).
narrative_ontology:measurement_basis(temp_su_t500, observed).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1000, 0.69).
narrative_ontology:measurement_basis(temp_su_t1000, observed).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1500, 0.72).
narrative_ontology:measurement_basis(temp_su_t1500, observed).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement_basis(temp_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__symbolic_transformation, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the temple_sacrifice_commitment kernel. The symbolic_transformation reading treats the commitment as having undergone authorized reinterpretation: prayer and study are the legitimate occupancy, not substitutes or suspensions. This reading directly influences the performance_only reading (by claiming the transformation is complete, not provisional) and coexists with study_as_exercise (which holds that intellectual engagement occupies the commitment, but through a different epistemic mechanism). The network family is necessary because each reading has different ε values, different victim sets, and different authority-grounding structures — what looks like coordination function (keeping the commitment alive) from one reading looks like extraction (rabbinical power claim) from another. The ε values differ substantially: performance_only has higher extractiveness (the literal reading is suppressed as heretical); study_as_exercise has lower extractiveness (the claim rests on epistemological equivalence, not institutional authority); hybrid_preparatory sits between (the commitment is lived but provisionally). Decomposing into four stories with distinct ε values captures the structural differences that a merged constraint would obscure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__symbolic_transformation, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
