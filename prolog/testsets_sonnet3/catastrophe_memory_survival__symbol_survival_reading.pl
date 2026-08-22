% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual as Symbolic Continuity — Identity Preservation Through Practice Fidelity
 *   domain: religious/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the symbol_survival_reading of the
 *   catastrophe_memory_survival kernel: the claim that Jewish communal
 *   survival across historical catastrophe is constituted by continuity of
 *   ritual practice and boundary-norms as symbolic experience, independent of
 *   any practical-knowledge content those rituals might also carry. On this
 *   reading, the mechanism of survival IS fidelity to correct symbolic form,
 *   and rabbinic/institutional authority to certify that form is therefore
 *   not incidental machinery but the very locus where survival is enacted.
 *   This produces a high-ε reading because interpretive control over 'correct
 *   practice' becomes a gatekeeping resource with real stakes (marriage,
 *   status, burial, belonging) for those whose practice has drifted, while
 *   the coordination benefit (group legibility, calendar structure, mutual
 *   recognition) is real but increasingly used to justify extraction of
 *   communal legitimacy toward institutions that administer the symbolic
 *   standard. This is a distinct constraint from the sibling readings, not a
 *   different measurement of the same one: competence_transmission_reading
 *   locates survival in embedded practical knowledge (much lower ε,
 *   near-rope, since practical knowledge transmission has few identifiable
 *   losers), and hybrid_encoding_reading splits the difference structurally.
 *   Each reading has its own beneficiary/victim structure and its own ε; per
 *   the ε-invariance principle they are three separate stories linked by
 *   network edges, not three measurements of one constraint.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: institutional/arbitrage — sets and interprets the standard of correct symbolic practice; benefits from being indispensable to survival on this reading
 *   - orthodox_institutional_bodies: organized/mobile — certifying institutions whose legitimacy rests on the symbol-survival premise
 *   - secularized_jews: moderate/constrained — bear status and belonging costs for practice drift under this reading
 *   - intermarried_families: moderate/constrained — sharpest victims of the boundary-norm enforcement (status, marriage, burial gates)
 *   - diaspora_youth_disengaging: powerless/trapped — inherit ritual obligation as a survival mandate with no legitimated exit
 *   - holocaust_survivor_descendants: moderate/constrained — excluded voice; often holds a practical-adaptation account of what actually enabled survival
 *   - academic_historians_of_jewish_practice: analytical/analytical — external observer comparing the symbol-survival claim against the historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual as Symbolic Continuity — Identity Preservation Through Practice Fidelity").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, 'e124f160-44af-40d8-8250-2cbffa2e4427').
narrative_ontology:cs_kernel_codification('e124f160-44af-40d8-8250-2cbffa2e4427', distributed).
narrative_ontology:cs_authority_grounding('e124f160-44af-40d8-8250-2cbffa2e4427', lineage).
narrative_ontology:cs_interpretation_layer_present('e124f160-44af-40d8-8250-2cbffa2e4427').
narrative_ontology:cs_reading_relation('e124f160-44af-40d8-8250-2cbffa2e4427', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('e124f160-44af-40d8-8250-2cbffa2e4427', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('e124f160-44af-40d8-8250-2cbffa2e4427', foundational, symbolic_form_fidelity_constitutes_survival).
narrative_ontology:cs_axiom_status(symbolic_form_fidelity_constitutes_survival, holdable).
narrative_ontology:cs_axiom_grounding('e124f160-44af-40d8-8250-2cbffa2e4427', symbolic_form_fidelity_constitutes_survival, conventional).
narrative_ontology:cs_axiom('e124f160-44af-40d8-8250-2cbffa2e4427', secondary, interpretive_authority_over_practice_is_indispensable_to_continuity).
narrative_ontology:cs_axiom_status(interpretive_authority_over_practice_is_indispensable_to_continuity, holdable).
narrative_ontology:cs_axiom_grounding('e124f160-44af-40d8-8250-2cbffa2e4427', interpretive_authority_over_practice_is_indispensable_to_continuity, conventional).
narrative_ontology:cs_reference_frame('e124f160-44af-40d8-8250-2cbffa2e4427', post_catastrophe_boundary_maintenance_imperative).
narrative_ontology:cs_drift_state('e124f160-44af-40d8-8250-2cbffa2e4427', contemporary_secular_diaspora, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e124f160-44af-40d8-8250-2cbffa2e4427', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, orthodox_institutional_bodies).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, intermarried_families).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, diaspora_youth_disengaging).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and interprets which ritual forms count as authentic continuity, adjudicates deviations, and controls conversion, marriage, and communal-recognition gates. Frames symbolic fidelity as the mechanism of Jewish survival after catastrophe, which positions rabbinic interpretive authority as indispensable to that survival and channels communal legitimacy and resources toward institutions it controls.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, beneficiary).

% Synagogues, day schools, and kashrut boards derive funding, membership, and authority from being the certified custodians of correct practice. Their institutional standing depends on the claim that ritual form itself, not adapted or reinterpreted content, is what carried the community through catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, orthodox_institutional_bodies, beneficiary,
    organized, generational, mobile, national).

% Have drifted from strict ritual observance while retaining Jewish identity through culture, ethics, or memory rather than symbolic practice. Under the symbol-survival framing they are read as a failure of continuity or as at risk of assimilation, which costs them communal standing, recognition of their children's status, and inclusion in rites of passage — even though their felt identity and ethical transmission persist without the ritual form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, constrained, national).

% Bear the sharpest cost of the boundary-norm reading: their children's status, ability to marry within the community, and burial rights are contested precisely because the constraint locates survival in unbroken symbolic-practice lineage rather than in any other form of continuity. Exit means accepting exclusion from communal ritual life; staying means submitting to a status hierarchy they did not choose.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, intermarried_families, payer,
    moderate, biographical, constrained, national).

% Young people raised inside communities that measure survival by fidelity to symbolic practice often experience ritual obligation as inherited weight rather than chosen meaning. Disengagement is treated as loss or betrayal rather than legitimate adaptation, foreclosing alternative modes of Jewish continuity (ethical, cultural, political) that this reading does not recognize as survival at all.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, diaspora_youth_disengaging, payer,
    powerless, biographical, trapped, global).

% Carry direct family memory of catastrophe and often hold complex, non-institutional views on what actually enabled survival — practical adaptation, secrecy, flight, mutual aid — that do not center ritual symbolism. Their testimony is folded into commemorative liturgy rather than consulted as a competing account of what continuity required.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, holocaust_survivor_descendants, excluded,
    moderate, generational, constrained, global).

% Study the historical record of ritual adaptation, syncretism, and practical knowledge transmission across catastrophes (expulsion, pogrom, Shoah) and can compare the symbol-survival account against documented evidence of practical and hybrid transmission mechanisms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, academic_historians_of_jewish_practice, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared ritual practice genuinely coordinates group identity, provides a stable calendar and life-cycle structure, and supplies a legible signal of belonging that lowers the cost of mutual recognition and support within a dispersed, historically persecuted population.
% TRANSFER_FUNCTION: Moves interpretive authority, communal legitimacy, and gatekeeping power (over marriage, burial, conversion, and communal recognition) to institutions and rabbinic figures who certify correct practice, while moving belonging, status, and inclusion costs onto those whose practice has drifted from the certified form.
% ABSENT_VOICES: Secularized and disengaged Jews, intermarried families, and survivors whose own accounts emphasize practical adaptation over symbolic fidelity are rarely given standing to redefine what counts as 'survival' within the institutions that administer that definition; their accounts are absorbed into commemorative liturgy rather than treated as competing evidence.
% DISAPPEARANCE_RATIONALE: If symbolic-fidelity-as-survival collapsed as the dominant frame, rabbinic and institutional gatekeeping over status and belonging would lose its primary justification and likely loosen considerably — a real rearrangement for those seats. But identity and memory practices among Jewish communities would very likely continue in adapted, hybrid, or purely cultural forms, so whether 'the world' rearranges depends entirely on which seat is asked: institutions say catastrophically yes, disengaged and secular Jews say largely no.
% FOUNDING_PROBLEM: After catastrophic ruptures (expulsion, pogrom, the Shoah), communities faced a genuine crisis of continuity: dispersed, traumatized populations needed some mechanism to remain recognizably one people across generations and geographies without a state or unified institution to hold them together.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic and institutional bodies attest the problem remains fully live and that symbolic practice fidelity is the proven mechanism. Independent historians of Jewish social history and demographers of contemporary Jewish identity (outside the institutions that administer ritual certification) document that continuity has in fact been substantially carried by ethnic self-identification, cultural transmission, and communal memory independent of strict ritual observance — suggesting the founding problem has been partially resolved by mechanisms this reading does not credit.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 and rising over the 70-unit interval because the story traces institutional gatekeeping (marriage, conversion, burial recognition) hardening as secularization increases the population it must classify as deficient — the more people drift from certified practice, the more the boundary-maintenance apparatus has to work, and the higher the stakes of its verdicts become. Theater ratio rises from 0.2 to 0.42 as an increasing share of ritual maintenance activity is oriented toward demonstrating fidelity and policing boundaries rather than toward the coordination function itself (shared calendar, mutual recognition) that originally justified it. Suppression is moderate (0.58) and rises only gradually — this is not primarily coercive suppression but exclusion-based: the mechanism is denial of status and belonging rather than physical or legal coercion, which caps how high suppression can honestly be authored relative to a state-enforced constraint. Accessibility collapse (0.6) reflects that once a person or family drifts from certified practice, re-entry into full communal standing becomes structurally difficult, though not impossible (hence moderate rather than mountain-level). Resistance (0.55) reflects real, organized pushback from secular, Reform, Reconstructionist, and intermarried constituencies who dispute the symbol-survival premise itself.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic/institutional seat, this is coordination in its purest form: a mechanism that held a dispersed, persecuted people together across centuries, now under threat from assimilation. From the secularized or intermarried seat, the identical mechanism operates as an enforced boundary that assigns them deficient status for an identity they experience as intact. The engine should compute these as different seat-level classifications from the same structural data — the institutional seat closer to rope/tangled_rope, the payer seats closer to snare — without either side's felt experience adjudicating which reading is 'true.'
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and orthodox institutional bodies are the structural beneficiaries: the symbol-survival premise makes their interpretive function constitutive of group survival itself, which is about as strong a legitimacy claim as an institution can hold, and it channels resources, membership, and deference toward them. Secularized Jews, intermarried families, and disengaging youth are the targets: the same premise that legitimates the institutions is what classifies their lived identity as insufficient or at-risk, with concrete costs in status and inclusion. Diaspora youth are placed at 'trapped' exit rather than 'constrained' because disengagement is not treated as a legitimate alternative identity within the frame — the frame itself has no category for chosen non-ritual continuity as a form of survival, foreclosing the exit rather than merely raising its cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — communal continuity after catastrophic rupture — was genuinely live historically. This reading's classification as tangled_rope rather than snare recognizes that the coordination function (shared calendar, mutual recognition, resilient group identity under persecution) is real and not merely cover; the extraction is asymmetric but layered onto authentic coordination, not substituted for it. The mandatrophy risk here is that the mechanism (ritual-form fidelity) has, per the founding_problem_corroboration, been partially superseded by other, non-ritual continuity mechanisms (cultural identification, ethnic self-identity, secular communal institutions) that demographic evidence shows are doing real continuity work — yet the institutions whose authority depends on ritual-fidelity-as-survival have structural reasons not to recognize this, which is exactly the founding_problem_status='contested' signal the R5 interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbol_vs_competence_survival_mechanism,
    'Did historical Jewish communal survival across catastrophe actually depend on symbolic ritual fidelity per se, or on the practical/adaptive knowledge often embedded within ritual practice (timing of holidays for agricultural/seasonal coordination, dietary law as historical health practice, communal mutual-aid protocols enacted through ritual occasions)?',
    'Historical and anthropological comparison of communities that maintained symbolic form with declining practical content versus communities that adapted symbolic form while retaining practical function, tracking which better predicted continuity outcomes across documented catastrophes (expulsion, pogrom, Shoah, Soviet suppression).',
    'If practical/competence content is the actual survival mechanism, this symbol_survival_reading is authoring high ε for a control function riding on a coordination story that is not the true mechanism — closer to the tangled_rope/snare boundary than authored. If symbolic fidelity independently matters (identity signaling has real coordination value even absent practical content), the tangled_rope classification with genuine coordination function is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbol_vs_competence_survival_mechanism, empirical, 'Whether ritual symbolism or embedded practical knowledge is the true survival-carrying mechanism — the central contest between this reading and its siblings.').

omega_variable(
    gatekeeping_necessity_vs_capture,
    'Is rabbinic interpretive authority over ''correct practice'' a necessary feature of any workable boundary-maintenance coordination mechanism, or has it been captured to serve institutional self-perpetuation beyond what coordination requires?',
    'Comparative study of denominational movements with less centralized gatekeeping (Reform, Reconstructionist, secular humanistic Judaism) to determine whether communal continuity and identity persistence is measurably lower absent centralized ritual-fidelity gatekeeping.',
    'If continuity outcomes are comparable across gatekeeping intensity, the extraction is not coordination-necessary and this reading understates capture; if outcomes diverge sharply, the gatekeeping intensity is closer to a genuine coordination requirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gatekeeping_necessity_vs_capture, conceptual, 'Whether the intensity of institutional gatekeeping is coordination-necessary or captured.').

omega_variable(
    kernel_framing_choice,
    'Is the symbol_survival_reading, the competence_transmission_reading, or the hybrid_encoding_reading the structurally correct account of the catastrophe_memory_survival kernel, and what guided selecting this reading as the one authored here?',
    'This story authors the symbol_survival_reading because it is the reading dominant in institutional Orthodox and rabbinic discourse and the reading with the clearest identifiable beneficiary (interpretive authority) and victim set (those excluded by boundary enforcement) — making it the reading with the sharpest, most falsifiable ε claim among the three. The hybrid_encoding_reading is likely the most empirically defensible account per historical and anthropological literature, but it was decomposed into its own sibling story per the ε-invariance principle rather than blended into this one.',
    'If the hybrid_encoding_reading is adopted as the dominant frame instead, the beneficiary/victim structure softens considerably (practical knowledge transmission benefits nearly everyone, diluting the concentrated extraction this reading identifies), and the classification would likely shift toward rope or a milder tangled_rope. Adopting the competence_transmission_reading exclusively would likely dissolve the tangled_rope classification into rope, since the victim set (those who lose status for ritual drift) has no clear analog if survival is read purely as practical-knowledge transmission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Documents which kernel reading this story instantiates and why, per Rule 2 (committer structure routed to omega, not folded into narrative).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t14, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 14, 0.26).
narrative_ontology:measurement(cata_tr_t28, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 28, 0.32).
narrative_ontology:measurement(cata_tr_t42, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 42, 0.36).
narrative_ontology:measurement(cata_tr_t56, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 56, 0.4).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cata_be_t14, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(cata_be_t28, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 28, 0.6).
narrative_ontology:measurement(cata_be_t42, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 42, 0.63).
narrative_ontology:measurement(cata_be_t56, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 56, 0.66).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cata_su_t14, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 14, 0.48).
narrative_ontology:measurement(cata_su_t28, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 28, 0.5).
narrative_ontology:measurement(cata_su_t42, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 42, 0.53).
narrative_ontology:measurement(cata_su_t56, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 56, 0.56).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__symbol_survival_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the catastrophe_memory_survival kernel. symbol_survival_reading (this file) authors high ε and a tangled_rope classification centered on rabbinic/institutional interpretive gatekeeping over ritual form. competence_transmission_reading authors substantially lower ε, closer to rope, since practical-knowledge transmission has a much thinner victim set. hybrid_encoding_reading sits structurally between the two, crediting both registers and correspondingly diluting the concentrated beneficiary/victim asymmetry this reading identifies. All three share the same underlying kernel text (ritual practice after catastrophe) but instantiate structurally distinct constraints with distinct ε values and distinct stakeholder sets, per the ε-invariance principle — they are not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
