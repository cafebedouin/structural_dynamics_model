% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission — Symbol Continuity Reading (High-Fidelity Ritual Mandate)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   A catastrophe-survivor community dispersed across host societies
 *   maintains a high-fidelity ritual transmission regime: fixed mourning
 *   rites, a synchronized commemorative calendar, certified officiants, and
 *   council review of any proposed variation. This file instantiates ONE
 *   reading of the contested kernel catastrophe_memory_transmission — the
 *   symbol-continuity reading, on which ritual preserves identity and
 *   mourning-practice as an intrinsic communal good and the transmission of
 *   symbolic form is itself the mechanism by which the community survives.
 *   The epsilon referent is the standing arrangement under contest — the
 *   fidelity mandate as this reading assesses it — never some reformed
 *   alternative: it delivers real identity continuity and genuine mourning
 *   function while sacrificing the adaptive capacity of the young and
 *   foreclosing reformers. Sibling readings (operational_competence,
 *   hybrid_embedded) are separate constraints with their own epsilon,
 *   beneficiaries, and victims; they are linked through the network, not
 *   averaged here. Claim and metrics are authored independently: the claimed
 *   type is tangled_rope because the structure possesses both a genuine
 *   coordination function (cross-generational identity and mourning
 *   transmission) and asymmetric extraction requiring active enforcement; the
 *   metrics describe the arrangement's operation as the historical record
 *   shows it.
 *
 * KEY AGENTS:
 *   - ritual_custodian_councils: Agenda-setting beneficiary (organized/identity_locked) — fixes the rite, certifies authenticity, collects the deference and office-continuity the mandate generates
 *   - commemorative_institutions: Secondary beneficiary (organized/constrained) — memorial schools, foundations, and museums whose budgets and purposes ride the ritual calendar
 *   - catastrophe_survivor_generation: Founding beneficiary (organized/trapped) — authorized the mandate; their assurance of remembrance runs entirely through it
 *   - younger_generation_members: Primary target (moderate/constrained) — bear the training burden, life-choice restrictions, and innovation-suppression costs
 *   - liturgical_reform_advocates: Targeted insiders (moderate/identity_locked) — propose variation and pay in standing and office
 *   - secular_memory_archivists: Excluded voice (institutional/mobile) — documentation-based remembrance kept outside the councils' process
 *   - ritual_studies_analysts: Analytical observer (analytical/analytical) — comparative view of the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission — Symbol Continuity Reading (High-Fidelity Ritual Mandate)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, '3b73697f-4cfc-43ae-bc3b-4abeb6c56a80').
narrative_ontology:cs_kernel_codification('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', fixed_text).
narrative_ontology:cs_authority_grounding('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', lineage).
narrative_ontology:cs_interpretation_layer_present('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80').
narrative_ontology:cs_reading_relation('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', foundational, mourning_form_is_intrinsic_communal_good).
narrative_ontology:cs_axiom_status(mourning_form_is_intrinsic_communal_good, holdable).
narrative_ontology:cs_axiom_grounding('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', mourning_form_is_intrinsic_communal_good, deontological).
narrative_ontology:cs_axiom('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', foundational, form_transmission_is_the_survival_mechanism).
narrative_ontology:cs_axiom_status(form_transmission_is_the_survival_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', form_transmission_is_the_survival_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', inherited_rite_as_identity_substance).
narrative_ontology:cs_drift_state('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', third_post_catastrophe_generation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3b73697f-4cfc-43ae-bc3b-4abeb6c56a80', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, ritual_custodian_councils).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, commemorative_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_members).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, liturgical_reform_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_survivor_generation).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, symbolic_form_continuity_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, fidelity_as_survival_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elder councils and liturgical boards that fix the order of mourning rites, certify which performances count as authentic, and train the next cadre of officiants. They decide which proposed variations are admitted and which are ruled corruptions. Their office, standing, and succession depend on the continuation of the fidelity schedule they administer; stepping back from enforcement would leave the office without a function. Exit for a council member means resignation into the ordinary congregation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_custodian_councils, agenda_setter,
    organized, generational, identity_locked, continental).

% Memorial schools, foundations, and museums whose calendars, curricula, and fundraising appeals are built around the ritual cycle. Each mandated observance generates enrollment, donations, and programming; their budgets and stated purposes track the fidelity schedule. They do not run the enforcement machinery, but their viability rides on it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, commemorative_institutions, beneficiary,
    organized, generational, constrained, national).

% The rupture generation who lived the catastrophe and composed the rites in its aftermath. They receive the assurance that the dead will be mourned in recognizable form and that the community will not dissolve into its host societies. They authorized the original fidelity requirements and treated deviation as betrayal of the dead. Their remaining leverage on being remembered runs entirely through the rites they fixed; they cannot revise what they have sanctified without unmaking their own witness.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_survivor_generation, beneficiary,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_survivor_generation, agenda_setter).

% Members born after the catastrophe who inherit the obligation to learn, perform, and pass on the rites. The mandate costs them years of language study and ritual training, restricts marriage, residence, and career choices compatible with the observance calendar, and marks their own innovations as corruptions. Leaving is possible — assimilation into host societies — but it severs family ties, communal belonging, and the mourner's relationship to their own dead; staying means bearing a schedule and identity they did not choose.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_members, payer,
    moderate, biographical, constrained, global).

% Insiders — cantors, educators, younger clergy — who propose abbreviated, translated, or musically updated forms and petition the councils for admission of variation. Their proposals are heard and routinely rejected as dilution; repeated advocacy costs them standing, appointments, and invitations to officiate. They stay inside the tradition because the tradition is what they are trying to save; leaving would end their standing to argue.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, liturgical_reform_advocates, payer,
    moderate, biographical, identity_locked, regional).

% Historians, oral-history archivists, and documentary filmmakers who preserve catastrophe memory through testimony recording, annotation, and open access rather than mandated performance. They argue remembrance survives translation and abbreviation and that fidelity mandates accelerate youth disaffection. They sit outside the councils' consultative process; their materials are cited when convenient and their recommendations are not voted on.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, secular_memory_archivists, excluded,
    institutional, generational, mobile, global).

% Scholars of ritual, collective memory, and disaster sociology who compare high-fidelity and adapting communities across catastrophes. They observe the full structure — transmission outcomes, enforcement costs, dropout trajectories — without administering or bearing the arrangement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__symbol_continuity_reading, ritual_custodian_councils).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of transmitting communal identity and mourning practice across generations after rupture: standardizes the rite, synchronizes the commemorative calendar across scattered host societies, and guarantees each generation receives the same forms — solved once, centrally, instead of per-family improvisation.
% TRANSFER_FUNCTION: Moves conformity-labor and adaptive discretion from younger members and would-be reformers to the custodial establishment: time, behavioral compliance, and the right to modify practice flow upward; recognition, remembrance-assurance, and communal standing flow back down.
% ABSENT_VOICES: Secular memory archivists and historians (documentation-based remembrance), trauma-informed pastoral voices who would prioritize mourning's psychological function over its form, and assimilated descendants who left and would testify to the severance costs — none sit on the custodial councils. The councils' unanimity about fidelity arises partly because these dissenting seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the fidelity mandate vanished overnight, rites would diverge community-by-community within a generation, commemorative institutions would lose their programming spine and funding rhythm, custodial offices would hollow out, and memory-work would migrate to archives and families. The diaspora's identity infrastructure would reorganize around voluntary, variable practice — a different world, arrived at quickly.
% FOUNDING_PROBLEM: After the catastrophe, survivors faced dissolution: communities scattered across host societies, transmission chains broken, and a fear that the dead would go unmourned and the communal identity extinguished within two generations. The fidelity mandate was built to guarantee that every future generation received the forms intact.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: survivor testimony archives and independent historiography of the rupture corroborate the founding transmission crisis as real and urgent; sociological surveys of the second and third generations corroborate that the forms were successfully transmitted while documenting retention and adaptation costs. No source outside the custodial establishment attests that the founding-crisis urgency persists unchanged; the assertion of continuing existential urgency rests on the councils alone.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end) because the mandate's costs — years of language and ritual training, restricted marriage and residence, foreclosed liturgical adaptation — fall on identifiable actors while the coordinating good is collectively enjoyed. Suppression (0.60) reflects an enforcement apparatus that matured over the interval: informal survivor moral authority at t0 hardened into councils, authenticity certification, and sanction of deviant performance by tn; roughly sixty percent of the suppressive force is structural (gatekeeping, marriage-market and communal pressure) and forty percent internalized (filial duty, guilt toward the dead), a split the omega variables carry forward. Theater rises from 0.12 to 0.31: early enforcement was almost entirely functional transmission, while a growing share of custodial activity is anniversary reaffirmation, authenticity dispute, and public fidelity declaration that maintains the office rather than the memory. Accessibility collapse is moderate (0.45) because alternatives genuinely persist — documented remembrance, abbreviated domestic observance, assimilation — but each carries heavy identity and social cost. Resistance (0.50) is real: youth disaffection, attrition, and recurring reform petitions. Identity-lock operates differently by seat: custodians exhibit institutional identity fusion (the office has become its function), reform advocates relational-ideological fusion (they cannot argue for the tradition from outside it), and younger members a mix that varies individually — the constrained exit atom averages this heterogeneity. All three temporal series run on one shared seven-point grid so no metric's end-state is silently substituted into earlier decades.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the custodian seat should compute differently from the same structural data. From the councils' position the mandate is the community's covenant with its dead and the source of their own office's meaning; from the younger members' position it is an inherited schedule of obligation they did not consent to; from the reform advocates' position it is a door that closes on every proposal they make; from the survivor generation's position it is a promise kept. Same nominal community membership, radically different exits: the councils cannot abandon enforcement without dissolving their office, the reformers cannot leave without losing standing to argue, the young can leave at the price of severance, and the founders cannot exit at all. The engine computes these per-seat classifications from power, horizon, and exit atoms; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual_custodian_councils and commemorative_institutions are declared beneficiaries and sit near the beneficiary end of directionality — the mandate subsidizes their authority and budgets respectively. Catastrophe_survivor_generation sits nearest the full-beneficiary pole: they receive remembrance-assurance and bear almost none of the ongoing conformity cost. Younger_generation_members and liturgical_reform_advocates are declared victims and sit near the target end; the reform advocates are pushed further toward full-target by their identity-locked exit (trapped advocates pay more than mobile ones), while the younger generation's constrained-but-real exit path (assimilation) moderates their effective extraction slightly below the locked seats. Secular_memory_archivists stand outside the structure — excluded rather than coordinated — and the analysts observe. Scope amplification applies modestly: the mandate spans a global diaspora, making uniform verification hard and favoring the enforcing center.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rupture-era transmission collapse, scattered communities, fear that the dead would go unmourned and the identity extinguish — was real and is largely solved: the forms were transmitted. Yet the arrangement persists at full stringency, and whether the problem it was built for still obtains is itself disputed (assimilation pressure keeps a version of the transmission crisis live). The tangled_rope classification prevents two symmetrical errors: reading the mandate as pure extraction ignores the genuine cross-generational coordination it performs and the sincere intrinsic-good conviction among custodians; reading it as pure coordination ignores the measurable asymmetric costs borne by identifiable actors who receive no compensating benefit. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): this is not the dead-plus-rearranges zombie flag — the honest state is a partially obsolesced mandate whose obsolescence is itself the object of communal dispute. If the custodial seat ever resolves its omega toward office-preservation, the classification should be revisited toward the snare end; if the hybrid reading's embedded-competence claim is vindicated, the extraction component shrinks toward coordination cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint instantiates the symbol-continuity reading of the catastrophe_memory_transmission kernel; the disagreement with the operational-competence and hybrid-embedded siblings is located in what the transmitted content IS — intrinsic symbolic form versus propositionalizable survival competence versus non-propositional knowledge embedded in practice. Which account of the transmitted content is correct?',
    'Comparative outcome studies of high-fidelity versus adapting communities across multiple catastrophe cohorts, controlling for initial human and material capital, combined with close ethnography of whether ritual performance actually rehearses operational skills.',
    'If competence transmission is real and separable, part of the measured cost is rehearsal price rather than sacrifice, epsilon falls and the reading drifts toward the hybrid account; if form-preservation shows no survival dividend beyond identity satisfaction, the fidelity mandate loses its coordination warrant and the arrangement trends toward pure enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Which account of what ritual transmits distinguishes this reading from its siblings.').

omega_variable(
    counterfactual_adaptive_capacity,
    'How much adaptive capacity is actually sacrificed to form-preservation, given that the counterfactual — the same community modifying its rites freely — is unobservable?',
    'Natural experiments: communities that relaxed fidelity after specific crises (post-war liturgical reforms, post-disaster simplifications) compared with matched high-fidelity communities on cohesion, retention, and crisis response over subsequent decades.',
    'If adapting communities retain cohesion and respond better to new threats, the sacrifice is real and the extraction component stands; if they fragment, fidelity''s protective value rises and effective extraction falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_adaptive_capacity, empirical, 'Measurement problem for the sacrificed-adaptive-capacity victim claim.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression that keeps younger members conforming structural (communal sanction, marriage and residence pressure, institutional gatekeeping) or internalized (filial duty, guilt toward the dead, identity fusion with the rite)?',
    'Post-exit trajectory of leavers: if duty, guilt, and ritual longing persist after assimilation removes all structural pressure, a large share of suppression is internalized.',
    'If internalized, effective suppression exceeds the structural measure — leavers carry the mandate with them — and relaxing external enforcement would not release the young; the classification-relevant suppression is higher than the authored scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the mandate''s suppressive force.').

omega_variable(
    intrinsic_good_or_authority_cover,
    'Is mourning-form preservation genuinely held as an intrinsic communal good, or is it the cover under which custodial authority reproduces itself?',
    'Revealed preference under cost: when fidelity maintenance becomes expensive enough (membership decline, funding stress), do custodians admit variation to save the community, or tighten enforcement to save the office?',
    'If custodial behavior tracks communal survival, the coordination half of the arrangement is genuine; if it tracks office-preservation, the mandate is authority reproduction wearing mourning''s clothing and the arrangement trends toward pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_good_or_authority_cover, conceptual, 'Whether the reading''s own intrinsic-good axiom describes custodial motivation or masks office-preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symcont_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(symcont_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(symcont_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(symcont_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(symcont_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(symcont_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(symcont_tr_t60, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 60, 0.31).

% Extraction over time
narrative_ontology:measurement(symcont_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(symcont_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(symcont_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(symcont_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(symcont_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(symcont_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(symcont_be_t60, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(symcont_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(symcont_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(symcont_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(symcont_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(symcont_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(symcont_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.59).
narrative_ontology:measurement(symcont_su_t60, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 60, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'ritual preserves the community after catastrophe' decomposes into three structurally distinct claims about what ritual transmits: survival competence (operational_competence_reading), competence embedded in form (hybrid_embedded_reading), and intrinsic symbolic form whose transmission is itself survival (this file). Each claim has its own epsilon, beneficiary/victim structure, and failure modes; forcing one story to carry all three would make epsilon observer-relative, violating the invariance principle. Family links run through network.affects_constraints: the upstream empirical question of what transmission actually delivers conditions the downstream classifications of the normative and hybrid readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
