% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Corpus as Substitution Archive: Prayer and Study Replace Sacrifice
 *   domain: religious/commitment_system/rabbinic_judaism
 *
 * SUMMARY:
 *   After the Second Temple's destruction in 70 CE, the Jewish sacrificial
 *   system became impossible. The rabbinic institution developed a
 *   substitution doctrine: prayer and Torah study replace sacrifice as
 *   spiritually equivalent practices. This constraint operates at the level
 *   of legitimacy—what counts as occupying the ancient role. The substitution
 *   reading (this story) claims that Kodashim (the Mishnaic and Talmudic
 *   corpus detailing sacrificial law) is a memorial archive documenting what
 *   was replaced, not a kernel currently occupied through study. This stands
 *   in tension with the sibling reading that study itself occupies the
 *   kernel, and against the literal reading that sacrifice is the true
 *   kernel, merely deferred pending restoration. The constraint's
 *   extractiveness lies in the way it simultaneously claims continuity with
 *   the sacrificial past while denying that the past can be restored. Those
 *   seeking literal sacrifice are told: (1) it is obsolete (discontinuity),
 *   (2) yet we preserve its law (false continuity), (3) study is equivalent
 *   (substitution), (4) but the substitution is permanent, not interim
 *   (foreclosing restoration).
 *
 * KEY AGENTS:
 *   - rabbinic_textual_institution: Authority that authors and administers the substitution doctrine; benefits from institutional centrality in interpreting how substitution works.
 *   - literal_sacrifice_practitioners: Constrained by the doctrine; told their role is obsolete while being positioned outside legitimate Jewish practice.
 *   - prayer_practitioners: The organized Jewish community, told prayer IS the substitute; benefit from continuity claim without requiring sacrifice.
 *   - textual_scholars: Primary beneficiaries; their study gains spiritual efficacy through the substitution doctrine.
 *   - messianic_restorationists: Excluded from the deliberative space; cannot argue that sacrifice is the true kernel awaiting restoration without risking delegitimation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.68).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.72).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.68).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Corpus as Substitution Archive: Prayer and Study Replace Sacrifice").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/commitment_system/rabbinic_judaism").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, 'd7344d92-75e9-4772-ba01-78452d26de69').
narrative_ontology:cs_kernel_codification('d7344d92-75e9-4772-ba01-78452d26de69', fixed_text).
narrative_ontology:cs_authority_grounding('d7344d92-75e9-4772-ba01-78452d26de69', lineage).
narrative_ontology:cs_interpretation_layer_present('d7344d92-75e9-4772-ba01-78452d26de69').
narrative_ontology:cs_reading_relation('d7344d92-75e9-4772-ba01-78452d26de69', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('d7344d92-75e9-4772-ba01-78452d26de69', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_axiom('d7344d92-75e9-4772-ba01-78452d26de69', foundational, substitution_is_permanent_replacement).
narrative_ontology:cs_axiom_status(substitution_is_permanent_replacement, holdable).
narrative_ontology:cs_axiom_grounding('d7344d92-75e9-4772-ba01-78452d26de69', substitution_is_permanent_replacement, deontological).
narrative_ontology:cs_axiom('d7344d92-75e9-4772-ba01-78452d26de69', foundational, archive_function_precludes_restoration).
narrative_ontology:cs_axiom_status(archive_function_precludes_restoration, holdable).
narrative_ontology:cs_axiom_grounding('d7344d92-75e9-4772-ba01-78452d26de69', archive_function_precludes_restoration, conventional).
narrative_ontology:cs_reference_frame('d7344d92-75e9-4772-ba01-78452d26de69', sacrificial_temple_era_baseline).
narrative_ontology:cs_drift_state('d7344d92-75e9-4772-ba01-78452d26de69', contemporary_diaspora_condition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d7344d92-75e9-4772-ba01-78452d26de69', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_textual_institution).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, literal_sacrifice_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, prayer_practitioners).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, textual_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic sages and their heirs control the interpretation and transmission of the Kodashim corpus (tractates of Mishnah and Talmud detailing sacrificial law). They authored the doctrine that prayer and study substitute for Temple sacrifice after the Second Temple's destruction. They administer the textual archive, decide what constitutes legitimate engagement with it (study for spiritual benefit), and enforce the reading that the corpus is pedagogical memorial rather than a blueprint for restoration. They benefit from institutional authority: the more completely study IS the practice, the more indispensable their role as keepers of the text.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_textual_institution, agenda_setter,
    institutional, civilizational, mobile, global).

% Individuals or movements (historically: Samaritans, Karaites, contemporary literalists) who believe the sacrificial system should be restored and performed, not studied as archive. They are told by rabbinic authority that sacrifice is obsolete, that its performance is forbidden (violating rabbinic jurisdiction), and that study substitutes for it. They bear the cost of being positioned as outside legitimate Jewish practice while maintaining that the kernel—physical sacrifice—remains the true occupation. Their exit is blocked by identity: they are Jewish, but the dominant institution denies their reading occupies the kernel.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, literal_sacrifice_practitioners, payer,
    powerless, civilizational, identity_locked, local).

% The Jewish community broadly, who pray three times daily in the traditional structure where each prayer service replaces a corresponding sacrificial offering (morning prayer replaces the morning sacrifice, etc.). They are told prayer substitutes for sacrifice—a substitution the rabbinic institution claims is continuous, maintaining the function under new conditions rather than abandoning it. They benefit from this framing: their prayer practice IS occupying the ancestral role, making them continuous with Temple Judaism without requiring sacrifice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, prayer_practitioners, beneficiary,
    organized, biographical, mobile, global).

% Rabbis, yeshiva students, and academic scholars who study Kodashim as a sacred text. They benefit from the doctrine that study IS the mitzvah (commandment fulfillment), making intellectual engagement with the law spiritually efficacious even without physical performance. Study becomes spiritual work; the more difficulty, the more merit. They are the primary beneficiaries of the substitution framing, which makes their labor indispensable.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, textual_scholars, beneficiary,
    powerful, biographical, mobile, global).

% Those who believe sacrifice will be restored in the messianic future and that preparation for that restoration requires continued study of the sacrificial laws. They are partially excluded: rabbinic authority permits their preparatory study (as long as it remains study, not attempted practice) but forbids them from declaring the corpus a blueprint ready for imminent enactment. Their voice—that the kernel is not permanently replaced but deferred—is not included in the deliberative space. They cannot argue for restoration without facing charges of messianic false-claiming or violating rabbinic authority.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, messianic_restorationists, excluded,
    moderate, civilizational, trapped, global).

% The meta-institutional seat that adjudicates competing readings of what Kodashim means and whether its engagement occupies or archives the kernel. This includes medieval commentators, modern denominational leadership (Orthodox, Conservative, Reform), and academic rabbinic scholars. From this seat, the constraint appears as a legitimate response to historical circumstance (Temple destruction necessitating substitution) or as a cover story that obscures a true replacement by claiming continuity.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_authority_collective, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of Jewish practice after the Temple's destruction by translating sacrifice into prayer and textual study. The coordination problem is existential: how does a religion continue when its central sacrificial apparatus is destroyed? The solution is substitution—reframing the role sacrifice occupied so that prayer and study can occupy it instead, preserving the spiritual economy while changing its mechanism.
% TRANSFER_FUNCTION: Transfers authority from would-be Temple priests and literal practitioners to the rabbinic textual institution. Those who would perform sacrifice are told their role is obsolete; those who study the law gain spiritual legitimacy and institutional power. The constraint moves prestige and religious authority from performance toward hermeneutics, from Temple toward academy.
% ABSENT_VOICES: Literal sacrifice practitioners and messianic restorationists are partially excluded—they are permitted to study but forbidden to argue that the kernel remains unsubstituted or reversible. Hellenistic-era Jews who read the substitution differently (Philo's allegorism), Samaritans who continued literal sacrifice, and Karaites who rejected rabbinic authority's reinterpretation are historically silenced. Contemporary scholars who would frame this as replacement rather than substitution are not eliminated but face hermeneutic pressure to frame it as continuity.
% DISAPPEARANCE_RATIONALE: If the substitution doctrine disappeared—if the rabbinic institution stopped teaching that prayer and study replace sacrifice—Jewish practice would bifurcate. One trajectory: literal sacrificial movements would attempt restoration or argue for messianic readiness. Another: prayer and study would lose their claimed equivalence and become merely commemorative, not functionally substitutive. The institutional unity maintained by the doctrine would fracture into competing readings of what the kernel is and whether it remains occupied.
% FOUNDING_PROBLEM: After the Romans destroyed the Second Temple in 70 CE, the Jewish sacrificial system ceased. Without a physical Temple, sacrifice was impossible. Judaism needed to continue, and the rabbis faced a fundamental problem: either declare the sacrificial commandments null (abandoning continuity), or find a way to maintain them without a Temple. They chose substitution—reframing prayer and Torah study as equivalent in spiritual function to the sacrifices they replaced.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic institution and mainstream Jewish denominations attest the founding problem is solved—substitution works, preserving continuity. Historians and comparative religionists attest the founding problem is solved but question whether substitution is continuity or replacement; the distinction matters for authority. Literal practitioners and some Jewish renewal movements attest the founding problem remains live—the kernel (physical sacrifice) cannot truly be substituted away; it is deferred. Scholars like Shaye Cohen and David Kraemer attest (from outside the benefiting parties) that substitution is historically real and represents a genuine theological innovation, not a false framing—but their corroboration is academic, not the testimony of communities claiming the benefit.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 (moderate-high) because the substitution doctrine simultaneously claims continuity (prayer/study = sacrifice) while denying restoration (the kernel is archived, not occupied). This asymmetry is the extraction: those who accept the substitution collect the narrative (continuity, legitimacy); those who want restoration are told they want the impossible. Suppression is 0.72 because the constraint's persistence depends on actively forbidding literal sacrifice and marginalizing those who believe it remains the true kernel—the rabbinic institution must enforce the reading that Kodashim is archive, not blueprint. Theater is 0.61 (high) because a substantial portion of engagement with Kodashim is performative: prayer services invoke the sacrificial structure (intentionally, not functionally), study includes ritual aspects (standing while learning, specific times for specific laws) that perform the role rather than occupy it functionally. The measurement series shows extractiveness rising from 0.32 (immediately after the Temple's destruction, when substitution was clearly necessary) to 0.68 (by the medieval period and maintained thereafter, when the substitution doctrine is reified and resistance is largely suppressed). The corresponding rise in theater_ratio (0.25 to 0.61) tracks the accumulation of performative ritual around the study corpus—over time, more of the engagement becomes theatrical maintenance of the substitution narrative rather than genuine functional continuity. Accessibility_collapse rises across all levels (structural alternatives—literal sacrifice—become more completely foreclosed) while resistance falls (from 0.62 individual-level resistance at t0 to 0.54 at t2000), tracking the internalization of suppression.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic institution) perceives the constraint as solving a coordination problem: how to preserve Jewish practice when its central infrastructure is destroyed? The answer: substitution, which maintains spiritual continuity while accommodating institutional change. From this seat, the constraint is necessary and legitimate. The payer seat (literal practitioners) perceives the constraint as a replacement disguised as substitution: prayer and study were added as supplements or temporary measures, not intended as permanent replacements. The rabbinic institution chose to present this substitution as permanent, foreclosing the possibility of restoration. From this seat, the constraint is extractive—it uses the language of continuity to deny the possibility of returning to the original kernel. The engine computes these as different effective types per seat because the structural asymmetry (beneficiary controls the interpretation framework, payer is locked out of that framework) creates different effective classifications. The claimed type (tangled_rope) bridges both: it genuinely coordinates the Jewish practice-economy by maintaining a role for study and prayer while simultaneously extracting authority from would-be literalists by defining them as outside the legitimate frame.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic institution sits at d near the beneficiary end (0.2-0.3): it collects authority, controls interpretation, benefits from the doctrine. Literal practitioners sit near the target end (d = 0.85-0.95): they are told their reading is illegitimate, their practice is forbidden, their kernel is archived. The prayer-practitioners sit near symmetric (d ≈ 0.5): they benefit from the narrative of continuity but are constrained by the substitution doctrine's rules (when to pray, how to invoke sacrifice, which movements are permitted). Textual scholars sit at low d (0.15-0.25): they benefit substantially (study becomes spiritually meritorious) but are not explicitly extracted from—they are invited into the hermeneutic work. The asymmetry is the engine's key input: from the agenda-setter's seat (rabbinic institution), the arrangement is a genuine coordination solving the problem of practice-persistence after Temple destruction. From the literal practitioner's seat, it is enforced replacement disguised as continuity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem status is contested: the rabbinic institution attests that substitution solves the persistence-of-practice problem (founding problem is live and addressed). Literal practitioners attest the founding problem is unresolved—the true kernel (sacrifice) cannot be substituted, only awaited (founding problem is live but unsolved in the rabbinic reading). The tangled_rope classification prevents misreading this as either pure rope (genuine coordination solving the problem for everyone) or pure snare (all extraction with no coordination). The constraint genuinely coordinates Jewish practice across the institutional break of Temple destruction—that coordination function is real. But it does so by extracting authority from those who reject the substitution reading, positioning them as outside the framework entirely. The classification holds both elements: real coordination (prayer and study do maintain a role that resembles the sacrificial function) and real asymmetric extraction (only the rabbinic reading of what the kernel is gets authority; alternate readings are suppressed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_vs_replacement_ambiguity,
    'Is the substitution of prayer and study for sacrifice a genuine functional continuity (the kernel is occupied under new conditions) or a replacement that claims continuity to obscure abandonment?',
    'Textual archaeology: tracing whether rabbinic discourse frames substitution as temporary (exile-necessitated) or permanent; examining whether messianic restoration rhetoric names sacrifice as the true kernel or merely as a future option; analyzing whether prayer liturgy invokes sacrifice as parallel or subordinate.',
    'If replacement, extractiveness increases and the constraint shifts toward snare (the archive claims continuity while denying restoration). If genuine substitution, extractiveness may be justified as coordination cost of preserving practice across institutional discontinuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_replacement_ambiguity, conceptual, 'Whether the substitution doctrine represents continuity or concealed replacement of the sacrificial kernel.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the suppression of literal sacrifice restoration discourse structural (legal prohibition, institutional exclusion, social sanction) or internalized (literal practitioners come to believe substitution is legitimate even after institutional pressure is removed)?',
    'Historical trace: examining how literal practitioners (Karaites, Samaritans, contemporary groups) initially resisted the substitution doctrine and what mechanisms (legal, theological, social) converted resistance to acceptance or permanent marginalization; post-exit suppression trajectory if any group gains legal ability to practice sacrifice.',
    'If primarily structural, suppression can be relaxed by legal/institutional change; if primarily internalized, the constraint persists even after external barriers are removed, indicating higher effective extraction on identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression of literal sacrifice practice is structural or internalized in resistant groups.').

omega_variable(
    kernel_identity_reading_ambiguity,
    'What is the actual kernel this reading is about? Is it (a) the act of making an offering to God (which prayer substitutes for), (b) the Temple institution (which cannot be substituted, only awaited), or (c) the legal corpus itself (which study occupies)?',
    'Examining rabbinic responsa and medieval commentaries where the kernel is most explicitly named; tracing whether different constituencies (Ashkenazi, Sephardic, Karaite, Samaritan) identified the kernel differently; comparing the present-day reconstruction movement''s kernel-identification with traditional readings.',
    'If the kernel is the sacrificial act, the substitution is genuine (prayer performs the role). If the kernel is the Temple institution, the substitution is partial evasion (study cannot substitute for architecture). If the kernel is the corpus, the substitution is tautological (studying the law IS the kernel). Different kernel identifications produce different ε values and type assignments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_identity_reading_ambiguity, conceptual, 'Ambiguity in what constitutes the kernel the substitution doctrine claims to occupy.').

omega_variable(
    false_summit_beneficiary_naturalness,
    'The substitution doctrine benefits the rabbinic institution (which authors and administers it). Is this constraint a natural response to historical necessity (no Temple available after 70 CE, so substitution is inevitable), or does the rabbinic institution benefit from the appearance of inevitability (only they can interpret how substitution works)?',
    'Comparative history: examining how other destroyed religions/institutions responded to the same problem (e.g., how Christianity handled the Temple destruction, how Zoroastrianism handled priestly displacement, how Hinduism addressed altar constraints); assessing whether the substitution doctrine or some alternative was ideologically inevitable vs. institutionally chosen.',
    'If inevitable, the apparent beneficiary distribution (rabbinic textual institution collects authority) is a false summit—the institution benefits from accident, not from design. If chosen, the institution''s authority-gain is intended extraction, increasing suspicion of tangled_rope vs. rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_beneficiary_naturalness, conceptual, 'Whether the rabbinic beneficiary distribution is natural response to historical necessity or constructed institutional advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.25).
narrative_ontology:measurement(koda_tr_t250, kodashim_corpus__substitution_archive, theater_ratio, 250, 0.35).
narrative_ontology:measurement(koda_tr_t500, kodashim_corpus__substitution_archive, theater_ratio, 500, 0.48).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__substitution_archive, theater_ratio, 1000, 0.61).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.61).
narrative_ontology:measurement(koda_tr_t2000, kodashim_corpus__substitution_archive, theater_ratio, 2000, 0.61).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(koda_be_t250, kodashim_corpus__substitution_archive, base_extractiveness, 250, 0.48).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__substitution_archive, base_extractiveness, 500, 0.62).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.68).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__substitution_archive, base_extractiveness, 2000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(koda_su_t250, kodashim_corpus__substitution_archive, suppression_requirement, 250, 0.52).
narrative_ontology:measurement(koda_su_t500, kodashim_corpus__substitution_archive, suppression_requirement, 500, 0.63).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__substitution_archive, suppression_requirement, 1000, 0.72).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.72).
narrative_ontology:measurement(koda_su_t2000, kodashim_corpus__substitution_archive, suppression_requirement, 2000, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=2000
narrative_ontology:measurement(koda_grid_01, kodashim_corpus__substitution_archive, accessibility_collapse(class), 0, 0.52).
narrative_ontology:measurement(koda_grid_02, kodashim_corpus__substitution_archive, accessibility_collapse(class), 2000, 0.76).
narrative_ontology:measurement(koda_grid_03, kodashim_corpus__substitution_archive, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(koda_grid_04, kodashim_corpus__substitution_archive, accessibility_collapse(individual), 2000, 0.74).
narrative_ontology:measurement(koda_grid_05, kodashim_corpus__substitution_archive, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(koda_grid_06, kodashim_corpus__substitution_archive, accessibility_collapse(organizational), 2000, 0.78).
narrative_ontology:measurement(koda_grid_07, kodashim_corpus__substitution_archive, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(koda_grid_08, kodashim_corpus__substitution_archive, accessibility_collapse(structural), 2000, 0.82).
narrative_ontology:measurement(koda_grid_09, kodashim_corpus__substitution_archive, resistance(class), 0, 0.58).
narrative_ontology:measurement(koda_grid_10, kodashim_corpus__substitution_archive, resistance(class), 2000, 0.48).
narrative_ontology:measurement(koda_grid_11, kodashim_corpus__substitution_archive, resistance(individual), 0, 0.62).
narrative_ontology:measurement(koda_grid_12, kodashim_corpus__substitution_archive, resistance(individual), 2000, 0.54).
narrative_ontology:measurement(koda_grid_13, kodashim_corpus__substitution_archive, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(koda_grid_14, kodashim_corpus__substitution_archive, resistance(organizational), 2000, 0.42).
narrative_ontology:measurement(koda_grid_15, kodashim_corpus__substitution_archive, resistance(structural), 0, 0.48).
narrative_ontology:measurement(koda_grid_16, kodashim_corpus__substitution_archive, resistance(structural), 2000, 0.38).
narrative_ontology:measurement(koda_grid_17, kodashim_corpus__substitution_archive, stakes_inflation(class), 0, 0.32).
narrative_ontology:measurement(koda_grid_18, kodashim_corpus__substitution_archive, stakes_inflation(class), 2000, 0.62).
narrative_ontology:measurement(koda_grid_19, kodashim_corpus__substitution_archive, stakes_inflation(individual), 0, 0.28).
narrative_ontology:measurement(koda_grid_20, kodashim_corpus__substitution_archive, stakes_inflation(individual), 2000, 0.58).
narrative_ontology:measurement(koda_grid_21, kodashim_corpus__substitution_archive, stakes_inflation(organizational), 0, 0.38).
narrative_ontology:measurement(koda_grid_22, kodashim_corpus__substitution_archive, stakes_inflation(organizational), 2000, 0.64).
narrative_ontology:measurement(koda_grid_23, kodashim_corpus__substitution_archive, stakes_inflation(structural), 0, 0.44).
narrative_ontology:measurement(koda_grid_24, kodashim_corpus__substitution_archive, stakes_inflation(structural), 2000, 0.68).
narrative_ontology:measurement(koda_grid_25, kodashim_corpus__substitution_archive, suppression(class), 0, 0.32).
narrative_ontology:measurement(koda_grid_26, kodashim_corpus__substitution_archive, suppression(class), 2000, 0.64).
narrative_ontology:measurement(koda_grid_27, kodashim_corpus__substitution_archive, suppression(individual), 0, 0.28).
narrative_ontology:measurement(koda_grid_28, kodashim_corpus__substitution_archive, suppression(individual), 2000, 0.62).
narrative_ontology:measurement(koda_grid_29, kodashim_corpus__substitution_archive, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(koda_grid_30, kodashim_corpus__substitution_archive, suppression(organizational), 2000, 0.68).
narrative_ontology:measurement(koda_grid_31, kodashim_corpus__substitution_archive, suppression(structural), 0, 0.42).
narrative_ontology:measurement(koda_grid_32, kodashim_corpus__substitution_archive, suppression(structural), 2000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__substitution_archive, 0.12).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (kodashim_corpus). The substitution_archive reading claims Kodashim documents what was replaced. The study_as_exercise reading claims study occupies the kernel. The performance_only reading claims the kernel remains unoccupied, merely deferred. These three readings form a constraint family linked by kernel identity, not by causal dependency. Each reading produces a different ε value and type classification based on what it claims the kernel is. The substitution_archive reading affects both siblings by establishing the interpretive framework that their readings must contest: if substitution is real, study-as-exercise must defend why study is occupation rather than exercise, and performance-only must defend why performance is still the true kernel despite substitution's claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
