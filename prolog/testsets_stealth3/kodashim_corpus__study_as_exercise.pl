% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Study-as-Performance Reading of the Kodashim Corpus
 *   domain: religious/commitment_system
 *
 * SUMMARY:
 *   Within rabbinic Judaism after the destruction of the Second Temple, the
 *   Order of Kodashim — the corpus of sacrificial law — legislates a cultus
 *   with no altar. The reading instantiated here holds that sustained study
 *   of these laws IS their performance: the tradition's own dictum accounts
 *   whoever engages the laws of the burnt offering as one who brought it
 *   (b.Menachot 110a). On this reading the kernel is not an idle text but an
 *   occupied one, continuously exercised through intellectual-spiritual
 *   engagement and coordinated by academies, curricula, and public study
 *   cycles. Participation is voluntary, no party transfers value to another
 *   under compulsion, and the arrangement's costs are the ordinary costs of
 *   sustaining any scholarly practice. Epsilon's referent is the standing
 *   study-practice arrangement as this reading sees it, assessed by the
 *   reading's own lights; the value is reading-indexed over that fixed
 *   referent. KEY AGENTS (by structural relationship): - kodashim_scholars:
 *   primary beneficiaries (organized/constrained) — occupy the kernel through
 *   daily engagement - rabbinic_academy_leadership: agenda-setting
 *   administrators (institutional/constrained) — run the curricular machinery
 *   that keeps the corpus in use - halakhic_authorities: co-administrators
 *   (institutional/constrained) — define the boundaries of the
 *   study-performance equation - observant_jewish_community: sustaining
 *   beneficiaries (organized/generational) — fund, join, and inherit the
 *   practice - women_in_traditional_communities: excluded seat
 *   (moderate/constrained) — historically denied access to the good the
 *   practice distributes - comparative_religion_analysts: analytical
 *   observers (analytical/analytical)
 *
 * KEY AGENTS:
 *   - kodashim_scholars: primary beneficiaries (organized/constrained) — receive fulfillment, standing, and discharge of the underlying commandments through engagement itself
 *   - rabbinic_academy_leadership: agenda setters (institutional/constrained) — set curriculum, sequence, and transmission; also collect students and standing (dual position)
 *   - halakhic_authorities: agenda setters (institutional/constrained) — adjudicate what the equation covers and how intention and restoration modify it
 *   - observant_jewish_community: beneficiaries (organized/generational) — sustain the practice materially and participate through study cycles
 *   - women_in_traditional_communities: excluded (moderate/constrained) — bear no transfer; their stake is access to the distributed good
 *   - comparative_religion_analysts: observers (analytical/analytical) — study the arrangement from outside its internal economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.06).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.06).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Study-as-Performance Reading of the Kodashim Corpus").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, 'c68ceeb2-c89a-4c5b-beda-234b3b159f7e').
narrative_ontology:cs_kernel_codification('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', fixed_text).
narrative_ontology:cs_authority_grounding('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', lineage).
narrative_ontology:cs_interpretation_layer_present('c68ceeb2-c89a-4c5b-beda-234b3b159f7e').
narrative_ontology:cs_reading_relation('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', foundational, sacrificial_obligations_remain_binding).
narrative_ontology:cs_axiom_status(sacrificial_obligations_remain_binding, holdable).
narrative_ontology:cs_axiom_grounding('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', sacrificial_obligations_remain_binding, deontological).
narrative_ontology:cs_axiom('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', foundational, study_of_laws_counts_as_offering).
narrative_ontology:cs_axiom_status(study_of_laws_counts_as_offering, holdable).
narrative_ontology:cs_axiom_grounding('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', study_of_laws_counts_as_offering, theological).
narrative_ontology:cs_reference_frame('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', continuously_occupied_kernel).
narrative_ontology:cs_drift_state('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c68ceeb2-c89a-4c5b-beda-234b3b159f7e', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, kodashim_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, observant_jewish_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, rabbinic_academy_leadership).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, study_performance_equation).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, rabbinic_interpretive_authority).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, covenantal_continuity_through_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heads the yeshivot and kolelim that set the study calendar: which tractates of the sacrificial order are learned, in what sequence, with which commentaries, and by whom. Trains and credentials the transmitters, funds scholarships from communal endowments, and organizes the public study cycles that keep the corpus in daily use. Draws students, standing, and institutional continuity from administering the practice; institutional succession depends on the practice continuing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, rabbinic_academy_leadership, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, rabbinic_academy_leadership, beneficiary).

% Responsa authors and decisors who adjudicate what the study-performance equation covers: whether learning a given offering's laws discharges the corresponding obligation, how intention enters, and what changes if the Temple service ever resumes. Publish rulings that define the boundaries of the practice; their authority rests on demonstrated mastery of the same corpus they govern.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Individual learners, from advanced students to recognized masters, who spend hours daily inside the sacrificial tractates. Receive fulfillment, communal standing, and — in the tradition's own accounting — discharge of the underlying commandments through the engagement itself. Could redirect their study to other corpora at real but not prohibitive personal cost; few do.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, kodashim_scholars, beneficiary,
    organized, biographical, constrained, global).

% Sustains the academies financially and socially, participates through synagogue study programs and personal learning, and inherits the continuity the practice maintains. Recites liturgy that presupposes the sacrificial order's ongoing relevance. Members can disengage individually at social cost; collectively the practice is constitutive of communal self-understanding.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, observant_jewish_community, beneficiary,
    organized, generational, constrained, global).

% In traditionally governed institutions, historically not admitted to advanced study of the sacrificial order and formally exempt from many of its time-bound obligations. Contemporary access varies widely by community — some now teach women Talmud including Kodashim, others maintain exclusion. Bear no transfer to or from the practice; their stake is access to the good the practice distributes.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, women_in_traditional_communities, excluded,
    moderate, generational, constrained, global).

% Historians and scholars of religion who study the arrangement from outside any participation: how the practice is organized, what it maintains, how it transmits across generations. Neither contribute to nor draw from the practice's internal economy; their published analyses occasionally feed back into the community's self-understanding.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, comparative_religion_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(kodashim_corpus__study_as_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed, multi-generational community's continuous engagement with a canonical legal corpus after the institution it describes ceased operating: shared curricula, sequenced tractates, public study cycles, and common interpretive conventions keep one body of law jointly occupied across geographies and centuries.
% TRANSFER_FUNCTION: Moves time, attention, and scholarly labor from individuals into the collective interpretive enterprise, and moves communal resources — endowments, stipends, seats of honor — toward those who sustain the engagement. Nothing is moved away from a deprived party: participants give voluntarily and receive fulfillment, standing, and continuity in return.
% ABSENT_VOICES: Inside the arrangement's own conversation, the structurally absent seat is women in traditionally governed academies, historically not admitted to advanced Kodashim study — and under this reading's own premise their admission matters most, since exclusion from study becomes exclusion from performance. Outside the arrangement, holders of rival construals of the same corpus reject the premise itself rather than any operational detail; that dissent is routed to the kernel-level omega variables and the sibling constraint files, not treated as a party inside this arrangement.
% DISAPPEARANCE_RATIONALE: If the practice vanished overnight, curricula would lose a major pillar, the community's account of its own observance would rearrange (the claim of continuous occupation of the sacrificial commandments would lapse), and the question of what these laws are for would reopen — with rival construals competing to fill the space the practice had held.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, commandments tied to sacrifice could no longer be performed in their physical mode, threatening the covenant's practicability and the community's claim to still observe the Torah it held itself bound to. The reading answers: the obligation survives in study — engaging the laws is their performance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the historical and physical record: the Temple's destruction is attested by Josephus and Roman sources independent of rabbinic interest, and the continued absence of a functioning altar is a plain empirical fact attested by the site itself. Liturgical texts petitioning restoration are recited by the whole community, not only the scholars who benefit from the study arrangement. No source disputes that the Temple is absent; dispute concerns only what follows from that absence.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.06, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. Claimed type rope comes from structure: a genuine coordination function (keeping one legal corpus jointly occupied across dispersed generations), net beneficiaries among participants, no identifiable victim set, no suppressed alternatives, and no enforcement machinery. Metrics are authored descriptively. Extractiveness 0.06: participation is voluntary and nothing is taken from a deprived party; the residual is opportunity cost of scholarly labor and mild prestige rents of institutional leadership. Suppression 0.05: no coercion backs the practice; substitutes (other areas of study, prayer, charity) are openly legitimate, and rival construals of the corpus persist alongside it. Accessibility collapse 0.25: understanding the arrangement does not close alternatives — accepting the equation leaves every other observance intact, and rejecting it leaves the corpus readable in other modes. Resistance 0.20: mild internal debate over the equation's limits and external critical indifference rather than organized opposition. Theater ratio 0.10: the core activity is functional within the reading's own lights (study genuinely discharges); a thin performative layer surrounds completion celebrations and public study campaigns, growing slowly over the interval. The temporal series run on one shared eight-point grid; suppression_requirement is deliberately not tracked as a series because the enforcement picture is static and minimal across the whole interval — the scalar captures it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the academy-leadership seat the arrangement is the living continuity it administers — the corpus is in daily use because it runs the machinery that keeps it there. From the scholar's seat it is vocation and fulfillment. From the excluded seat the same structure presents a closed door: an arrangement that distributes its central good along gendered lines, where the distribution rule (who may occupy the kernel) matters more than the aggregate fact of occupation. From the observer seat the whole thing is commitment-system maintenance — a community keeping a canonical text operative after the institution it describes ceased. The engine derives these per-seat classifications from the power, horizon, and exit data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the low-d end: scholars receive the practice's direct goods, the community receives continuity, and both declarations feed low derived directionality. No victims exist, so no seat derives a high target-directionality — there is no transfer to be targeted by. The analytically interesting seat is the excluded one: women in traditionally governed institutions neither pay into nor collect from the practice, but are denied access to the good it distributes. Pure exclusion-without-transfer is invisible to a derivation that reads only beneficiary and victim declarations, so an explicit override sets that power-atom's directionality near symmetric (0.45), slightly target-ward to reflect denied benefit. Receipt check performed before authoring gain_flow: academy leadership collects prestige and enrollment, but that is benefit-from-operation rather than captured extraction; scholars collect fulfillment; the community collects continuity. No seat receives value taken from another, so the affirmative 'diffuse' is authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The mislabeling risks run in both directions and the classification guards against each. Outwardly the arrangement can look piton-shaped: elaborate scholastic practice surrounding a defunct institution invites the reading that the function atrophied and what remains is performance. The mandatrophy test defeats that appearance: the founding problem — a destroyed Temple and commandments unperformable in their physical mode — is not dead; it recurs daily in liturgy that petitions restoration and in the plain fact that no altar exists. A live founding problem means no mandate has outlived its function. Inwardly, the arrangement cannot be misread as snare: a snare requires identifiable victims and suppressed exits, and this arrangement has neither — exits are open, substitutes are honored, and no party bears a transfer. The slow theater-ratio rise is monitored but sits far from piton thresholds, and the fixing-cost datum (prohibitive) reflects the absence of anything to cure rather than entrenchment of a harmed class: diffuse gains plus prohibitive removal cost here marks a healthy, load-bearing arrangement, not an inertial one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates one reading (study_as_exercise) of the kodashim_corpus kernel; the sibling readings (performance_only, substitution_archive) instantiate different constraints over the same corpus. Where exactly is the disagreement located, and how would the sibling files classify?',
    'Comparative analysis of the sibling constraint files. The disagreement is located in the status of the sacrificial obligation: persistently binding and dischargeable now through study (this reading), versus a husk awaiting messianic restoration (performance_only), versus a superseded cultus preserved as memorial (substitution_archive).',
    'Under performance_only the corpus is inert preparation and the arrangement''s type shifts toward archival/piton profiles; under substitution_archive the coordination function changes from occupation to commemoration. This file''s near-zero epsilon and rope structure hold only for its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; sibling classifications are separate constraints, not hedges inside this one.').

omega_variable(
    gendered_access_deprivation_ambiguity,
    'Does the historical restriction of advanced Kodashim study to men constitute deprivation (exclusion from the mitzvah''s available mode of performance) or exemption (release from a time-bound obligation)? Under this reading''s own premise the question sharpens: if study IS the performance, then exclusion from study is exclusion from performance.',
    'Internal legal analysis of whether women are obligated in the study of sacrificial commandments, combined with sociological data on contemporary access patterns across communities, which currently range from full admission to maintained exclusion.',
    'If deprivation, the arrangement carries a denied-access asymmetry that lifts effective extraction above zero for that seat and strains the clean-rope classification; if exemption, no party is deprived and the rope reading stands uncontaminated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_access_deprivation_ambiguity, conceptual, 'Whether the excluded seat is deprived of a good or relieved of a burden.').

omega_variable(
    restoration_counterfactual_reclassification,
    'If the Temple cultus resumed, what happens to this reading? Does study-as-performance become preparation, redundancy, or complement to physical offering?',
    'The reading''s own legal literature anticipates the question (whether sacrificial study obligations persist after restoration); the counterfactual is resolved by whichever ruling the restored practice adopts.',
    'Restoration would dissolve the reading''s founding condition (obligations unperformable physically), potentially revealing the arrangement as transitional rather than steady-state — a scaffold-like structure that persisted because its transition never arrived.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_counterfactual_reclassification, conceptual, 'Counterfactual restoration would restructure the reading''s justification and possibly its type.').

omega_variable(
    epsilon_reading_indexation,
    'Epsilon is authored by this reading''s lights over the standing arrangement (voluntary participation, no transfer from any deprived party). Would an external economic accounting of the identical referent — opportunity cost of scholarly labor, communal funds directed to academies, prestige rents of institutional leadership — register materially higher extraction?',
    'Author the external-economic reading as its own constraint story over the same referent and compare; divergence localizes the disagreement in observable selection rather than in the arrangement itself.',
    'A materially higher external epsilon would not falsify this file (epsilon is reading-indexed over a fixed referent) but would establish a sibling story whose classification diverges — decomposition, not compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_reading_indexation, conceptual, 'Reading-indexed epsilon versus external economic accounting of the same arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_corpus__study_as_exercise, theater_ratio, 70, 0.05).
narrative_ontology:measurement_basis(koda_tr_t70, observed).
narrative_ontology:measurement(koda_tr_t250, kodashim_corpus__study_as_exercise, theater_ratio, 250, 0.06).
narrative_ontology:measurement_basis(koda_tr_t250, observed).
narrative_ontology:measurement(koda_tr_t600, kodashim_corpus__study_as_exercise, theater_ratio, 600, 0.07).
narrative_ontology:measurement_basis(koda_tr_t600, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__study_as_exercise, theater_ratio, 1000, 0.08).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__study_as_exercise, theater_ratio, 1500, 0.09).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t1885, kodashim_corpus__study_as_exercise, theater_ratio, 1885, 0.1).
narrative_ontology:measurement_basis(koda_tr_t1885, observed).
narrative_ontology:measurement(koda_tr_t1945, kodashim_corpus__study_as_exercise, theater_ratio, 1945, 0.11).
narrative_ontology:measurement_basis(koda_tr_t1945, observed).
narrative_ontology:measurement(koda_tr_t2026, kodashim_corpus__study_as_exercise, theater_ratio, 2026, 0.12).
narrative_ontology:measurement_basis(koda_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_corpus__study_as_exercise, base_extractiveness, 70, 0.05).
narrative_ontology:measurement_basis(koda_be_t70, observed).
narrative_ontology:measurement(koda_be_t250, kodashim_corpus__study_as_exercise, base_extractiveness, 250, 0.05).
narrative_ontology:measurement_basis(koda_be_t250, observed).
narrative_ontology:measurement(koda_be_t600, kodashim_corpus__study_as_exercise, base_extractiveness, 600, 0.05).
narrative_ontology:measurement_basis(koda_be_t600, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__study_as_exercise, base_extractiveness, 1000, 0.06).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__study_as_exercise, base_extractiveness, 1500, 0.06).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t1885, kodashim_corpus__study_as_exercise, base_extractiveness, 1885, 0.07).
narrative_ontology:measurement_basis(koda_be_t1885, observed).
narrative_ontology:measurement(koda_be_t1945, kodashim_corpus__study_as_exercise, base_extractiveness, 1945, 0.06).
narrative_ontology:measurement_basis(koda_be_t1945, observed).
narrative_ontology:measurement(koda_be_t2026, kodashim_corpus__study_as_exercise, base_extractiveness, 2026, 0.06).
narrative_ontology:measurement_basis(koda_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% Constraint family: the kodashim_corpus kernel decomposes into three readings — study_as_exercise (this file), performance_only, and substitution_archive — each a separate constraint with its own epsilon, beneficiary structure, and type, per the epsilon-invariance principle. The corpus's textual stability is the shared upstream feeding all three. This reading dominates numerically in practicing communities and thereby sets the interpretive environment in which the siblings operate (influence without logical displacement, except where the obligation-status question is binary — see reading_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__study_as_exercise, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
