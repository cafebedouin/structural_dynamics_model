% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Cultural Archive — Voluntary Study and Transmission Practice
 *   domain: religious law / halakhic authority / commitment-system dynamics / cultural transmission
 *
 * SUMMARY:
 *   A corpus of sacrificial law — the Levitical descriptions, the Mishnaic
 *   and Talmudic orders of Kodashim, and their commentaries — is taught and
 *   studied across contemporary Jewish communities as a cultural-historical
 *   archive. Under the framing this story instantiates, the study practice
 *   preserves identity and continuity: it links present communities to their
 *   Temple-era textual foundations without asserting that any sacrifice is
 *   owed, that study fulfills any command, or that any duty is being
 *   deferred. Participation is voluntary and unenforced; the arrangement's
 *   costs are study time and curricular attention, borne by those who choose
 *   them, and its output is a shared inheritance passed to future
 *   generations. The story assesses this standing arrangement — the living
 *   practice of transmission — as the symbolic archive reading sees it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.04).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Symbolic Cultural Archive — Voluntary Study and Transmission Practice").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious law / halakhic authority / commitment-system dynamics / cultural transmission").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, torah_learners).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, future_jewish_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, rabbinic_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities across Israel and the diaspora maintain schools, study cycles, and curricula in which the sacrificial corpus is taught. The archive gives them a shared textual inheritance connecting present practice to Temple-era foundations. De-emphasizing the corpus is available — some communities barely teach it — but it carries an identity cost in continuity claims, which keeps most communities engaged though no one compels them.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Individuals who encounter the corpus in day schools, yeshivot, adult-education classes, or private study. They receive textual fluency and identity grounding; the time cost is theirs and is borne by choice, ending whenever they stop. Some study the sacrificial portions for cultural connection, some for curiosity, some as one stretch of a broader curriculum.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, torah_learners, beneficiary,
    moderate, biographical, mobile, global).

% Generations not yet born or not yet of age who will receive the archive as transmitted. They cannot consent to or decline the inheritance in advance; what reaches them unbidden is the corpus and the identity it carries. Whether they keep it is decided later, but the transmission itself arrives without their participation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, future_jewish_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Teachers, curriculum committees, and institutional heads who decide where the sacrificial corpus sits in the study cycle and how it is framed — as heritage, as history, or as text among texts. They sustain the practice by teaching it and draw professional identity and livelihood from the role. They could reframe or drop the corpus; doing so would unsettle their institutions' self-understanding and their own vocations.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, rabbinic_educators, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__symbolic_archive_reading, rabbinic_educators, beneficiary).

% The accumulated corpus and its unbroken transmission chain — the good the practice maintains, listed as a non-actor for completeness. It exerts no force and collects nothing: communities and educators do the maintaining, learners do the carrying, and future generations do the inheriting.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).

% Descendants of the transmitting communities who have left religious practice but remain inside the identity the archive is said to preserve. They take no part in deciding what the collective memory contains or how it is taught. Some would object that continuity is claimed in their name; others would say the archive has nothing to do with them. Their absence is a standing fact the practice's continuity claims must accommodate.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, secular_jewish_descendants, excluded,
    moderate, biographical, mobile, global).

% Historians and scholars of rabbinic literature who study the sacrificial corpus and its reception without joining its transmission. They document the historical rupture the practice responds to, trace how the heritage framing emerged, and assess from outside whether the study practice does the continuity work it is said to do.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, academic_jewish_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational transmission of a textual corpus: the sacrificial-law archive is preserved, taught, and referenced so that communal identity remains linked to its Temple-era textual foundations. No single generation or family can maintain the transmission chain alone; the shared study practice solves that continuity problem.
% TRANSFER_FUNCTION: Moves study time and curricular attention from present learners and educators into the maintenance of shared memory, and moves identity capital from prior generations — through the preserved corpus — to present and future ones. No money, labor, or status is taken from any participant seat by any other.
% ABSENT_VOICES: Secular and disaffiliated descendants of the transmitting communities are absent from decisions about what the collective archive contains and how it is taught; some would object that continuity is claimed in their name, others that the archive is irrelevant to them. Their absence is structural: the continuity narrative is authored by those who participate in the practice.
% DISAPPEARANCE_RATIONALE: If the study practice vanished overnight, the transmission chain would break: curricula would lose the corpus, educators would lose a vocation-defining subject, and the communities' continuity claims would thin to their remaining channels. The books would remain, but the living practice of transmission — and the identity work it performs — would have to be rebuilt from nothing by whoever wanted it.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE ended the sacrificial cult, a community whose founding law centered on sacrifice faced a continuity problem: how to keep the corpus central to identity when performance had become impossible and — under this reading's framing — is no longer claimed of anyone. The study-and-preservation arrangement was built to keep the archive alive as cultural inheritance.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Judaism and scholars of rabbinic literature — seats outside the beneficiary communities — attest both the historical rupture (the cult's cessation and the rabbinic turn to text) and the ongoing character of the identity-continuity problem in diaspora communities. No corroboration comes from the archive itself; the attesting seats are the observer stakeholders named in this story.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near zero (0.05): the arrangement's only costs are voluntarily borne study time and the curricular space the corpus occupies; nothing moves from any seat to another under compulsion. Suppression is near zero (0.04): no enforcement machinery exists or is needed — a community or learner that stops simply stops, and the static enforcement picture is why no suppression_requirement series is authored. Theater ratio is low (0.12): under this reading the preservation function is genuine rather than a proxy for a lost one; the small performative share reflects study that has become partly self-referential ritual as living memory of the Temple recedes. Accessibility collapse is low (0.20): identity and continuity can be carried by many alternative channels — calendar, language, lifecycle ritual, other textual corpora — so understanding the archive practice forecloses no alternative. Resistance is near zero (0.08): voluntary heritage study meets no organized opposition. Claim and metrics are independent authored facts: the claimed type states what this reading takes the structure to be — a pure coordination of transmission with net-beneficiary participants and no victim set — and the metrics describe its operation as the reading assesses it; neither was tuned to the other or to a predicted engine output. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream. The temporal series share one grid (t = 0, 15, 30, 45, 60, 75, modeling roughly 1950–2025, the era in which the archive framing consolidated): extractiveness declines as communal expectation waned and participation became chosen; theater rises slightly as the practice ritualized.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is muted under this reading because every participating seat is a net beneficiary; the engine should compute rope-ward classifications at all occupied seats. The residual asymmetries are exit-shaped rather than extraction-shaped. Learners (mobile) can end participation at will and experience the corpus as enrichment. Educators (constrained, agenda-setting) have woven the corpus into vocation and institutional self-understanding, so for them the practice is load-bearing identity rather than optional study — the closest seat to identity lock, though exit remains materially available. Communities (constrained) carry continuity claims that make de-emphasis costly without anyone coercing them. The excluded seat — secular descendants — experiences the arrangement as an inheritance claimed on their behalf without their participation; per R3 that perception is commentary-grade and drives no classification override. The observer seat sees the historical rupture the practice responds to and can test the preservation claim from outside.
 *
 * DIRECTIONALITY LOGIC:
 *   All participant seats are declared beneficiaries, so derived directionality sits near the beneficiary end across the board. Learners (moderate power, mobile exit) sit nearest pure benefit. Communities and educators (organized, constrained exit) sit slightly toward symmetry because their exit is identity-costly, but their declared benefit and the absence of any victim keep them beneficiary-side. Future generations (powerless, trapped) would be misread by the raw derivation — trapped exit normally signals a target — so a directionality override sets the powerless atom to d = 0.15: the inheritance reaches them unbidden, but what reaches them is the benefit itself, not a burden. The kernel context's stated beneficiary, Jewish collective memory and identity, is authored as a non-agent stakeholder (agent: false) rather than a beneficiaries[] entry: it is the good at stake, excluded from derivation and directionality, since an abstract good collects no rents and exerts no force.
 *
 * MANDATROPHY ANALYSIS:
 *   Under this reading the founding problem — post-70 continuity — is live and the preservation function is genuine, so there is no mandatrophy: the practice has not outlived its mandate, and no mandatrophy_resolved flag is authored. The classification guards against two misreadings. Reading the practice as degraded or theatrical — vestigial maintenance of a dead obligation — imports a different reading's frame, under which study without performance is precisely what a defunct duty leaves behind; within the archive frame the theater ratio is genuinely low because preservation IS the function, not a proxy for one. Reading it as coercive fails for lack of any victim set: nothing is taken from anyone. The live question is empirical rather than structural — whether study actually carries the continuity function the rope claim rests on (omega preservation_function_efficacy) — and the sibling readings relocate the question entirely: what this frame calls preservation, the performance frame calls deferral. That relocation is committer structure and is carried in the omegas, not in this classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates one reading (symbolic_archive_reading) of the sacrifice_obligation_kernel; the same canonical corpus instantiates structurally different constraints under the sibling readings. What changes if a sibling''s structure is adopted instead of this one?',
    'Cross-reading comparison of the four linked constraint stories: each reading''s epsilon, beneficiary/victim structure, and enforcement profile are authored independently, and the engine''s per-reading classifications are the resolution surface.',
    'Under study_as_exercise_reading the arrangement becomes a live obligation occupied by study; under performance_only_reading an unfulfilled binding duty with the non-performing community in default; under messianic_suspension_reading a suspended duty maintained in readiness. Each adoption changes extractiveness and victim structure discontinuously from this story''s near-zero, no-victim profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one kernel, four readings; this file is the symbolic archive reading.').

omega_variable(
    disagreement_location_deontic_status,
    'Where exactly do the readings disagree? Not on the corpus''s content or the value of studying it, but on the deontic status of the corpus: does any binding halakhic obligation exist now, and if so in what mode — fulfilled by study, awaiting physical performance, or suspended until restoration?',
    'No empirical resolution is available: the question is settled within each holder''s normative framework, and the corpus''s own silence (no functioning Temple, no binding court) leaves the status underdetermined by the texts alone.',
    'Because the disagreement is located at the obligation''s existence and mode, the readings cannot be merged or averaged: any single framework must pick one status, which is why this reading forecloses each sibling within its own frame while all four remain live across parties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_deontic_status, conceptual, 'The contest is located at the obligation''s deontic status, not at study''s value.').

omega_variable(
    denial_meta_halakhic_status,
    'Is this reading''s central denial — that no halakhic claim is being made — itself a halakhic position (a ruling issued inside the system that no obligation binds) or a stance taken from outside the halakhic system?',
    'Examine how holders frame the denial: as a halakhic ruling (which would reinstate a normative claim the reading denies making) or as a cultural-historical characterization external to halakha.',
    'If the denial is itself halakhic, the reading smuggles a binding meta-claim back in and its zero-extraction profile is unstable; if the denial is external, the reading is cleanly non-normative and its classification is secure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(denial_meta_halakhic_status, conceptual, 'Whether the archive framing''s central denial is internal or external to halakha.').

omega_variable(
    preservation_function_efficacy,
    'Does studying the sacrificial corpus actually carry the identity-continuity function this reading claims for it, or is continuity maintained by other practices (calendar, language, lifecycle ritual) with corpus study as a rider?',
    'Longitudinal identity and affiliation studies comparing communities and individuals that vary corpus-study exposure while holding other continuity practices constant.',
    'If study is not load-bearing, the theater ratio is understated and the practice drifts toward decorative transmission with a preservation story attached; if study is load-bearing, the coordination claim is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_function_efficacy, empirical, 'Whether the preservation function the classification rests on is empirically real.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement(sacr_tr_t15, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(sacr_tr_t45, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 45, 0.1).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 75, 0.12).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sacr_be_t15, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 15, 0.09).
narrative_ontology:measurement(sacr_be_t30, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(sacr_be_t45, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 45, 0.07).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 60, 0.06).
narrative_ontology:measurement(sacr_be_t75, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 75, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'sacrifice obligation law' conflates four structurally distinct claims about the corpus's present deontic status. Per the epsilon-invariance principle, the kernel decomposes into four constraint stories — one per reading — each with its own epsilon, beneficiary/victim structure, and classification: study_as_exercise (obligation present and occupied by study), performance_only (obligation present and unfulfilled; non-performers in default), messianic_suspension (obligation present but suspended; readiness maintained), and this file (no obligation; cultural archive, zero extraction, no victim set). Historically this reading is downstream of the performance reading — it emerged as a framing for communities that had already ceased to expect performance — and the family links let the engine compare epsilon across readings of one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__symbolic_archive_reading, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
