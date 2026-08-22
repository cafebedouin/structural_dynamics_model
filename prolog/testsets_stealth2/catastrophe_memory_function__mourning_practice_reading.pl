% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Tisha B'Av Memorial Obligation — Mourning-Practice Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   The annual observance of Tisha B'Av — a roughly twenty-five-hour fast
 *   with five classical prohibitions, the candlelit reading of Lamentations,
 *   and the recitation of elegies accumulated over two millennia — obligates
 *   community members to re-enact the destruction of the Temples and
 *   subsequent catastrophes on a fixed date. This file instantiates ONE
 *   reading of the catastrophe_memory_function kernel: the mourning-practice
 *   reading, on which the ritual's operative content is commemorative
 *   mourning and boundary-maintenance (D1/D4) and nothing else — the ritual
 *   IS the boundary-maintenance, with no survival-competence transmission
 *   claimed. The sibling readings (survival_competence_reading,
 *   hybrid_transformation_reading) are separate constraints in separate files
 *   with their own epsilon values; they are not averaged into this one. The
 *   epsilon referent here is the standing arrangement under contest — the
 *   annual obligatory mourning itself — assessed by this reading's own
 *   lights, never the redeemed or transformed arrangement the tradition
 *   promises.
 *
 * KEY AGENTS:
 *   - rabbinic_calendar_authorities: Agenda-setter and receipt-seat (institutional/arbitrage) — fixes the date, compiles the liturgy, adjudicates exemptions, collects legitimacy and calendar-control
 *   - observant_jewish_communities: Primary beneficiary (organized/constrained) — collects identity continuity, synchronized memory, and belonging
 *   - doubting_insiders: Primary target (moderate/identity_locked) — pays the full annual cost of compliance while the identity good bypasses them
 *   - yom_hashoah_founders: Excluded rival (institutional/mobile) — built a civil commemoration outside the religious calendar after failing to share or reshape the canonical day
 *   - religious_zionist_dissenters: Internal beneficiary-dissenter (organized/constrained) — observes while disputing the day's justifying condition
 *   - historians_of_collective_memory: Analytical observer (analytical/analytical) — sees the full two-millennium structure without holding any seat in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.4).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Tisha B'Av Memorial Obligation — Mourning-Practice Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:has_sunset_clause(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '7b466b93-b2a5-4d44-b852-7ec188659493').
narrative_ontology:cs_kernel_codification('7b466b93-b2a5-4d44-b852-7ec188659493', fixed_text).
narrative_ontology:cs_authority_grounding('7b466b93-b2a5-4d44-b852-7ec188659493', lineage).
narrative_ontology:cs_interpretation_layer_present('7b466b93-b2a5-4d44-b852-7ec188659493').
narrative_ontology:cs_reading_relation('7b466b93-b2a5-4d44-b852-7ec188659493', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b466b93-b2a5-4d44-b852-7ec188659493', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('7b466b93-b2a5-4d44-b852-7ec188659493', foundational, commemoration_constitutes_identity).
narrative_ontology:cs_axiom_status(commemoration_constitutes_identity, holdable).
narrative_ontology:cs_axiom_grounding('7b466b93-b2a5-4d44-b852-7ec188659493', commemoration_constitutes_identity, deontological).
narrative_ontology:cs_axiom('7b466b93-b2a5-4d44-b852-7ec188659493', foundational, mourning_carries_no_competence_content).
narrative_ontology:cs_axiom_status(mourning_carries_no_competence_content, holdable).
narrative_ontology:cs_axiom_grounding('7b466b93-b2a5-4d44-b852-7ec188659493', mourning_carries_no_competence_content, empirically_contingent).
narrative_ontology:cs_axiom('7b466b93-b2a5-4d44-b852-7ec188659493', secondary, fasts_transform_to_festivals_at_redemption).
narrative_ontology:cs_axiom_status(fasts_transform_to_festivals_at_redemption, holdable).
narrative_ontology:cs_axiom_grounding('7b466b93-b2a5-4d44-b852-7ec188659493', fasts_transform_to_festivals_at_redemption, theological).
narrative_ontology:cs_reference_frame('7b466b93-b2a5-4d44-b852-7ec188659493', perpetual_mourning_until_redemption).
narrative_ontology:cs_drift_state('7b466b93-b2a5-4d44-b852-7ec188659493', post_1967_jerusalem_sovereignty, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('7b466b93-b2a5-4d44-b852-7ec188659493', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, observant_jewish_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, rabbinic_calendar_authorities).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, doubting_insiders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, religious_zionist_dissenters).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, exile_incompleteness_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, communal_memory_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fix the date of the fast in the lunisolar calendar, compile and authorize the lament liturgy (Eicha and the kinot corpus), adjudicate exemptions for illness and danger, and add new laments when fresh catastrophes arrive — as when Holocaust elegies entered the canon during the twentieth century. They teach the obligations, answer practical questions, and in earlier centuries administered communal discipline for public violation. Their standing rests on being the trusted keepers of the calendar and liturgy; stepping outside that custodial role would dissolve the basis of their own authority.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, rabbinic_calendar_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, rabbinic_calendar_authorities, beneficiary).

% Gather each summer on the ninth of Av to fast, hear Lamentations read by candlelight, and recite elegies naming the destructions of 586 BCE and 70 CE and later calamities. What flows to them is continuity: the same words their grandparents said, a shared date that synchronizes dispersed communities worldwide, and a yearly rehearsal of who they are. Leaving the practice would not endanger them; it would loosen the thread connecting them to ancestors and to each other.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, observant_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Members, often born into observant families or embedded in observant neighborhoods, who no longer hold the underlying beliefs but continue to fast and attend because declining would cost them family standing, marriage prospects, communal belonging, and self-understanding. Each year they pay the full price of the day — hunger, restriction, the emotional labor of enacting grief they do not feel — while the identity good the day produces passes them by. Exit exists but means leaving the world that raised them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, doubting_insiders, payer,
    moderate, biographical, identity_locked, regional).

% Israeli legislators and survivors' organizations who in the 1950s established a civil Holocaust remembrance day outside the religious calendar, after proposals to house Holocaust memory within the traditional fast met resistance from the liturgy's custodians, who preferred adding elegies to the existing day rather than sharing or ceding the commemorative ground. They wanted a state commemoration open to all citizens regardless of observance. The two commemorations now run in parallel, each marking the other as insufficient.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, yom_hashoah_founders, excluded,
    institutional, generational, mobile, national).

% Observant thinkers and some rabbis who keep the fast but argue openly — especially since 1948, and again after 1967 when the Temple Mount came under Israeli control — that mourning a destruction whose central site is again in Jewish hands sits uneasily beside the tradition's own promise that these fasts will turn to joy. They propose downgrading, transforming, or reinterpreting the day while remaining inside the community that observes it, bearing the dissonance between practice and argument.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, religious_zionist_dissenters, beneficiary,
    organized, generational, constrained, national).

% Scholars of ritual, memory, and nationalism who study how the fast has operated across two millennia — how it survived emancipation, absorbed new catastrophes into its liturgy, competed with civil commemoration, and drew unprecedented secular attendance after the Holocaust. They take no part in observance and owe nothing to any of the other seats.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, historians_of_collective_memory, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, rabbinic_calendar_authorities).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of intergenerational catastrophic-memory preservation: individual grief fades within a generation, so a fixed annual date, a shared lament liturgy, and a bodily practice (fasting) coordinate millions of dispersed agents to re-enact the loss simultaneously, keeping the memory and the group boundary legible across centuries and continents.
% TRANSFER_FUNCTION: Moves a day of bodily comfort and ordinary activity from observing individuals into the collective ledger of memory; moves legitimacy and calendar-control to the institutions that keep the date and authorize the liturgy; and moves commemorative attention toward the catastrophes named in the canonical elegies and away from rival commemorations.
% ABSENT_VOICES: Secular and civil commemorators (the Yom HaShoah founders) sit outside the halakhic conversation that governs the day; Reform leadership that abandoned the fast in the emancipation era is no longer at the table; and nonbelieving insiders are present in body but muted in voice, since voicing doubt inside the community carries costs they largely decline to pay.
% DISAPPEARANCE_RATIONALE: If the obligation vanished overnight, the densest continuously transmitted thread of Second-Temple destruction memory would thin within a few generations; communal calendars, the lament liturgy, and the identity boundary the fast draws between the community and its neighbors would all rearrange around whatever replaced them — or around nothing. The parallel civil commemoration would also lose the foil against which it defined itself.
% FOUNDING_PROBLEM: Preserving the memory of the Temple's destruction (586 BCE and 70 CE) and sustaining communal identity through catastrophe and dispersion — ensuring a scattered people would not forget the loss that came to define them, until the tradition's promised reversal when the fasts become festivals.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Judaism and collective memory, writing from outside the benefiting parties, attest that the original preservation function was real and that the mechanism worked across two millennia. The Knesset record establishing Yom HaShoah attests, from a state seat outside the religious calendar, that the traditional vessel was judged insufficient or unavailable for the newest catastrophe. Religious-Zionist halakhic dissent after 1948 and 1967 attests the contested status from inside. Affirmation that the founding problem is still live comes chiefly from within the benefiting parties; external sources corroborate the historical function and the contest itself, not the liveness claim.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.40 at interval end) because the annual cost is bounded — one day, five prohibitions — and most participants self-select into a practice that returns them identity goods; the extraction that exists concentrates on the identity-locked payer seat. Suppression (0.45) reflects enforcement that has migrated from formal communal discipline to social and identity-based holding: leaving is possible but expensive. Theater ratio (0.28) is modest — the practices still carry their commemorative function for most observers, with rote drift visible mainly among peripheral affiliates. Accessibility collapse is low (0.35): alternatives demonstrably persist, as the successful establishment of a civil rival commemoration and the survival of private and secular mourning forms show. Resistance (0.52) is real and documented: Reform abandonment, Zionist transformation proposals after 1948 and 1967, and the Yom HaShoah rivalry. The measurement series run on one shared eight-point grid (every tracked metric authored at every time point). The suppression_requirement series is authored deliberately because this story specifically tracks enforcement-capacity change: formal disciplinary machinery decayed across the interval while social enforcement persisted, producing a falling trajectory rather than a static picture. The extraction series humps around 1948-1967 — the legitimacy contest made compliance costlier for dissenters and doubters — then eases as the liturgy absorbed Holocaust elegies and the fast recovered coherent function.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the calendar authorities' position, the arrangement is a living inheritance they steward and without which their own role dissolves. From the communities' position, it is a rhythm that delivers belonging at a tolerable annual price. From the doubting insiders' position, the identical practices operate as compulsory grief — full cost, no collected good, exit priced in family and selfhood. From the excluded founders' position, the day is a competitor that crowded their commemoration out of the canonical ground. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The two declared beneficiary groups derive low directionality: communities collect the identity good directly, and the authorities — despite administering rather than merely receiving — are structurally near the beneficiary end because the arrangement subsidizes their standing (their secondary beneficiary role records this). Doubting insiders derive high directionality: they bear the transfer and their identity_locked exit pushes them toward the full-target end, so effective extraction is amplified for exactly the seat least able to refuse. The dissenters derive low directionality from their beneficiary role; the dissonance costs they bear are recorded qualitatively in their situation rather than forced through an override, because the derivation already places them correctly on the beneficiary side. No directionality overrides are used — the beneficiary/victim declarations plus exit options produce the right relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite misreadings. First, the piton misread: a two-thousand-year-old obligatory fast looks superficially vestigial, and a lazy analysis would score high theater and declare the mandate dead. The temporal record contradicts this — the liturgy absorbed a new catastrophe mid-century, secular attendance surged after the Holocaust, and the theater ratio stays below a third — so the function is demonstrably renewed, not performed. Second, the pure-rope misread: calling the whole arrangement benign coordination would erase the locked payer seat and the authority rents, which is why the tangled_rope claim carries both a coordination function and named victims. The founding_problem_status of 'contested' (rather than 'dead') is the honest genealogical finding: the destruction is past and its memory preserved, but whether the unredemption condition that justifies continued mourning still obtains is disputed by the tradition's own internal dissent — and the mismatch consumer therefore sees contested x world_rearranges, not the dead-arrangement zombie signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint instantiates only the mourning-practice reading of the catastrophe_memory_function kernel; does the ritual additionally transmit survival-competence as the sibling readings claim, such that this file''s epsilon misdescribes the arrangement''s operative content?',
    'Comparative study of crisis-adaptation and institutional-continuity outcomes in communities formed by the fast versus matched communities without it — the empirical test that would adjudicate the survival and hybrid readings'' core claims.',
    'Demonstrated competence-transmission would shift the correct instantiation toward the hybrid reading and raise the arrangement''s assessed functional content; confirmed absence would stabilize this reading as the sole accurate instantiation of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the kernel''s D5 content exists alongside the D1/D4 content this reading isolates.').

omega_variable(
    founding_problem_liveness,
    'Is the founding problem — persistent unredemption justifying continued mourning — still live, given restored sovereignty and the tradition''s own promise that these fasts will become festivals?',
    'Halakhic consensus process on whether the transformation triggers have been met, read against external historiography of whether the memorial''s justifying condition still obtains; neither is decidable from inside the commemorative frame alone.',
    'If the founding problem is dead, the arrangement persists by inertia and drifts toward theatrical maintenance; if live, ongoing coordination legitimately sustains the obligation and the tangled_rope profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, conceptual, 'Contested liveness of the unredemption condition the mourning obligation answers to.').

omega_variable(
    suppression_relocation_internalization,
    'As formal communal discipline decayed across the interval, did total suppressive force actually fall, or did it relocate into identity lock and internalized obligation that the scalar cannot see?',
    'Post-exit trajectory studies of people who leave observant communities: if the felt obligation, guilt, and calendar-attachment persist after the external barriers drop, the suppression was substantially internalized.',
    'If internalized, effective suppression exceeds the authored scalar and the doubting-insider seat''s extraction is understated by the structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_relocation_internalization, empirical, 'Structural versus internalized suppression mechanism behind the falling enforcement series.').

omega_variable(
    rival_commemoration_absorption,
    'Did absorbing Holocaust elegies into the fast renew its commemorative function, or begin converting it into a generic catastrophe container whose boundary-specific content dilutes over time?',
    'Track liturgical composition and stated attendance motivation over coming decades; compare communities that retained separate civil Shoah commemorations against those that folded memory into the fast.',
    'Dilution would raise the theater ratio over time and push the arrangement toward degraded inertia; genuine renewal stabilizes the current profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rival_commemoration_absorption, empirical, 'Direction of the mid-century absorption of rival commemorative content.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 1900, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1900, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(cata_tr_t1920, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1920, 0.23).
narrative_ontology:measurement(cata_tr_t1940, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1940, 0.26).
narrative_ontology:measurement(cata_tr_t1948, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1948, 0.27).
narrative_ontology:measurement(cata_tr_t1967, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1967, 0.29).
narrative_ontology:measurement(cata_tr_t1980, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(cata_tr_t2026, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t1900, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1900, 0.34).
narrative_ontology:measurement(cata_be_t1920, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(cata_be_t1940, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1940, 0.38).
narrative_ontology:measurement(cata_be_t1948, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1948, 0.42).
narrative_ontology:measurement(cata_be_t1967, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1967, 0.46).
narrative_ontology:measurement(cata_be_t1980, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1980, 0.44).
narrative_ontology:measurement(cata_be_t2000, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement(cata_be_t2026, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 2026, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1900, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(cata_su_t1920, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1920, 0.63).
narrative_ontology:measurement(cata_su_t1940, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1940, 0.58).
narrative_ontology:measurement(cata_su_t1948, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1948, 0.53).
narrative_ontology:measurement(cata_su_t1967, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(cata_su_t1980, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1980, 0.47).
narrative_ontology:measurement(cata_su_t2000, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 2000, 0.46).
narrative_ontology:measurement(cata_su_t2026, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the memory function of catastrophe ritual' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints: this file (mourning-practice and boundary-norms only), survival_competence_reading (adaptive-capacity transmission only), and hybrid_transformation_reading (both contents). The decomposition exists because ascribing different operative content to the same ritual yields different epsilon values, different failure modes, and different research programs; forcing one story to span all three would make epsilon observer-relative. This reading is upstream of the hybrid reading in the sense that the boundary-maintenance substrate it isolates is the territory the hybrid reading must additionally explain; the survival reading stands as a parallel rival function-ascription. All three files link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
