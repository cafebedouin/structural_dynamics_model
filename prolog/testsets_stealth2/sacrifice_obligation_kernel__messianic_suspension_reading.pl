% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Messianic Suspension of the Sacrificial Obligation with Study-Maintained Readiness
 *   domain: religious/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   After the destruction of the Second Temple (70 CE), a covenant whose
 *   fixed text commands an ongoing sacrificial regime lost its operative
 *   venue. This story instantiates ONE reading of that predicament — the
 *   messianic_suspension_reading of the sacrifice_obligation_kernel: the
 *   obligations remain in full divine force but are suspended (not annulled,
 *   not transformed into another practice) until messianic restoration, and
 *   the community's study of the sacrificial corpus maintains operational
 *   readiness for that restoration. The epsilon referent is the standing
 *   arrangement — suspension plus readiness-study — assessed by this
 *   reading's own lights: a divinely granted interim that spares the
 *   community impossible performance, financed by a modest study levy, with
 *   preserved capacity running to a future generation. CONSTRAINT FAMILY: the
 *   kernel decomposes into four readings, each a separate constraint with its
 *   own epsilon — this file (scaffold-claimed, epsilon 0.12, instrumental
 *   study, future-generation beneficiary); study_as_exercise_reading (study
 *   occupies the mitzvah now, shifting beneficiary weight toward the present
 *   community); performance_only_reading (the obligation binds
 *   unconditionally, so the unmet duty weighs on the present community,
 *   raising tension and resistance); symbolic_archive_reading (no halakhic
 *   claim — heritage-preservation with near-zero epsilon and no restoration
 *   beneficiary). The fixed text is the shared upstream; the downstream
 *   readings inherit its authority while disputing study's status and the
 *   obligation's interim force. Claim/metric independence: the scaffold claim
 *   rests on the arrangement's self-declared transitional character; the
 *   metrics are authored separately from what the arrangement's operation
 *   descriptively shows. KEY AGENTS (by structural relationship): -
 *   halakhic_decisors: agenda-setting seat (institutional/identity_locked) —
 *   administer the suspension interpretation and the study mandate; can
 *   neither annul the commandments nor restore the venue -
 *   contemporary_observant_communities: principal cost-bearing beneficiary
 *   (organized/identity_locked) — relieved of sacrificial duty, bears the
 *   study-load that maintains readiness - kohanim_priestly_lineages:
 *   dual-positioned beneficiary (moderate/identity_locked) — preserve lineage
 *   and purity readiness for future service at continuing personal cost -
 *   restoration_preparation_movements: concentrated beneficiary
 *   (organized/identity_locked) — translate readiness-doctrine into vessels,
 *   garments, and site preparation - future_restored_generation: designated
 *   terminal beneficiary (powerless/trapped) — inherits whatever operational
 *   capacity the interim preserves; holds no seat in present decisions -
 *   academic_historians_of_religion: analytical observer
 *   (analytical/analytical) — attests the founding problem and the
 *   arrangement's post-70 genesis from outside the benefiting parties
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Messianic Suspension of the Sacrificial Obligation with Study-Maintained Readiness").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious/halakhic_authority/commitment_system_dynamics").

narrative_ontology:has_sunset_clause(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '32a3910a-44ff-469e-a1b0-843c01a38c8f').
narrative_ontology:cs_kernel_codification('32a3910a-44ff-469e-a1b0-843c01a38c8f', fixed_text).
narrative_ontology:cs_authority_grounding('32a3910a-44ff-469e-a1b0-843c01a38c8f', lineage).
narrative_ontology:cs_interpretation_layer_present('32a3910a-44ff-469e-a1b0-843c01a38c8f').
narrative_ontology:cs_reading_relation('32a3910a-44ff-469e-a1b0-843c01a38c8f', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('32a3910a-44ff-469e-a1b0-843c01a38c8f', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('32a3910a-44ff-469e-a1b0-843c01a38c8f', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('32a3910a-44ff-469e-a1b0-843c01a38c8f', foundational, obligation_suspended_not_transformed).
narrative_ontology:cs_axiom_status(obligation_suspended_not_transformed, holdable).
narrative_ontology:cs_axiom_grounding('32a3910a-44ff-469e-a1b0-843c01a38c8f', obligation_suspended_not_transformed, theological).
narrative_ontology:cs_axiom('32a3910a-44ff-469e-a1b0-843c01a38c8f', foundational, study_maintains_operational_readiness).
narrative_ontology:cs_axiom_status(study_maintains_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('32a3910a-44ff-469e-a1b0-843c01a38c8f', study_maintains_operational_readiness, instrumental).
narrative_ontology:cs_reference_frame('32a3910a-44ff-469e-a1b0-843c01a38c8f', sanctuary_contingent_full_obligation).
narrative_ontology:cs_drift_state('32a3910a-44ff-469e-a1b0-843c01a38c8f', contemporary_post_destruction_interim, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('32a3910a-44ff-469e-a1b0-843c01a38c8f', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_observant_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priestly_lineages).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, restoration_preparation_movements).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_restored_generation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_observant_communities).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priestly_lineages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate what the commandments require while their venue lies in ruins: they rule that the duties remain in force but are held in abeyance by Heaven, set the place of the sacrificial corpus in the study curriculum, and answer questions about priestly marriage, purity, and firstlings that keep the categories warm. They cannot annul the commandments — no council claims authority to uproot a divine word — and cannot rebuild the venue; their discretion runs to interpretation and preparation only. Stepping outside the tradition's interpretive chain would end their standing as decisors.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_decisors, agenda_setter,
    institutional, generational, identity_locked, global).

% Live inside the arrangement daily: prayers ask for restoration, the destruction is mourned on a fixed calendar, and the sacrificial tractates occupy a central place in adult study. They are spared the cost of bringing animals, tithes, and pilgrimage under current conditions, and they give back study-hours, curricular priority, and donations toward restoration-oriented projects. Leaving the arrangement would mean leaving the covenantal community altogether, not choosing among its readings.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_observant_communities, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_observant_communities, payer).

% Families of priestly descent maintain the distinctions a future service would require: known lineage, marital discipline, ritual-purity awareness, and the priestly blessing delivered in synagogues. The arrangement keeps their hereditary office meaningful during the interim; the cost is continuing restrictions and genealogical vigilance with no present venue in which to serve. Their status is bound to the arrangement's continuation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priestly_lineages, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priestly_lineages, payer).

% Organizations that treat the interim as a preparation period: they fashion vessels and garments to scriptural specification, research priestly genealogy and red-heifer candidates, and educate the public on the service's mechanics. The readiness-doctrine is their charter; their funding, membership, and purpose depend on the obligation remaining live-but-suspended. If the obligation were archived as history, their reason for existing would lapse.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, restoration_preparation_movements, beneficiary,
    organized, generational, identity_locked, regional).

% The people, wherever and whenever they live, who would inherit either a working capacity — trained personnel, intact procedures, identified priests, prepared instruments — or a gap filled by improvisation. They make no decisions now and bear no present cost; the arrangement's entire justification runs toward them, and they cannot decline the inheritance or its burdens.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_restored_generation, beneficiary,
    powerless, civilizational, trapped, global).

% Scholars of Second Temple Judaism and rabbinic literature who study the post-destruction adaptation without standing inside the covenantal claim. They document the crisis of 70 CE, the redaction of the sacrificial orders after the destruction, and the range of strategies communities adopted; their analyses corroborate the founding problem from outside the circle that benefits from the arrangement.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, academic_historians_of_religion, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__messianic_suspension_reading, future_restored_generation).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__messianic_suspension_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a commandment-set legally alive and communally rehearsed across a performance-gap that no participant chose: the suspension preserves the obligation's binding status without demanding impossible acts, and the study curriculum preserves the procedural knowledge — species, quantities, sequences, priestly and purity prerequisites — that a restored service would consume. It also solves a status-continuity problem for priestly lineages, whose hereditary office would otherwise have no interim meaning.
% TRANSFER_FUNCTION: Moves study-hours, curricular priority, and donation flows from contemporary community members into preservation of restoration-capacity designated for a future generation; moves assurance of continued office to priestly lineages; moves interpretive jurisdiction and institutional centrality to the decisor class. No material wealth leaves the community — the transferred goods are time, attention, and status.
% ABSENT_VOICES: Inside the commitment system no major seat is structurally absent — the sibling readings are held and argued openly, so the breadth of the suspension frame reflects real consensus rather than missing dissent. Structurally absent: (1) the animals and animal-welfare constituencies who would bear the resumed practice's costs if the suspension ever lifted — they hold no seat in halakhic deliberation and would object to restoration itself; (2) Jews outside the covenantal framework, and former members, for whom restoration-expectation reads as exclusionary or distressing; (3) the future generation itself, whose interests the arrangement claims to advance and who cannot speak — its voice is simulated by the very intermediaries the arrangement empowers.
% DISAPPEARANCE_RATIONALE: If the suspension-plus-readiness arrangement vanished overnight — if the community stopped treating the obligation as alive-but-held — the sacrificial tractates would lose their organizing rationale and drift into history curricula, priestly-lineage consciousness would erode without its interim office, preparation movements would lose their charter, the liturgy's restoration petitions would detach from practice, and the community's account of its own fidelity would be rewritten. The covenantal self-understanding rearranges; there are stakeholders on every seat.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE made a divinely commanded performance-regime impossible overnight. The arrangement was built to answer a precise question: what is the status of an obligation whose performance God's providence has blocked — violated, voided, transformed, or suspended? The suspension reading answers: suspended, in full force, awaiting restoration.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic historians of Second Temple Judaism and rabbinics attest both the crisis and the post-destruction redaction of the sacrificial orders (Kodashim, Taharot) as a response to it — the Mishnah's detailed sacrifice procedures were compiled after the venue was gone, which is hard to explain unless the obligation's status was a live problem. The daily liturgy's restoration petitions and the calendar of destruction-mourning attest the problem's persistence from inside the tradition but outside any modern beneficiary's interest. No credible source disputes that the founding problem was the post-destruction obligation gap; what is disputed is the answer, which is the kernel contest itself.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because the arrangement's only systematic levy is study-time and curricular priority; the dominant flow runs the other way — the community is spared animal, tithe, and pilgrimage costs it would owe under performance conditions. Suppression is low (0.08): nothing coercive maintains the suspension — it is upheld by the venue's absence and by acceptance, and the three sibling readings operate as live legal alternatives inside the same system, so alternatives are nowhere near collapsed (accessibility_collapse 0.15). Resistance is low (0.12): the intra-traditional contest is over study's status, not over the suspension frame itself. Theater_ratio (0.18) reflects a real but growing commemorative share: as living memory of the service receded, some engagement drifted from readiness-maintenance toward mourning and preservation — the series tracks this slow rise while staying well under the Goodhart threshold. The measurement series run on one shared six-point grid (both tracked metrics authored at every point); suppression_requirement is deliberately untracked because the enforcement picture is static — nothing ratchets and nothing decays — so its level is carried by the scalar alone. Temporal shape: both series rise gently; extraction accumulates as the study apparatus institutionalizes and preparation movements professionalize, theater accumulates as restoration defers.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the decisor seat the arrangement is stewardship: keeping a divine word legally alive without demanding the impossible — a fiduciary holding, not a levy. From the community seat it is relief plus a modest tax: no animals and no pilgrimage logistics, in exchange for curriculum hours. From the priestly-lineage seat it is status-preservation with carrying costs. From the preparation-movement seat it is charter and livelihood. From the future-generation seat — the least computable, since it holds no present agent — it is pure inheritance. An analytical seat sees a two-millennium provisional measure that has outlived every institution that founded it. The engine derives these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   All four declared beneficiary groups derive low directionality: contemporary communities (net relief), priestly lineages (status preserved), preparation movements (charter and funding), and the future generation (terminal recipient of preserved capacity). No victim class is declared — during the suspension nobody bears a harm the arrangement inflicts; the study levy is the only systematic cost-bearing, and it falls on agents who are simultaneously beneficiaries, which is why communities and lineages carry secondary payer roles rather than appearing in a victims array. The decisor seat is left to structural derivation: it administers the arrangement and accrues interpretive authority, but it also bears teaching labor and can neither annul nor restore anything — the derivation places it mid-range, and no override is authored because the beneficiary data plus exit options already yield the right shape. Suppression enters the engine unscaled; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an obligation whose venue providence removed — is still live: the venue is still absent, so the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag; mandatrophy is not resolved and the flag is not declared. The live risk runs the other way: this is a scaffold whose sunset is eschatological, and scaffolds rot when their transition stops being believed. If restoration-expectation ever collapsed inside the community while the apparatus persisted, the founding problem would die in place — status flipping to dead against a still-rearranging world — and the theater series is the early-warning instrument for exactly that rot: its slow climb toward commemorative engagement is the signature of a transitional arrangement drifting toward theatrical maintenance. The study_readiness_efficacy omega is the second tripwire: readiness that no longer transfers to practice is piton-formation inside a scaffold's shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_sacrifice_kernel,
    'This constraint is one reading (messianic_suspension_reading) of the sacrifice_obligation_kernel; which structural features would change under the sibling readings, and where exactly is the disagreement located?',
    'Comparative classification of the four reading-files in the kernel family: diff beneficiary structures, epsilon values, and coordination types across readings. The disagreement is located in the halakhic status of study relative to the suspended obligation, and in whether the obligation retains binding force during the interim.',
    'Under study_as_exercise_reading the study apparatus becomes substitutive fulfillment (the center of gravity shifts from readiness-preservation to interim discharge, and the future-generation beneficiary thins); under performance_only_reading the unmet binding obligation weighs on the present community (tension and resistance rise sharply); under symbolic_archive_reading the halakhic claim vanishes entirely (heritage-preservation with near-zero epsilon and no restoration beneficiary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_sacrifice_kernel, conceptual, 'Committer structure: one reading of a four-reading kernel; disagreement located in study''s status and the obligation''s interim force.').

omega_variable(
    eschatological_sunset_functionality,
    'Does an undated termination condition that no human act can trigger (messianic restoration) function as a sunset clause, or does indefinite deferral quietly convert the transitional arrangement into a steady state?',
    'Behavioral test: do community practices, curricular emphasis, and preparation investment track perceived imminence (revival-pressure episodes such as red-heifer candidates or political ruptures in site access), or are they invariant to restoration-expectation cycles? Corpus comparison with other long-deferred conditional terminations.',
    'If the sunset is behaviorally inert, the arrangement operates as steady-state coordination and the classification should drift from scaffold toward rope; if investment tracks imminence, the transitional character is live and the scaffold claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eschatological_sunset_functionality, conceptual, 'Whether the eschatological termination condition functions as a real sunset.').

omega_variable(
    study_readiness_efficacy,
    'Does the study apparatus actually preserve transferable operational capacity for a restored service, or has multi-generational scholastic drift severed procedural knowledge from performable practice?',
    'Structured readiness assessment: expert panels simulating restoration scenarios, tracing whether tractate mastery converts to executable procedure (species identification, measurements, sequence, priestly qualification), as the preparation movements'' own vetting attempts to do.',
    'If efficacy is low, the theater_ratio is understated and the arrangement is drifting toward theatrical maintenance of an atrophied function; if high, the instrumental justification holds and the low-extraction profile is sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_readiness_efficacy, empirical, 'Empirical probe of whether the readiness-maintenance function is real.').

omega_variable(
    suspension_authority_attribution,
    'Is the suspension genuinely an act of the divine legislator (as the reading asserts), or a human communal adaptation retroactively clothed in divine agency?',
    'Internal-textual analysis: whether the tradition treats non-performance as excused rather than violated (absence of any penitential requirement for unoffered sacrifices, explicit providential framing in liturgy and law) versus evidence of quiet human decision-points in the arrangement''s administration.',
    'If the suspension is humanly administered, the agenda-setting seat shifts from Heaven to the decisor class, administrative rents become visible, and effective extraction rises above the authored low value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_authority_attribution, conceptual, 'Divine versus human authorship of the suspension.').

omega_variable(
    future_beneficiary_vs_intermediary_capture,
    'Do readiness investments actually transfer capacity forward to a future generation, or does ''future generations'' function as a projection behind which present intermediaries (the decisor class, preparation movements) accrue status and funding?',
    'Resource-flow tracing: what share of readiness-directed resources converts to durable transferable capacity (texts mastered, personnel qualified, artifacts usable) versus present consumption (institutional overhead, prestige, fundraising).',
    'If intermediary capture dominates, the designated terminal beneficiary is nominal, the receipt of gains shifts to present seats, and the low-extraction reading tilts toward a hybrid with a coordinated-many, paying-many structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_beneficiary_vs_intermediary_capture, empirical, 'Whether the future-generation beneficiary is real or launders present institutional interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t40, observed).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement_basis(sacr_tr_t60, observed).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement_basis(sacr_tr_t80, observed).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement_basis(sacr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.07).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 40, 0.09).
narrative_ontology:measurement_basis(sacr_be_t40, observed).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 60, 0.1).
narrative_ontology:measurement_basis(sacr_be_t60, observed).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 80, 0.11).
narrative_ontology:measurement_basis(sacr_be_t80, observed).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement_basis(sacr_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__messianic_suspension_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, information_standard).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% One kernel, four constraints: the colloquial label 'the sacrificial obligation' conflates four structurally distinct claims about study's status and the obligation's interim force. Each reading gets its own epsilon, its own beneficiary structure, and its own classification; this file authors the suspension reading (epsilon 0.12, scaffold-claimed, information_standard coordination). Family edges run through network.affects_constraints; the fixed text is the shared upstream from which all four readings descend, and each downstream reading cites it as warrant while disputing what it demands during the interim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
