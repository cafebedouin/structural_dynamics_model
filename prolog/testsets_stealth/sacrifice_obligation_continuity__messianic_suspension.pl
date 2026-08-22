% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrificial Obligation Suspension — Messianic Readiness Maintenance Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   After the destruction of the Second Temple, the tradition's central
 *   sacrificial commandments became unperformable. This story instantiates
 *   one reading of how the obligation persisted: the messianic_suspension
 *   reading, under which the obligation is suspended — deliberately neither
 *   fulfilled nor violated — pending a restoration that will reactivate it,
 *   and the sustained study of sacrificial law functions as a maintenance
 *   protocol keeping the community ready for reactivation. The claim/metric
 *   gap is deliberate per the independence rule: the constraint is CLAIMED as
 *   scaffold because the arrangement's justification is explicitly
 *   transitional (it exists to carry the community to a terminus it names),
 *   while the authored metrics describe its actual operation — a moderate,
 *   real readiness burden, low structural suppression, and a study practice
 *   that is part genuine technical transmission and part routinized
 *   recitation. Epsilon's referent is the standing arrangement under contest
 *   — the suspension-plus-maintenance regime — assessed by this reading's own
 *   lights; the rival readings are separate constraints, not hedges folded
 *   into this one.
 *
 * KEY AGENTS:
 *   - halakhic_authorities: Agenda setter (institutional/identity_locked) — articulates the suspension doctrine, sets the liturgical requirements, and holds the only seat that could dissolve or reactivate the obligation
 *   - rabbinic_academies: Primary beneficiary and co-administrator (institutional/identity_locked) — receives enrollment, funding, and purpose from the maintenance program it runs
 *   - observant_praying_communities: Primary payer, incidental beneficiary (organized/constrained) — bears the daily readiness burden, receives continuity and a guilt-free resolution
 *   - kodashim_scholars: Secondary payer (moderate/identity_locked) — invests careers in indefinitely deferred application
 *   - restoration_advocacy_movements: Forward-oriented beneficiary (organized/constrained) — derives its entire warrant from the obligation's pending status
 *   - liberal_dissolved_movements: Excluded voice (organized/mobile) — dissolved the bindingness outright and objects from outside the conversation
 *   - comparative_religion_scholars: Analytical observer (analytical/analytical) — sees the full structure including the four-reading competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.42).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.25).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.42).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrificial Obligation Suspension — Messianic Readiness Maintenance Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '10820a2b-2eed-411f-b9fe-62fe7e686b10').
narrative_ontology:cs_kernel_codification('10820a2b-2eed-411f-b9fe-62fe7e686b10', fixed_text).
narrative_ontology:cs_authority_grounding('10820a2b-2eed-411f-b9fe-62fe7e686b10', lineage).
narrative_ontology:cs_interpretation_layer_present('10820a2b-2eed-411f-b9fe-62fe7e686b10').
narrative_ontology:cs_reading_relation('10820a2b-2eed-411f-b9fe-62fe7e686b10', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('10820a2b-2eed-411f-b9fe-62fe7e686b10', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_reading_relation('10820a2b-2eed-411f-b9fe-62fe7e686b10', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('10820a2b-2eed-411f-b9fe-62fe7e686b10', foundational, obligation_suspended_not_discharged).
narrative_ontology:cs_axiom_status(obligation_suspended_not_discharged, holdable).
narrative_ontology:cs_axiom_grounding('10820a2b-2eed-411f-b9fe-62fe7e686b10', obligation_suspended_not_discharged, theological).
narrative_ontology:cs_axiom('10820a2b-2eed-411f-b9fe-62fe7e686b10', secondary, study_maintains_restoration_readiness).
narrative_ontology:cs_axiom_status(study_maintains_restoration_readiness, holdable).
narrative_ontology:cs_axiom_grounding('10820a2b-2eed-411f-b9fe-62fe7e686b10', study_maintains_restoration_readiness, instrumental).
narrative_ontology:cs_reference_frame('10820a2b-2eed-411f-b9fe-62fe7e686b10', standing_temple_service_order).
narrative_ontology:cs_drift_state('10820a2b-2eed-411f-b9fe-62fe7e686b10', contemporary_post_destruction_interregnum, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('10820a2b-2eed-411f-b9fe-62fe7e686b10', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_academies).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, observant_praying_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, restoration_advocacy_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, kodashim_scholars).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, observant_praying_communities).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, kodashim_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decisors and rabbinic leadership who articulate the suspension doctrine, set the daily liturgical requirements that operationalize it, and adjudicate its edge cases. They are also the only seat that could formally dissolve the obligation's bindingness or declare the suspension ended; abandoning the framework would dissolve the warrant of their own office, since their authority rests on transmitting the law intact through the interruption.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Academies whose advanced curricula in the sacrificial orders, scholarly hierarchies, and enrollment economies exist because the sacrificial corpus retains normative relevance. They design and administer the study program that constitutes readiness maintenance, and they receive students, funding, and institutional purpose from its continuation. Dropping the sacrificial orders from the curriculum would amputate a defining portion of their institutional identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_academies, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, rabbinic_academies, agenda_setter).

% Communities that carry the readiness burden: daily recitation of sacrificial passages, study expectations, and a liturgical calendar sequenced around a restoration that has not arrived. In exchange they receive covenantal continuity, a resolution of otherwise unperformable commandments that carries no guilt, and a communal identity anchored in shared anticipation. Leaving the practice means leaving the community's normative world — possible, but at severe social and identity cost.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, observant_praying_communities, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, observant_praying_communities, beneficiary).

% Specialists who invest years mastering sacrificial law whose practical application is indefinitely deferred. They receive scholarly standing, teaching positions, and intellectual community within the academies. Their professional identity is fused with the corpus's living relevance; redirecting to a non-sacrificial specialty would forfeit accumulated standing and self-conception.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, kodashim_scholars, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, kodashim_scholars, beneficiary).

% Organizations engaged in vessel reconstruction, priestly genealogy documentation, architectural planning, and advocacy for resumed service. The suspension doctrine supplies their entire warrant: if the obligation were dissolved, their mission would lose its normative foundation overnight; if it were currently fulfilled another way, their urgency would evaporate. Their identity is bound to the restoration they await.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, restoration_advocacy_movements, beneficiary,
    organized, generational, constrained, global).

% Movements that resolved the same founding rupture by dissolving the obligation's bindingness outright. They object that the suspension framing retrojects continuity onto what was rupture and allocates communal time, liturgy, and curriculum to a dormant system. They sit outside the halakhic conversation this reading governs and have already exercised the exit this reading's adherents decline.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, liberal_dissolved_movements, excluded,
    organized, generational, mobile, continental).

% Researchers who study the arrangement as a post-destruction adaptive strategy and compare suspension doctrines across religious traditions. They take no position inside the framework, observe the full structure including the competition among the four readings, and document the arrangement's persistence without sharing its premises.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__messianic_suspension, rabbinic_academies).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains legal and liturgical continuity between a destroyed sacrificial system and its anticipated restoration: it keeps the law's technical detail alive in living memory, sequences communal practice around a shared expectation, and resolves the paradox of commandments that cannot currently be performed without declaring them void or violated.
% TRANSFER_FUNCTION: Moves time and attention — daily liturgical recitation, years of specialized study — from praying communities and scholars into the maintenance of sacrificial-law competence; moves legitimacy, continuity, and institutional purpose to the rabbinic establishment and the academies that administer the maintenance program.
% ABSENT_VOICES: Liberal movements that dissolved the bindingness outright would object that the suspension framing launders rupture into continuity; secular historians would object that the doctrine's self-description as suspension conceals an adaptive invention after 70 CE. Within the tradition, holders of the standing-liability reading object that suspension understates what is owed. None of these voices sits inside the halakhic conversation this reading administers.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine vanished overnight, the communities holding it would confront the unperformable-obligation paradox with no resolution in place: they would have to adopt one of the rival readings (fulfillment-through-study, dissolution-of-bindingness, or acceptance of standing violation), and each path rearranges liturgy, curriculum, institutional funding, and self-understanding. Restoration advocacy movements would lose their warrant entirely; the academies' advanced programs would lose their normative anchor.
% FOUNDING_PROBLEM: The destruction of the Second Temple made the Torah's sacrificial commandments unperformable, threatening a dilemma: either the covenant's obligations stood unfulfilled (a standing breach), or the law's central content had been abrogated. The suspension doctrine was built to hold the obligation intact through an indeterminate interregnum — neither discharged nor violated — until restoration resumes performance.
% FOUNDING_PROBLEM_CORROBORATION: The founding rupture is corroborated from outside the benefiting parties: Roman-era historical accounts, later Christian and Islamic textual witnesses to the cessation of sacrifice, and academic historiography of the post-70 rabbinic adaptation (including the preservation of sacrificial procedure in the Mishnah within living memory of the Temple). The problem's continued liveness, however, is attested only by the reading's own premises and the institutions that benefit from it — no source outside the beneficiary set affirms that an obligation currently awaits restoration; external scholarship corroborates the arrangement's persistence, not the problem's liveness. That asymmetry is itself signal.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.42 reflects the readiness burden's real but bounded weight: daily liturgical time, curriculum allocation, and specialized study careers, uncompensated by any current performance and unmollified by guilt (suspension, not violation, is the design's core move). Suppression 0.25 is mostly internalized — identity fusion, habit, communal expectation — with minimal structural barrier; per the engine's rules it is authored as a raw structural property and is not scaled by power or scope. Theater_ratio 0.30: a substantial share of the practice is genuine technical transmission (the sacrificial orders remain among the most demanding in the curriculum), but a large share is rote recitation of procedures describing rites that cannot occur. Accessibility_collapse 0.20 is low because the rival readings remain fully live and accessible — understanding the suspension doctrine collapses none of them. Resistance 0.35 captures real intra-traditional contestation from rival readings and thin external resistance. The temporal series run on one shared eight-point grid (70–2026 CE) with both tracked metrics authored at every point; no suppression_requirement series is authored because the enforcement picture is static — communal norms without a machinery that builds up or decays — and that stability is carried by the scalar. The 1967 inflection marks renewed practical plausibility (Jerusalem's status change and the restoration-advocacy surge), which briefly lowered theater and raised extractiveness before population-level diversification diluted the average burden.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the academies' seat the arrangement is the living center of the tradition — the corpus is alive, the program is working. From the praying communities' seat it is a liturgical tax paid against an uncertain dividend, softened by identity and continuity returns. From the restoration movements' seat it is urgent preparation on a credible clock. From the excluded liberal movements' seat it is an anachronism allocating scarce communal resources to a dormant system. From the observer's seat it is a two-millennium adaptive strategy whose self-description (suspension) and function (continuity maintenance) partially diverge. The engine computes per-seat classifications from the structural data; this story does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: rabbinic_academies and restoration_advocacy_movements sit near the beneficiary end (low d, subsidized or damped effective extraction), with the academies' dual agenda_setter role pulling them slightly back toward symmetry since they also bear governance costs. observant_praying_communities and kodashim_scholars sit toward the target end (high d) as the burden's bearers, moderated by their secondary beneficiary roles — the communities receive continuity and identity returns; the scholars receive standing and position. Exit modulation matters: identity_locked payers (scholars) sit nearer the full-target end than the constrained communities, whose exit is costly but structurally available. halakhic_authorities derive a mid-low d: they administer and collect legitimacy while bearing the arrangement's custodial costs. No directionality overrides are used — the beneficiary/victim declarations plus exit options produce the correct relationships without correction. The excluded and observer seats fall outside the derivation by construction.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim guards against two mislabels. Reading the arrangement as pure coordination would erase its constitutive provisionality — its justification is the transition to restoration, not a steady state, and it carries a declared sunset clause (the restoration event) even though the clause's firing date is indeterminate. Reading it as pure extraction would require victims and suppressed exits that the no-liability structure specifically avoids: suspension-without-guilt is the design's anti-extraction feature, and the rival readings remain accessible (low accessibility_collapse), so no one is trapped inside this reading's frame. The mandatrophy risk runs in the opposite direction: the founding problem's liveness is contested, and a sunset unfired for nineteen centuries invites drift toward inertial, theatrical maintenance — tracked honestly in the theater_ratio series (rising through the medieval routinization, dipping at the restoration-advocacy inflection) and flagged in the indefinite_sunset_firing omega. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no dead-problem flag fires, correctly, because the arrangement's persistence genuinely depends on arrangements (liturgy, curriculum, institutional funding) that would rearrange if it vanished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading (messianic_suspension) of the kernel sacrifice_obligation_continuity; how would the structural profile change under each sibling reading?',
    'Track adoption patterns across movements and jurisdictions: which reading each community''s liturgy, curriculum, and pastoral rulings actually instantiate.',
    'Under study_as_performance, study becomes current fulfillment — a present-tense benefit structure appears and the readiness-burden framing weakens. Under performance_only, the obligation stands unmet, introducing a standing-liability victim set (guilt returns) and raising effective extraction sharply. Under archival_preservation, bindingness dissolves entirely, epsilon drops toward negligible, and the maintenance obligation disappears. Each sibling is a separate constraint file with its own epsilon and stakeholder surface.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer structure: which sibling reading is instantiated changes victim sets, epsilon, and classification.').

omega_variable(
    indefinite_sunset_firing,
    'The arrangement''s sunset clause (messianic restoration) has an indeterminate firing date; after roughly nineteen centuries unfired, does the scaffold''s transitional character survive, or is the arrangement converting into inertial permanence?',
    'Track whether readiness practice retains forward-looking orientation — practical preparation, restoration activism, legal drafting for resumed service — versus becoming commemorative routine detached from any expected terminus.',
    'If commemorative routine dominates, the trajectory bends toward degraded inertia and the theater_ratio series should be read as leading-indicator; if live anticipation persists in practice (not only doctrine), the transitional classification holds despite the elapsed interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_sunset_firing, empirical, 'Whether an unfired sunset across two millennia preserves or erodes the arrangement''s transitional justification.').

omega_variable(
    institutional_capture_degree,
    'How much of the maintenance protocol''s persistence serves communal covenantal continuity, and how much serves institutional reproduction of the academies that administer the study program?',
    'Counterfactual curricular analysis: would advanced sacrificial-law study persist at current scale absent the career structures, enrollment economics, and scholarly hierarchies built on it? Compare communities where the curriculum is mandated versus elective.',
    'High institutional capture would raise effective extraction for the payer seats above the communal-benefit baseline and warrant scrutiny of whether the coordination function is being maintained for its stated end or for the administrator''s reproduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_degree, empirical, 'Degree to which the readiness-maintenance economy accrues to administering institutions rather than the community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 70, 0.15).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 250, 0.22).
narrative_ontology:measurement(sacr_tr_t600, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 600, 0.27).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1000, 0.31).
narrative_ontology:measurement(sacr_tr_t1400, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1400, 0.34).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1800, 0.35).
narrative_ontology:measurement(sacr_tr_t1967, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(sacr_tr_t2026, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 70, 0.36).
narrative_ontology:measurement(sacr_be_t250, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 250, 0.39).
narrative_ontology:measurement(sacr_be_t600, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 600, 0.43).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1000, 0.45).
narrative_ontology:measurement(sacr_be_t1400, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1400, 0.46).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1800, 0.44).
narrative_ontology:measurement(sacr_be_t1967, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1967, 0.47).
narrative_ontology:measurement(sacr_be_t2026, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2026, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__messianic_suspension, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'what happened to sacrifice law after the Temple' covers four structurally distinct claims that share one fixed-text kernel (the Torah's sacrificial code) but diverge on the obligation's current deontic status. This reading (messianic_suspension) authors epsilon for the suspension-plus-study-maintenance arrangement as this reading sees it: moderate readiness burden, no current victim set, no guilt. study_as_performance adds a present-fulfillment benefit structure; performance_only adds a standing-liability victim set; archival_preservation removes bindingness and drops epsilon toward negligible. Each sibling is a separate file with its own beneficiaries, victims, and classification; the files are linked here because the upstream fixed-text kernel is cited as warrant by each downstream reading, and adoption of one reading structurally pressures the resource base (students, funding, liturgical time) available to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
