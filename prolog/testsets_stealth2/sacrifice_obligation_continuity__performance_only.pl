% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation as Unfulfillable Physical Duty (Performance-Only Reading)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates the performance_only reading of the kernel
 *   sacrifice_obligation_continuity: the sacrificial commandments remain
 *   fully binding after the Temple's destruction, discharge requires physical
 *   performance at an altar, and study of the sacrificial corpus is
 *   preparation for future restoration, not satisfaction. The structural
 *   delta this reading produces is realized in the data: the current
 *   generation enters the victim set carrying an obligation it cannot
 *   discharge, extraction runs high because liability accrues without remedy,
 *   and study functions as placeholder rather than payment. The epsilon
 *   referent is the standing performance-only arrangement itself, assessed by
 *   the reading's own lights: the reading does not deny the burden, it names
 *   it, praying daily for the only remedy it recognizes. Claim and metrics
 *   are authored independently: the claimed type is tangled_rope because the
 *   arrangement holds a genuine coordination function (covenantal continuity,
 *   vow integrity, readiness of the service's legal infrastructure) together
 *   with asymmetric extraction sustained by active enforcement; the metrics
 *   describe the arrangement's observed operation without being tuned to any
 *   predicted engine output. Sibling readings are separate constraints linked
 *   through the network, not folded into this one.
 *
 * KEY AGENTS:
 *   - - post_destruction_generations: Primary target (moderate/identity_locked) — inherits binding obligations it cannot discharge
 *   - - penitents_owing_purification_offerings: Sharpest target (powerless/trapped) — atonement processes left permanently incomplete
 *   - - contemporary_vow_makers: Target (moderate/constrained) — pious speech converted into multi-generation debt
 *   - - kohanic_priestly_line: Primary beneficiary (organized/identity_locked) — hereditary claim kept indispensable across the interregnum
 *   - - temple_restoration_institutions: Secondary beneficiary (organized/constrained) — modern-era collector of mandate, funds, and salience
 *   - - halakhic_authorities: Agenda setter (institutional/constrained) — administers the stringency and rules study preparatory
 *   - - study_substitute_tradition: Excluded voice (organized/mobile) — holds the rival satisfaction doctrine at the margins
 *   - - comparative_ritual_scholarship: Analytical observer (analytical/analytical) — documents the arrangement and its rivals from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.76).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.68).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.76).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation as Unfulfillable Physical Duty (Performance-Only Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '94d66c41-7e73-41f5-bc98-315cb219a94f').
narrative_ontology:cs_kernel_codification('94d66c41-7e73-41f5-bc98-315cb219a94f', fixed_text).
narrative_ontology:cs_authority_grounding('94d66c41-7e73-41f5-bc98-315cb219a94f', lineage).
narrative_ontology:cs_interpretation_layer_present('94d66c41-7e73-41f5-bc98-315cb219a94f').
narrative_ontology:cs_reading_relation('94d66c41-7e73-41f5-bc98-315cb219a94f', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('94d66c41-7e73-41f5-bc98-315cb219a94f', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('94d66c41-7e73-41f5-bc98-315cb219a94f', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('94d66c41-7e73-41f5-bc98-315cb219a94f', foundational, physical_performance_required_for_discharge).
narrative_ontology:cs_axiom_status(physical_performance_required_for_discharge, holdable).
narrative_ontology:cs_axiom_grounding('94d66c41-7e73-41f5-bc98-315cb219a94f', physical_performance_required_for_discharge, theological).
narrative_ontology:cs_axiom('94d66c41-7e73-41f5-bc98-315cb219a94f', foundational, interim_liability_without_remedy).
narrative_ontology:cs_axiom_status(interim_liability_without_remedy, holdable).
narrative_ontology:cs_axiom_grounding('94d66c41-7e73-41f5-bc98-315cb219a94f', interim_liability_without_remedy, deontological).
narrative_ontology:cs_reference_frame('94d66c41-7e73-41f5-bc98-315cb219a94f', continuously_binding_sacrificial_order).
narrative_ontology:cs_drift_state('94d66c41-7e73-41f5-bc98-315cb219a94f', extended_post_destruction_interregnum, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('94d66c41-7e73-41f5-bc98-315cb219a94f', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, kohanic_priestly_line).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, temple_restoration_institutions).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, post_destruction_generations).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, penitents_owing_purification_offerings).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, contemporary_vow_makers).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, sacrificial_law_bindingness).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, physical_performance_necessity).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, vow_integrity_across_interregnum).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Families tracing descent from the Temple priesthood keep genealogies, purity disciplines, and liturgical privileges alive across twenty centuries of dispersion. They perform no sacrifices and charge no fees; what accrues to them is standing: the reading keeps their exclusive future role at the center of the covenant's hopes, and every petition for restoration names their service. Exit would mean letting the lineage recede into history, a cost their communities treat as betrayal of ancestors.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, kohanic_priestly_line, beneficiary,
    organized, generational, identity_locked, global).

% Modern organizations reconstruct vessels, qualify candidates for priestly service, breed and inspect candidate red heifers, and campaign for access to the ancient mount. Their budgets, staff, and public salience depend on the obligation remaining live and dischargeable only by performance; were study deemed sufficient or the law lapsed, their work would become hobby rather than mandate.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, temple_restoration_institutions, beneficiary,
    organized, generational, constrained, global).

% Every cohort since the destruction inherits a sacrificial order it cannot perform. Festivals, impurities, and wrongs each generate obligations the liturgy acknowledges aloud and no one can settle. The debt is not dischargeable by study, charity, or elapsed time under this reading; it passes forward intact. Leaving the covenantal community is the only exit, and communities price it as severance from ancestors and descendants alike.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, post_destruction_generations, payer,
    moderate, generational, identity_locked, global).

% Individuals whose wrongs or impurities classically conclude with a purification or reparation offering remain, under this reading, unfinished: repentance, charity, and fasting mitigate but do not complete the process, and they die owing an offering no market sells. They hold no lever over the arrangement; their remedy lies in an event, a rebuilt altar, that they cannot cause.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, penitents_owing_purification_offerings, payer,
    powerless, biographical, trapped, global).

% Anyone who pledges an offering today incurs a debt maturing only when an altar stands. Annulment before a court releases some vows; pledged-offering obligations otherwise persist for life and beyond, turning ordinary acts of devotion into liabilities that heirs may inherit.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, contemporary_vow_makers, payer,
    moderate, biographical, constrained, global).

% Courts and codifiers decide what the obligation demands now: they rule that study trains rather than discharges, weigh which vows admit annulment, certify readiness standards, and patrol the boundary against the study-substitute position. Their standing rests on administering a demanding line; relaxing it would spend doctrinal capital accumulated over centuries.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Scholars and lineages reading the ancient dictum that whoever studies the laws of the burnt-offering is as if he offered it as literal satisfaction. They hold that the liability this arrangement assigns is already paid in text. Their position is documented in the core canon yet sits outside this reading's operative rulings; its holders publish, teach, and wait at the margins.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, study_substitute_tradition, excluded,
    organized, generational, mobile, global).

% Academic historians and anthropologists of religion who compare how traditions handle lost rites, whether as obligation, preparation, memory, or cancellation. They take no side inside the covenant and bear none of its liabilities; they document the arrangement's mechanics and its rivals from outside.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, comparative_ritual_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, kohanic_priestly_line).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a covenantal legal system coherent while its central rite is unavailable: preserves the normative force of the sacrificial corpus, holds vow-makers to their words across generations, maintains the priestly service's legal infrastructure in working order for resumption, and gives a dispersed community a shared orientation toward restoration.
% TRANSFER_FUNCTION: Moves unfulfillable liability onto every post-destruction generation; moves legitimacy and indispensability to the priestly line; moves funds, labor, and public attention to restoration enterprises; moves doctrinal authority to the courts that administer the stringency.
% ABSENT_VOICES: Holders of the study-as-fulfillment tradition would testify that the liability is already discharged and the assigned guilt manufactured; archival voices would testify that the obligation lapsed with the altar. Both survive in the textual record and in other communities but are ruled out of this reading's operative consensus.
% DISAPPEARANCE_RATIONALE: If the performance-only arrangement vanished overnight, with the obligation dissolved or study deemed sufficient, vow law would rearrange as pledged offerings were extinguished or textually discharged, penitents' atonement processes would close, restoration institutions would lose their mandate, the priestly line's claim on the future service would weaken, and the liturgy's daily restoration petitions would lose their object.
% FOUNDING_PROBLEM: How a covenant whose constitutive rite requires an altar survives the altar's destruction: whether its obligations continue, lapse, transform, or await restoration, and what the generations in between owe.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the continuous codes and responsa from the Geonic period through contemporary courts adjudicate actual vow and purity cases as live liabilities; the fixed liturgy, composed across many eras and dispersed communities, petitions daily for the service's restoration and thereby presupposes an outstanding obligation; penitential manuals instruct the owing of offerings when the Temple shall stand. None of these witnesses is a kohen or a restoration organization.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.76) because the obligation is fully decoupled from any available discharge: liability accrues to people who cannot act, which is the definition of guilt without remedy. Suppression (0.68) is doctrinal rather than physical: the reading persists by patrolling its boundary against the study-substitute and archival exits, using codification, court rulings, and communal sanction. Theater ratio (0.38) is moderate and rising: study is honestly labeled preparatory, but modern restoration activity (vessel fabrication, heifer inspection, advocacy media) is increasingly performative, staging readiness without producing performance. Accessibility collapse is low (0.40) because the sibling readings remain live, documented, and held by real parties; understanding this reading does not dissolve its alternatives. Resistance (0.55) is substantial: the rival readings are themselves the resistance, alongside mass non-orientation toward restoration among the very populations bearing the liability. The measurement series run on one shared time grid, every tracked metric authored at every examined point; the trajectories are monotonic rather than cyclical, so no intermittent-reinforcement dynamic is claimed. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the penitent and vow-maker positions the arrangement is experienced as enforced liability without remedy, closer to a snare's phenomenology; from the agenda-setter position it is stewardship of covenantal integrity through catastrophe, closer to coordination maintenance; from the beneficiary positions it is the preservation of a sacred order they are sworn to keep possible. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place the priestly line and restoration institutions near the subsidized end: the arrangement maintains their standing and mandate at no discharge cost to them. The victim declarations place the post-destruction generations, penitents, and vow makers near the full-target end, amplified by their exit profiles: identity lock for the covenantal majority, trap for penitents whose remedy is an event they cannot cause. The authorities sit mid-to-high: they administer the arrangement and bear its legitimacy costs without collecting the guilt. Global spatial scope makes readiness claims hard to verify, which modestly amplifies effective extraction for targets. No directionality overrides were needed: the role declarations differentiate the seats along lines the structural derivation can read.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Calling this a pure snare would erase the coordination function even critics concede: vow integrity, corpus preservation, and the coherence of a legal system whose rite is unavailable are real services the arrangement performs. Calling it a rope would erase the unremediable liability laid on every generation since the destruction. Tangled rope holds both halves. On obsolescence: the founding problem, coherence of obligation under loss of the performance site, is live within this reading, so no sunset applies and no piton drift is claimed; the arrangement is actively maintained, not inertially retained. The receipt surface sharpens the picture: gains accrue to a named seat while removal is cheap, since the study-substitute mechanism is internal to the canon and precedented, so persistence reflects maintenance by preference rather than impossibility. Had the founding problem been dead while the world still rearranged around the arrangement, the mismatch consumer would flag a zombie; here the problem is live and the flag does not fire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status_assignment,
    'Which reading of the sacrifice-obligation kernel governs, and what does each assignment do to the victim set?',
    'Intra-traditional adjudication through codified consensus formation, cross-checked by comparative mapping of which operative positions living communities actually hold.',
    'Adopting study_as_performance would empty the victim set and collapse epsilon sharply; archival_preservation would dissolve the constraint entirely; messianic_suspension would remove liability while retaining readiness costs. This story''s classification holds only under the performance_only assignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_status_assignment, conceptual, 'Committer structure: this constraint is the performance_only reading of kernel sacrifice_obligation_continuity; sibling readings change the victim set and epsilon structurally.').

omega_variable(
    suppression_internalization_split,
    'Is the arrangement''s suppressive force primarily structural (court rulings, codification, communal sanction) or internalized (a formed conscience that enforces the liability on itself)?',
    'Observe obligation-intensity in cohorts where institutional enforcement has thinned, such as secularizing diaspora communities: if felt liability persists after enforcement capacity decays, the internalized component dominates.',
    'If internalized, effective suppression exceeds the structural measure and outlives enforcement decay; the arrangement would persist in conscience even where its courts vanish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized enforcement of the unfulfillable obligation.').

omega_variable(
    restoration_impossibility_horizon,
    'Will a discharge site ever exist, or is the performance condition permanently unsatisfiable?',
    'Track the geopolitical and religious trajectory of the Temple Mount and the operational maturity of priestly-readiness programs.',
    'Permanent impossibility fixes extraction at its ceiling and drifts the profile toward pure liability collection; restoration would convert the interregnum structure into ordinary coordination almost overnight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_impossibility_horizon, empirical, 'Whether the guilt-without-remedy condition is temporary or permanent.').

omega_variable(
    study_readiness_theater_boundary,
    'Is study under this reading behaviorally preparatory, or does it already function as satisfaction in practice despite the official denial?',
    'Ethnography of sacrificial-law study communities: whether learners report discharge or deferral, and whether readiness outputs such as trained personnel and viable ritual procedure actually materialize.',
    'Widespread behavioral satisfaction would drive the theater ratio past 0.5 and expose the reading''s stated premise as a mislabel of lived practice, pressuring convergence toward the study_as_performance sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_readiness_theater_boundary, conceptual, 'Whether the placeholder function of study is genuinely preparatory or covertly satisfactory.').

omega_variable(
    beneficiary_capture_era_shift,
    'Which seat captures the arrangement''s gains across eras: the priestly line''s continuous accrual of standing, or the modern restoration institutions'' accrual of funds and mandate?',
    'Longitudinal accounting of restoration-movement finances and of kohanic institutional salience across the interval.',
    'If modern institutional capture dominates, the recent trajectory leans toward a snare profile with a concentrated collector; if diffuse status preservation dominates, the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_era_shift, empirical, 'Era-dependent location of the receipt seat for the arrangement''s gains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soc_performance_only_tr_t70, sacrifice_obligation_continuity__performance_only, theater_ratio, 70, 0.12).
narrative_ontology:measurement(soc_performance_only_tr_t300, sacrifice_obligation_continuity__performance_only, theater_ratio, 300, 0.16).
narrative_ontology:measurement(soc_performance_only_tr_t800, sacrifice_obligation_continuity__performance_only, theater_ratio, 800, 0.2).
narrative_ontology:measurement(soc_performance_only_tr_t1300, sacrifice_obligation_continuity__performance_only, theater_ratio, 1300, 0.24).
narrative_ontology:measurement(soc_performance_only_tr_t1800, sacrifice_obligation_continuity__performance_only, theater_ratio, 1800, 0.28).
narrative_ontology:measurement(soc_performance_only_tr_t1967, sacrifice_obligation_continuity__performance_only, theater_ratio, 1967, 0.34).
narrative_ontology:measurement(soc_performance_only_tr_t2026, sacrifice_obligation_continuity__performance_only, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(soc_performance_only_be_t70, sacrifice_obligation_continuity__performance_only, base_extractiveness, 70, 0.64).
narrative_ontology:measurement(soc_performance_only_be_t300, sacrifice_obligation_continuity__performance_only, base_extractiveness, 300, 0.67).
narrative_ontology:measurement(soc_performance_only_be_t800, sacrifice_obligation_continuity__performance_only, base_extractiveness, 800, 0.69).
narrative_ontology:measurement(soc_performance_only_be_t1300, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1300, 0.71).
narrative_ontology:measurement(soc_performance_only_be_t1800, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1800, 0.73).
narrative_ontology:measurement(soc_performance_only_be_t1967, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1967, 0.75).
narrative_ontology:measurement(soc_performance_only_be_t2026, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2026, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(soc_performance_only_su_t70, sacrifice_obligation_continuity__performance_only, suppression_requirement, 70, 0.42).
narrative_ontology:measurement(soc_performance_only_su_t300, sacrifice_obligation_continuity__performance_only, suppression_requirement, 300, 0.46).
narrative_ontology:measurement(soc_performance_only_su_t800, sacrifice_obligation_continuity__performance_only, suppression_requirement, 800, 0.52).
narrative_ontology:measurement(soc_performance_only_su_t1300, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1300, 0.57).
narrative_ontology:measurement(soc_performance_only_su_t1800, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1800, 0.61).
narrative_ontology:measurement(soc_performance_only_su_t1967, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1967, 0.66).
narrative_ontology:measurement(soc_performance_only_su_t2026, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the sacrifice obligation after the destruction' decomposes into four structurally distinct constraints sharing one kernel (sacrifice_obligation_continuity), per the epsilon-invariance principle. Each reading carries its own epsilon, victim set, and type: performance_only (this file: binding and unfulfillable, high extraction, current generation in the victim set), study_as_performance (satisfied textually, low extraction), messianic_suspension (deferred without liability, moderate readiness costs), archival_preservation (lapsed, negligible extraction, memory function only). The fixed textual corpus is upstream of all four; this reading's categorical denial of textual satisfaction defines what its siblings must deny, so its edges run to all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
