% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Study-as-Fulfillment Reading of the Sacrificial Obligation
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel
 *   sacrifice_obligation_continuity: that the study of sacrifice law is
 *   itself fulfillment of the commandment, so the obligation persists and is
 *   discharged through textual engagement. The standing arrangement under
 *   contest is the post-destruction persistence of the sacrificial
 *   obligation; epsilon is authored for THAT arrangement as this reading sees
 *   it, never for the rights-respecting or restorationist alternative. The
 *   claim/metric gap is deliberate and small here: the reading is CLAIMED as
 *   rope, and the authored metrics describe a genuinely low-extraction,
 *   lightly enforced arrangement, because the reading's whole design is to
 *   make the obligation cheaply satisfiable. Sibling readings
 *   (performance_only, messianic_suspension, archival_preservation) are
 *   separate constraints in separate files with their own epsilon, party
 *   structures, and classifications; they are linked through
 *   network.affects_constraints and are not averaged into this one.
 *
 * KEY AGENTS:
 *   - - torah_scholars: Primary beneficiary and co-administrator (organized/identity_locked) — their daily labor is the fulfilling act; the doctrine's persistence is their vocation's meaning
 *   - - rabbinic_academies: Agenda-setter (institutional/mobile) — curate, transmit, and adjudicate the reading across generations
 *   - - lay_textual_students: Secondary beneficiary (moderate/constrained) — discharge the obligation through accessible study cycles
 *   - - diaspora_jewish_communities: Collective beneficiary (organized/identity_locked) — maintain covenantal continuity without a cultic center
 *   - - temple_restorationist_movements: Excluded objector (moderate/identity_locked) — deny the equation from outside the adjudicating institutions
 *   - - women_outside_study_obligation: Excluded party (moderate/constrained) — the fulfilling channel was sized to male obligation and schedules
 *   - - comparative_ritual_historians: Analytical observer (analytical/analytical) — documents the substitution without normative stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.18).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.26).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Study-as-Fulfillment Reading of the Sacrificial Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, 'f44a455a-8349-4c3c-ad9f-b5181af18ec5').
narrative_ontology:cs_kernel_codification('f44a455a-8349-4c3c-ad9f-b5181af18ec5', fixed_text).
narrative_ontology:cs_authority_grounding('f44a455a-8349-4c3c-ad9f-b5181af18ec5', lineage).
narrative_ontology:cs_interpretation_layer_present('f44a455a-8349-4c3c-ad9f-b5181af18ec5').
narrative_ontology:cs_reading_relation('f44a455a-8349-4c3c-ad9f-b5181af18ec5', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('f44a455a-8349-4c3c-ad9f-b5181af18ec5', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('f44a455a-8349-4c3c-ad9f-b5181af18ec5', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('f44a455a-8349-4c3c-ad9f-b5181af18ec5', foundational, sacrifice_commandment_remains_binding).
narrative_ontology:cs_axiom_status(sacrifice_commandment_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('f44a455a-8349-4c3c-ad9f-b5181af18ec5', sacrifice_commandment_remains_binding, deontological).
narrative_ontology:cs_axiom('f44a455a-8349-4c3c-ad9f-b5181af18ec5', foundational, textual_study_constitutes_fulfillment).
narrative_ontology:cs_axiom_status(textual_study_constitutes_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('f44a455a-8349-4c3c-ad9f-b5181af18ec5', textual_study_constitutes_fulfillment, conventional).
narrative_ontology:cs_reference_frame('f44a455a-8349-4c3c-ad9f-b5181af18ec5', textual_fulfillment_equilibrium).
narrative_ontology:cs_drift_state('f44a455a-8349-4c3c-ad9f-b5181af18ec5', contemporary_restorationist_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('f44a455a-8349-4c3c-ad9f-b5181af18ec5', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, torah_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, lay_textual_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, talmudic_study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, hosea_lips_for_bulls_exegesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devote their lives to the study of the sacrificial tractates and related law. Under this reading their daily textual labor is itself the sacrificial service, so their ordinary work doubles as complete discharge of the commandment. They transmit the equivalence doctrine through teaching and ordination, and their vocational standing rests on the equation holding. Leaving the tradition would forfeit the meaning structure of their entire vocation, not merely a job.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, torah_scholars, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__study_as_performance, torah_scholars, agenda_setter).

% Set the curricula that schedule the sacrificial material, certify the transmitters, and house the interpretive machinery that keeps the equivalence doctrine operative. The academies persist across generations of individual teachers and could in principle reframe the doctrine, but the surrounding edifice of liturgy, schooling, and patronage is built around the current framing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, rabbinic_academies, agenda_setter,
    institutional, generational, mobile, global).

% Fulfill the sacrificial commandment through regular study cycles open to any literate participant, wherever they live. The practice costs time and attention but no money, travel, or ritual infrastructure. Communal expectation sustains participation; scaling back carries mild social cost but leaves no unsatisfiable residue, since whatever study was done counted fully.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, lay_textual_students, beneficiary,
    moderate, biographical, constrained, global).

% Maintain covenantal continuity across dispersion without an altar, priesthood, or central shrine. The liturgical calendar embeds the sacrificial passages, and the reading lets every scattered community meet the same obligation the Temple cult once centralized. Membership in the community is constitutive of identity rather than a chosen affiliation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, diaspora_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% Hold that study prepares for restored sacrifice rather than fulfilling it, and work toward physical reconstruction of the cult. Their arguments circulate in publications and pilgrimages but carry no adjudicative weight inside the academies that transmit the dominant reading. They are present at the edge of the conversation without a seat in it.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, temple_restorationist_movements, excluded,
    moderate, generational, identity_locked, global).

% In the classical framing the recurring study obligation bound men, and the fulfilling practice was calibrated to male schedules and schooling. Women's access to the fulfilling channel arrived unevenly and late, and their standing under the equivalence doctrine remains unsettled in parts of the tradition. They live inside the communities the reading serves while the practice was not originally shaped around them.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, women_outside_study_obligation, excluded,
    moderate, biographical, constrained, global).

% Document how post-destruction communities converted an unperformable cult into a textual practice, comparing the pattern with parallel substitutions in other traditions. They take no side on whether the substitution discharges anything; their contribution is the record of how the move was made and defended.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, comparative_ritual_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__study_as_performance, torah_scholars).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of keeping a geographically dispersed obligated community in continuity with a commandment whose original performance site no longer exists: it channels the obligation into a universally accessible practice requiring no altar, no priesthood, and no journey, so that fulfillment no longer depends on geography or institutional survival.
% TRANSFER_FUNCTION: Moves time, attention, and scholarly labor from individuals into the textual tradition; moves interpretive authority and communal deference toward the scholarly class that transmits the material; and returns a discharge of obligation to every participant in proportion to engagement.
% ABSENT_VOICES: Temple restorationists would object that study prepares rather than fulfills and that the community has substituted consolation for commandment; they stand outside the academies that adjudicate the question. Women historically exempt from the recurring study obligation would ask why the fulfilling channel was sized to men's schedules. Priestly lineages whose hereditary office is suspended would note that the reading retires their function without their consent.
% DISAPPEARANCE_RATIONALE: If the equivalence doctrine vanished overnight, the community would face a binding commandment with no possible performance: either widespread consciousness of standing violation, or a mass shift toward restorationist politics to make performance possible again. The daily study cycles, the liturgical placement of the sacrificial passages, and the scholarly vocations built on the material would lose their point simultaneously.
% FOUNDING_PROBLEM: The destruction of the Second Temple left sacrificial commandments binding yet unperformable: an obligation with no site, no altar, and no priesthood. The reading was built to answer how an obligated community lives under a commandment it cannot physically discharge.
% FOUNDING_PROBLEM_CORROBORATION: Josephus, a contemporary non-beneficiary witness, records the cessation of the daily sacrifice and the community's distress at it. The continuous liturgical petitions for restoration, composed across many centuries and territories, attest that the sense of an unperformable obligation never lapsed. Academic historians of religion with no confessional stake document the post-destruction adaptation as a real institutional response to a real rupture. Notably, holders of all three sibling readings agree the destruction created the problem; they dispute only its resolution, which is itself corroboration that the problem is not a beneficiary invention.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the reading converts an unperformable obligation into an accessible one: the cost of compliance is time and literacy, not money, travel, or ritual infrastructure, and whatever is done counts fully. Suppression (0.26) reflects residual normative binding force — the commandment still obligates, and communal expectation sustains participation — but nothing is coerced that participants do not already have reason to do, and suppression is authored as a raw structural property, unscaled by power or scope; only extractiveness gets scaled downstream. Theater ratio (0.16) is low because, from this reading's own lights, study IS the function rather than a performance standing in for a missing function; the slow creep upward tracks liturgical recitation of the sacrificial passages becoming habitual alongside actual study. Accessibility collapse (0.42) is moderate because the sibling readings remain live, practiced alternatives rather than collapsed options. Resistance (0.30) reflects the restorationist rejection of the equation plus internal strains over access. The suppression_requirement series DECLINES across the interval: early in the tradition the equivalence doctrine needed active defense against authorities who read it as evasion; as it internalized, the enforcement burden fell — a normalization trajectory, not enforcement decay of a failing arrangement. All series run on one shared time grid (points 0-50 at decade steps) so no metric borrows another's end-state value.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the scholar seat the arrangement is nearly pure subsidy: the doctrine makes their ordinary work count as the tradition's highest service. From the restorationist seat the same structure operates as a suppression of the performance standard — a community talking itself out of a commandment. From the lay seat it is a low-cost fulfillment with mild participatory pressure. The engine derives these divergences from the power, exit, and role data; this story authors the structure and does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party sits in the beneficiary set — scholars, students, and communities all gain an accessible discharge — so derived directionality runs low for all seated agents and effective extraction stays near the floor everywhere. No victim group is declared because, under this reading, the obligation is satisfied rather than displaced onto anyone. The excluded seats (restorationists, women outside the study obligation) are commentary-grade absences: they mark where objection lives, but per the R3 ruling an authored absence drives no classification override and no directionality correction. The access_equity omega below marks the one place a hidden target population could exist — strata who cannot reach the fulfilling practice would bear an unsatisfiable obligation the headline numbers miss.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: the Temple still stands destroyed, and the condition the reading answered persists. No mandatrophy is declared. The rope classification guards against two misreadings. Against the piton reading: if the archival sibling were true, these same study practices would be inertial memory-theater with no normative function, and the theater ratio would be the whole story — this story's low theater_ratio and live founding problem are what separate the two. Against the snare reading: a snare requires identifiable victims, and under this reading's own lights there are none; the access_equity omega is the designated place where a snare-flavored residue (an obligated population locked out of the fulfilling channel) would surface if it exists. The classification thus prevents both flattery (calling a dead ritual alive) and calumny (calling subsidized fulfillment extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel sacrifice_obligation_continuity; how would the classification change if a sibling reading were adopted instead?',
    'Corpus comparison across the four sibling stories: each instantiates the same kernel with different beneficiary/victim structures, and cross-reading deltas locate what each reading adds or removes.',
    'If performance_only prevailed, a victim set appears immediately — everyone unable to perform bears an unsatisfiable obligation — and extractiveness rises sharply. If archival_preservation prevailed, the obligation dissolves and these practices become memory-work with no normative force. This story''s low-extraction profile is contingent on the study_as_performance premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: the same practices classify differently under each sibling reading of the shared kernel.').

omega_variable(
    fulfillment_sincerity,
    'Is study experienced by practitioners as genuine fulfillment of the commandment, or as a consoling substitute for a lost performance?',
    'Analysis of devotional literature, responsa, and practitioner testimony across periods: language of discharge versus language of longing tracks the difference.',
    'If the dominant experience is substitution-with-longing, the theater_ratio is understated and the arrangement drifts toward maintained performance of a missing function; if discharge-language dominates, the low theater figure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_sincerity, empirical, 'Whether the equivalence is lived as fulfillment or as consolation.').

omega_variable(
    access_equity,
    'Does the accessibility premise hold across the obligated population — across literacy, gender, class, and historical period — or do strata exist who cannot reach the fulfilling practice?',
    'Historical literacy and schooling data mapped against the obligation''s demographic scope; doctrinal analysis of the exemption rules that shaped who was ever bound to the study channel.',
    'If substantial strata were obligated-but-unable, a latent victim set exists beneath the no-victim declaration, effective extraction is understated, and the arrangement carries a snare-flavored residue the headline metrics miss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_equity, empirical, 'Whether the fulfilling channel is genuinely accessible to all whom the obligation binds.').

omega_variable(
    restoration_contingency,
    'Would this reading sunset upon physical restoration of the cult — converting study back into preparation — or does it claim permanence independent of circumstances?',
    'Textual analysis of the reading''s own conditionals in Talmudic and later codification: whether the equivalence is framed as interim provision or as a standing truth about the commandment''s nature.',
    'If interim, the arrangement carries hidden transitional character and its persistence after any restoration would be mandatrophy; if permanent, the rope classification is stable across all futures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_contingency, conceptual, 'Whether the reading is scoped to the absence of the Temple or unconditional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(sacr_tr_t10, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t30, observed).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t40, observed).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 50, 0.16).
narrative_ontology:measurement_basis(sacr_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 10, 0.21).
narrative_ontology:measurement_basis(sacr_be_t10, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 20, 0.2).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t30, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 30, 0.19).
narrative_ontology:measurement_basis(sacr_be_t30, observed).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(sacr_be_t40, observed).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 50, 0.18).
narrative_ontology:measurement_basis(sacr_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t10, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(sacr_su_t10, observed).
narrative_ontology:measurement(sacr_su_t20, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 20, 0.3).
narrative_ontology:measurement_basis(sacr_su_t20, observed).
narrative_ontology:measurement(sacr_su_t30, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 30, 0.27).
narrative_ontology:measurement_basis(sacr_su_t30, observed).
narrative_ontology:measurement(sacr_su_t40, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 40, 0.24).
narrative_ontology:measurement_basis(sacr_su_t40, observed).
narrative_ontology:measurement(sacr_su_t50, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 50, 0.22).
narrative_ontology:measurement_basis(sacr_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'what happened to the sacrifice obligation' covers four structurally distinct claims that cannot share one epsilon. Each sibling gets its own story, its own beneficiary/victim structure, and its own classification; this story links to all three. The upstream common ground (the historical destruction and cessation, attested by non-beneficiary witnesses) is cited by every reading as the problem statement; the readings diverge on the normative response, so influence runs from the shared factual substrate into each sibling rather than between siblings, except where a reading's adoption would foreclose another's core premise (this reading versus archival_preservation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
