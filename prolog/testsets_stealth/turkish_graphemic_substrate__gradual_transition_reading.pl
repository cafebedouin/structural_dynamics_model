% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__gradual_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__gradual_transition_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__gradual_transition_reading
 *   human_readable: Managed Dual-Script Transition Regime (Gradual Reading of the Turkish Graphemic Substrate)
 *   domain: political linguistics / state formation / cultural engineering
 *
 * SUMMARY:
 *   This story instantiates the gradual_transition_reading of the
 *   turkish_graphemic_substrate kernel: a counterfactual arrangement in which
 *   the 1928-era script changeover runs as a managed dual-script period of
 *   five to fifteen years, with parallel publication, bilingual schooling,
 *   and corpus translation, terminating in full Latin adoption. Historically
 *   the abrupt secular-nationalist path won: the changeover was executed in
 *   months, severing the literate generation from the new print sphere
 *   overnight. This file authors only the gradual reading, as a clean
 *   epsilon-invariant constraint; the sibling readings are separate stories
 *   linked through the network. The epsilon referent is the standing
 *   arrangement under contest, the dual-script transitional regime itself,
 *   assessed by this reading's own lights, not the post-transition Latin
 *   steady state the reading endorses. Claim and metrics are independent
 *   authored facts: the reading claims scaffold (a transitional support with
 *   a published terminal date), and the metrics describe the hypothesized
 *   operation honestly, including its real costs to students, provinces, and
 *   the treasury.
 *
 * KEY AGENTS:
 *   - - ministry_of_national_education: Agenda setter (institutional/mobile) — drafts the statute, fixes the terminal date, administers the changeover machinery
 *   - - older_ottoman_literate_citizens: Primary beneficiary (moderate/constrained) — retains access to print and administration in the familiar script through the window
 *   - - ulema_and_manuscript_scholars: Concentrated beneficiary (organized/identity_locked) — corpus custody and institutional employment continue; gains accrue durably here
 *   - - ottoman_script_printers: Secondary beneficiary (moderate/constrained) — existing capital stays productive until retooling is affordable
 *   - - dual_literate_transitional_students: Primary payer (powerless/trapped) — bears the doubled literacy burden without consent or exit
 *   - - provincial_education_administrators: Payer with offsetting gain (organized/constrained) — duplication costs against a longer compliance runway
 *   - - radical_modernizer_faction: Payer-insider (powerful/mobile) — bears slowed homogenization while holding the acceleration lever
 *   - - ottoman_continuity_opposition: Excluded party (organized/identity_locked) — rejects the terminus, held no seat in the drafting commission
 *   - - linguistic_reform_historians: Analytical observer (analytical/analytical) — reads the counterfactual against the comparative record of alphabet reforms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, 0.3).
domain_priors:suppression_score(turkish_graphemic_substrate__gradual_transition_reading, 0.42).
domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__gradual_transition_reading, scaffold).
narrative_ontology:human_readable(turkish_graphemic_substrate__gradual_transition_reading, "Managed Dual-Script Transition Regime (Gradual Reading of the Turkish Graphemic Substrate)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__gradual_transition_reading, "political linguistics / state formation / cultural engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:has_sunset_clause(turkish_graphemic_substrate__gradual_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__gradual_transition_reading, '2b6c06a5-ed33-435f-8105-16da3319dab9').
narrative_ontology:cs_kernel_codification('2b6c06a5-ed33-435f-8105-16da3319dab9', formalized).
narrative_ontology:cs_authority_grounding('2b6c06a5-ed33-435f-8105-16da3319dab9', expertise).
narrative_ontology:cs_interpretation_layer_present('2b6c06a5-ed33-435f-8105-16da3319dab9').
narrative_ontology:cs_reading_relation('2b6c06a5-ed33-435f-8105-16da3319dab9', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2b6c06a5-ed33-435f-8105-16da3319dab9', turkish_graphemic_substrate__secular_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('2b6c06a5-ed33-435f-8105-16da3319dab9', foundational, script_transition_requires_managed_dual_literacy).
narrative_ontology:cs_axiom_status(script_transition_requires_managed_dual_literacy, holdable).
narrative_ontology:cs_axiom_grounding('2b6c06a5-ed33-435f-8105-16da3319dab9', script_transition_requires_managed_dual_literacy, instrumental).
narrative_ontology:cs_axiom('2b6c06a5-ed33-435f-8105-16da3319dab9', foundational, heritage_transmission_constrains_modernization_tempo).
narrative_ontology:cs_axiom_status(heritage_transmission_constrains_modernization_tempo, holdable).
narrative_ontology:cs_axiom_grounding('2b6c06a5-ed33-435f-8105-16da3319dab9', heritage_transmission_constrains_modernization_tempo, deontological).
narrative_ontology:cs_reference_frame('2b6c06a5-ed33-435f-8105-16da3319dab9', managed_dual_script_migration).
narrative_ontology:cs_drift_state('2b6c06a5-ed33-435f-8105-16da3319dab9', counterfactual_mid_transition, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('2b6c06a5-ed33-435f-8105-16da3319dab9', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, older_ottoman_literate_citizens).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, ulema_and_manuscript_scholars).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, ottoman_script_printers).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, dual_literate_transitional_students).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, provincial_education_administrators).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, radical_modernizer_faction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, provincial_education_administrators).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, pedagogical_gradualism).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, managed_institutional_transition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the alphabet transition statute, fixes the terminal date fifteen years out, and runs the changeover machinery: bilingual teacher-training colleges, parallel publication schedules for the official gazette, translation bureaus converting the administrative corpus, and annual review points with authority to adjust the timetable by decree. Funds the duplication out of the central budget and reports progress to the cabinet.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, ministry_of_national_education, agenda_setter,
    institutional, generational, mobile, national).

% Adults schooled before the changeover read and write only the old orthography. Through the transition window they keep full access to newspapers, court filings, and correspondence in the familiar script while the new one spreads through official life around them. Acquiring the Latin letters late in adult life is slow and partial for most, so they remain inside the space the dual-printing rules keep open for them.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, older_ottoman_literate_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Custodians of the Arabic-letter corpus: court records, theological libraries, endowment deeds, calligraphic training. The dual period keeps their archives legible to a new generation of officials and keeps their institutions employed as translation and teaching bodies while the changeover proceeds. Their scholarly standing is bound up with the letters themselves; leaving the corpus behind would mean leaving the profession.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, ulema_and_manuscript_scholars, beneficiary,
    organized, civilizational, identity_locked, national).

% Own the Arabic-sort foundries and press lines built up over decades. The dual-publication mandate keeps demand for their existing capital alive through the whole window instead of stranding it in a single season, and the long runway makes retooling to Latin sorts payable out of operating revenue rather than ruinous.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, ottoman_script_printers, beneficiary,
    moderate, immediate, constrained, regional).

% Children enrolled during the window study both orthographies: doubled literacy hours, slower fluency in each, textbooks existing in two editions. They were enrolled without consent and cannot opt out of the curriculum; the payoff for the doubled effort, durable access to the pre-changeover corpus, arrives decades after they leave school.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, dual_literate_transitional_students, payer,
    powerless, biographical, trapped, national).

% Must staff bilingual classrooms, duplicate textbook orders, and run two filing systems on fixed provincial budgets. Phased deadlines give them years rather than months to comply, and the annual review points let them petition for schedule relief where rural staffing falls short.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, provincial_education_administrators, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, provincial_education_administrators, beneficiary).

% Republican elites who argued the changeover should be completed in months, not years. They hold posts inside the ministries administering the delay and bear its costs: a decade of duplicated administration, continued operation of religious courts in the old script, slower homogenization of the provinces. Their lever is the amendment process, pressing to shorten the terminal date at each review point.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, radical_modernizer_faction, payer,
    powerful, generational, mobile, national).

% Clerics, endowment administrators, and traditionalist men of letters who reject the Latin terminus altogether. The statute schedules the retirement of the old script from official life and gave them no seat in the drafting commission; they agitate through mosque networks and private presses outside the process, and their institutional life is inseparable from the letters slated for retirement.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, ottoman_continuity_opposition, excluded,
    organized, civilizational, identity_locked, national).

% Comparative scholars of alphabet changeovers, including the Soviet latinization campaigns and the Central Asian cyrillicizations and later reversals, who reconstruct what the managed path would have required and compare it against the abrupt reform the historical record actually contains. They hold no stake in either script and publish for audiences outside the country.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, linguistic_reform_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__gradual_transition_reading, ulema_and_manuscript_scholars).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__gradual_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the changeover problem every writing-system reform faces: how a state moves from one graphemic standard to another without cutting the existing literate population off from print, records, and each other. Parallel publication, bilingual schooling, and translation bureaus let information cross the script boundary while the new standard's infrastructure is built.
% TRANSFER_FUNCTION: Moves the costs of the changeover, including duplicated printing, doubled literacy instruction, and corpus translation, onto the central budget, the provincial administrations, and the school-age cohort of the window; moves continued readership and institutional employment to the old-script constituency; and moves official legitimacy to the Latin standard gradually, on a published schedule, rather than by overnight decree.
% ABSENT_VOICES: Two constituencies were never given a seat: the traditionalist faction that rejects the Latin terminus outright, whose preferred outcome the terminal date schedules for extinction, and the mass of barely literate villagers whose exposure to doubled signage, doubled tax forms, and doubled schooling was assumed rather than consulted. Both would object from outside the drafting commission.
% DISAPPEARANCE_RATIONALE: If the dual-script machinery vanished mid-window, publishing, schooling, and administration would snap to one of the two poles overnight, either the abrupt single-decree changeover the historical record actually shows or indefinite continuation of the old standard, because every intermediate arrangement (parallel gazettes, bilingual classrooms, translation bureaus) depends on the statute's schedules and funding lines continuing.
% FOUNDING_PROBLEM: In the late 1920s the republic faced a graphemic fork: the Arabic-derived orthography was opaque to phonemic spelling and tied to the Ottoman-Islamic archive, while the Latin alphabet promised European alignment and mass literacy but was readable by almost none of the existing literate population, and the entire written record of the civilization sat in the old letters.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties, the archival profession attests the underlying problem's persistence: cataloguers of the Ottoman archives reported for decades afterward that the national written record had become illegible to most trained researchers within a generation of the abrupt changeover, and comparative historians of alphabet reform, foreign and Turkish alike, attest that the rupture costs this reading targets were real. No one attests the managed reading's remedy itself, because it was never enacted; corroboration covers the problem, not the counterfactual.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__gradual_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__gradual_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__gradual_transition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__gradual_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__gradual_transition_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).
:- end_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.30, a window-weighted figure: the series runs 0.30 down to 0.16 as the terminal date approaches, because the arrangement's burdens (duplicated infrastructure, doubled instruction) concentrate in the early phase and wind down toward the sunset. Suppression is 0.42 as a raw structural property, unscaled by power or scope; the suppression_requirement series traces enforcement capacity deliberately, since this story's dynamic is enforcement wind-down: mandating dual publication against initial printer and clerical resistance at the start, relaxing as norms settle and the deadline does the coercive work. Theater is low (scalar 0.18) because the dual machinery performs real work (translation, parallel printing, teacher training), with a mild rise late in the window as ceremonial bilingualism outlives its function. Accessibility collapse is moderate (0.45): the statute forecloses the two polar alternatives, but within the window script choice stays open. Resistance is 0.52, two-flanked: traditionalists reject the terminus, modernizers push acceleration. All three tracked series share one time grid (T=0,3,6,9,12,15) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the ministry's position the arrangement is an orderly migration it designed and can amend; from the students' position it is a compulsory doubled burden with deferred payoff; from the ulema's position it is a lifeline for a corpus-bound vocation; from the radical modernizers' position it is a decade of sabotaged homogenization administered by their own colleagues. Same statute, different structural relationships: the engine derives these divergences from the declared roles, exits, and horizons rather than from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: older citizens retain full access at others' expense, the ulema collect institutional continuation, printers collect extended demand. Students sit nearest the full-target end: trapped, powerless, bearing the transfer's labor. Provincial administrators split (payer with a secondary beneficiary position), deriving a mid-range d. The radical modernizer faction carries a directionality override (powerful -> 0.55): the victim declaration alone would derive a near-full-target d, but they co-administer the arrangement, hold the amendment lever, and can convert their cost into accelerated timetables, placing them nearer symmetric than their payer role suggests. Scope is national for most seats, which modestly amplifies effective extraction through verification difficulty; the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's mandate is the transition itself and dies with the terminal date; the classification keeps the dual regime from being misread as either pure coordination (which would ignore who pays the doubled burden) or pure extraction (which would ignore the real changeover function the machinery performs). The drift risk is entrenchment: if the window stretches past its published schedule, duplication spending persists past preservation need, theater rises, and the arrangement decays toward an inertia-carried remnant. The R5 interview supports the live reading: founding_problem_status is live (archive inaccessibility persists a century on) and the disappearance verdict is world_rearranges, so no dead-mandate mismatch flag fires; the sunset_credibility omega carries the entrenchment question instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the turkish_graphemic_substrate kernel; how would instantiating a sibling reading change the structural data?',
    'Side-by-side compilation of the three reading stories: compare sunset presence, victim sets, and enforcement profiles across the ottoman_continuity and secular_nationalist files.',
    'The continuity sibling removes the terminal date entirely (open-ended old-script officialdom, no sunset clause); the secular nationalist sibling removes the dual period entirely (immediate rupture, victim set centered on the severed generation rather than the doubled cohort). Every classification in this file is conditional on the gradualist framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading, what siblings would change structurally.').

omega_variable(
    dual_period_efficacy,
    'Would a managed dual-script window actually have preserved more intergenerational knowledge transfer than the abrupt reform, net of its higher implementation costs?',
    'Comparative evidence from later alphabet changeovers with dual periods, such as the Central Asian cyrillic-to-latin reversals and post-Soviet script politics: measure corpus-access continuity and literacy-acquisition lag under managed versus abrupt regimes.',
    'If dual windows historically decay into de facto single-script operation with a lost cohort anyway, the reading''s benefit claim collapses and the arrangement prices as pure overhead; if continuity gains are real, the transitional costs are partially redeemed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_period_efficacy, empirical, 'Whether the counterfactual''s central benefit survives contact with comparative evidence.').

omega_variable(
    sunset_credibility,
    'Do managed transition regimes actually terminate on their published schedule, or does the dual bureaucracy entrench and stretch the window indefinitely?',
    'Track enforcement and duplication spending against the statutory timetable in comparable dual-period reforms; look for review-point patterns of repeated schedule extension.',
    'If the window reliably stretches, the declared transitional character decays toward an entrenched dual regime carried by inertia, and the sunset clause stops doing classificatory work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_credibility, empirical, 'Whether the terminal date binds in practice.').

omega_variable(
    scholarly_establishment_capture,
    'Does the knowledge-preservation mandate function as an open-ended vehicle for the old-script scholarly establishment to extend its institutional life past any preservation need?',
    'Examine whether translation-bureau staffing and endowment funding scale with remaining untranslated corpus volume (preservation logic) or with establishment headcount (capture logic).',
    'If capture dominates, the beneficiary seat''s gains are rents rather than transition services, and the arrangement''s effective extraction rises above the authored baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholarly_establishment_capture, conceptual, 'Preservation function versus establishment self-extension.').

omega_variable(
    student_burden_regressivity,
    'Does the doubled literacy burden fall disproportionately on rural and poor pupils, who lack the tutoring and printed reinforcement available to urban elites?',
    'Compare literacy-acquisition lag and grade repetition between urban and rural cohorts schooled under dual-orthography curricula in comparable reforms.',
    'If regressive, the payer seat splits: urban students approach symmetry while rural students approach the full-target position, raising measured effective extraction on the weakest seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(student_burden_regressivity, empirical, 'Distribution of the double-literacy cost across class lines.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__gradual_transition_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tgs_gradual_tr_t0, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tgs_gradual_tr_t3, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 3, 0.13).
narrative_ontology:measurement(tgs_gradual_tr_t6, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(tgs_gradual_tr_t9, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 9, 0.17).
narrative_ontology:measurement(tgs_gradual_tr_t12, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(tgs_gradual_tr_t15, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 15, 0.22).

% Extraction over time
narrative_ontology:measurement(tgs_gradual_be_t0, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tgs_gradual_be_t3, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(tgs_gradual_be_t6, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement(tgs_gradual_be_t9, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 9, 0.27).
narrative_ontology:measurement(tgs_gradual_be_t12, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 12, 0.21).
narrative_ontology:measurement(tgs_gradual_be_t15, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 15, 0.16).

% Suppression requirement over time
narrative_ontology:measurement(tgs_gradual_su_t0, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(tgs_gradual_su_t3, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(tgs_gradual_su_t6, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement(tgs_gradual_su_t9, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 9, 0.3).
narrative_ontology:measurement(tgs_gradual_su_t12, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 12, 0.25).
narrative_ontology:measurement(tgs_gradual_su_t15, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 15, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__gradual_transition_reading, information_standard).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__secular_nationalist_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the turkish_graphemic_substrate kernel into three reading-stories per the epsilon-invariance principle: each reading fixes a distinct arrangement (open-ended old-script officialdom / managed dual-script window / immediate Latin adoption) with distinct beneficiary and victim sets and therefore a distinct, stable epsilon. This file authors the gradual_transition_reading; the sibling files carry the other two. Edges run from this reading to both siblings because the gradual statute's terminal date is the pivot both siblings contest: the continuity reading denies the terminus, the secular nationalist reading denies the waiting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
