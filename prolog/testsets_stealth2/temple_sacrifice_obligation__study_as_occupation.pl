% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Occupation of the Temple Obligation
 *   domain: religious/halakhic/commitment-systems
 *
 * SUMMARY:
 *   After the destruction of the Second Temple (70 CE), a
 *   commandment-centered covenantal community retained a fully elaborated
 *   body of sacrificial law whose physical performance had become impossible.
 *   The standing arrangement this story is about is the interpretive
 *   settlement — crystallized in the rabbinic period and consolidated through
 *   the codes, curricula, and daily liturgy — under which engagement with the
 *   sacrificial texts in study legitimately occupies (discharges) the
 *   obligation while the Temple stands absent. The arrangement is claimed as
 *   rope: it solves a real fidelity problem with minimal coercive overhead,
 *   its participants are net beneficiaries, and the alternative interpretive
 *   settlements remain live rather than suppressed. The authored metrics
 *   describe the arrangement's actual operation as this reading assesses it:
 *   a small extractive residue (obligation-energy channeled through
 *   authority-administered forms, sustaining adjudicative necessity),
 *   near-zero suppression, low theater, and no victim set. The interval is
 *   anchored in CE years, from the Mishnah's codification of the sacrificial
 *   order (c. 200) to the present. Assumptions stated: the settlement's
 *   crystallization is dated to the tannaitic-amoraic curricular enterprise;
 *   the extractive residue is read as the authority-sustaining channeling of
 *   obligation-energy, not as any transfer harming bearers.
 *
 * KEY AGENTS:
 *   - halakhic_interpretive_authority: agenda-setter (institutional / identity_locked) — articulates and administers the settlement; collects the legitimacy gain of an obligation kept live without a surfaced revision question
 *   - observant_obligation_bearers: primary participants (moderate / identity_locked) — owe the obligation, receive the study path as discharge, pay authority-directed study-time
 *   - torah_study_institutions: secondary beneficiaries (organized / constrained) — venues and canons kept central by the settlement
 *   - kohanim_priestly_line: displaced patrimony-holders (moderate / identity_locked) — hereditary officiants converted into subject matter; no seat in the settlement's administration
 *   - academic_historians_of_religion: analytical observer (analytical / analytical) — documents the settlement's formation from outside the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.08).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Occupation of the Temple Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic/commitment-systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '9da1e6dd-fd72-44e6-8aba-edb537f2fae1').
narrative_ontology:cs_kernel_codification('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', fixed_text).
narrative_ontology:cs_authority_grounding('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', lineage).
narrative_ontology:cs_interpretation_layer_present('9da1e6dd-fd72-44e6-8aba-edb537f2fae1').
narrative_ontology:cs_reading_relation('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', temple_sacrifice_obligation__study_as_archiving, forecloses).
narrative_ontology:cs_reading_relation('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', temple_sacrifice_obligation__messianic_suspension, forecloses).
narrative_ontology:cs_axiom('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', foundational, study_constitutes_performance).
narrative_ontology:cs_axiom_status(study_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', study_constitutes_performance, theological).
narrative_ontology:cs_axiom('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', foundational, obligation_binding_absent_temple).
narrative_ontology:cs_axiom_status(obligation_binding_absent_temple, holdable).
narrative_ontology:cs_axiom_grounding('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', obligation_binding_absent_temple, deontological).
narrative_ontology:cs_reference_frame('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', post_destruction_study_occupation).
narrative_ontology:cs_drift_state('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('9da1e6dd-fd72-44e6-8aba-edb537f2fae1', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_obligation_bearers).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, halakhic_interpretive_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, torah_study_institutions).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, obligation_continuity_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_preeminence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates and administers the settlement that study of the sacrificial codes discharges the obligation while the Temple stands absent: it fixes curricula (which sacrificial orders are studied, in what sequence), rules on what counts as sufficient engagement, and maintains the daily liturgical rehearsal of the sacrifice order. Its adjudicative role persists because the obligation it interprets remains binding; were the obligation declared lapsed or suspended, a substantial share of its interpretive portfolio would lose its object. Exit from this role would mean dissolving the interpretive office itself.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_interpretive_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, halakhic_interpretive_authority, beneficiary).

% Owe the sacrificial commandments and cannot perform them at the Temple site. The settlement gives them a daily path of discharge: engaging the sacrificial texts in study and reciting the sacrifice order in the liturgy. What flows from them is study-time directed along authority-fixed curricula; what flows to them is a standing answer to the question of whether they are in breach. Leaving the path would mean leaving the covenantal framework altogether rather than choosing another form of engagement.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_obligation_bearers, beneficiary,
    moderate, generational, identity_locked, global).

% Academies, study cycles, and publishing enterprises whose schedules and canons are organized around the sacrificial orders. The settlement keeps those orders central rather than archival, sustaining enrollment, lecture series, and print runs devoted to them. Their alternatives are bounded by the curricula the interpretive authority fixes.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, torah_study_institutions, beneficiary,
    organized, generational, constrained, global).

% Hereditary descendants of the Temple officiants. The sacrificial service they alone could perform is now material anyone may study; the settlement assigns them no special role in its administration and no exclusive claim on its subject matter. Their residual ritual functions (priestly blessing, redemption of the firstborn) survive at the margins. They hold standing by descent to speak about the sacrificial system but hold no seat in the interpretive process that governs how it is studied.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, kohanim_priestly_line, excluded,
    moderate, generational, identity_locked, global).

% Scholars of Second Temple and rabbinic Judaism who study the settlement's formation from outside the covenantal framework: they document the post-destruction adaptation of sacrificial law into study practice and liturgy, and analyze what the adaptation preserved and what it displaced. They take no position inside the framework and bear no part of the obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, academic_historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_occupation, halakhic_interpretive_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-destruction fidelity problem for a commandment-centered community: with the Temple site and apparatus gone, study provides a uniform, teachable, repeatable form in which the obligation to the sacrificial service remains actively engaged rather than lapsed — one curriculum, one liturgical rehearsal, one interpretive standard shared across a dispersed community.
% TRANSFER_FUNCTION: Moves obligation-engagement from the destroyed sacrificial apparatus into study practice: bearers contribute study-time and liturgical attention along authority-fixed curricula; fulfillment flows back to the bearers; adjudicative necessity, curricular centrality, and the avoidance of a canon-obsolescence admission flow to the interpretive authority and its institutions.
% ABSENT_VOICES: The priestly line holds standing by descent to speak for the sacrificial service its ancestors performed, but the settlement was articulated by the sage class and assigns the priesthood no seat in governing how the service is studied or remembered. Within the framework, the party to whom the service is owed is present only through the interpretive authority's mediation — no independent voice attests that study is accepted in place of offering. Outside the framework, critical historians who read the settlement as institutional self-preservation are not participants in halakhic discourse.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight — if study no longer counted as occupation — every obligation-bearer would wake to a standing unanswered breach question; the daily liturgy's sacrifice order would lose its operative meaning and become either empty recitation or an unbearable daily reminder of non-performance; the study institutions would lose a curricular anchor; and the interpretive authority would face immediate pressure to adopt a replacement settlement, since the framework does not permit simply ignoring a binding commandment. The framework's practice-world reorganizes around whichever replacement wins.
% FOUNDING_PROBLEM: After 70 CE the community lost the Temple, altar, and priesthood through which its central commandments were performed. The arrangement was built to answer: does a commandment that cannot be performed leave its bearers in daily breach, and if not, in what form does fidelity to it continue?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested from outside the beneficiary set by academic historians of rabbinic Judaism, who independently document the post-destruction crisis of the sacrificial commandments and the adaptation of sacrifice law into study and liturgy. Within the framework it is corroborated by the priestly line's continuing descent-based standing and by the daily liturgical rehearsal itself, which all rites preserve. That the problem remains live is corroborated by the unbroken daily rehearsal across sixteen centuries and by contemporary legal rulings that continue to treat sacrificial law as practically oriented; no source outside the beneficiary set attests that the problem is dead.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.15 at interval end) because under this reading the obligation is discharged in its current form: study is intrinsically valued — it fulfills the standing study commandment and, on this reading, the sacrificial obligation simultaneously — so bearers pay directed time for a good they already endorse. The residue that remains is the channeling of obligation-energy through authority-fixed curricula, which sustains the interpretive office's necessity; the series shows it accumulating mildly (0.08 to 0.15) as the settlement's institutional consolidation deepened. Suppression is near zero (0.08) and is authored as a static scalar: the settlement coerces no one, its persistence needs no enforcement machinery, and the enforcement picture does not change over the interval — so no suppression_requirement series is authored. Theater (0.12) is low because study is the real function; the performative share is rote liturgical recitation of the sacrifice order, which in mass practice substitutes for engaged study. Accessibility_collapse is low (0.20): the alternative settlements and the option of diminished engagement remain accessible within the tradition's own discourse — the settlement forecloses nothing. Resistance is low (0.15): the settlement is the mainstream; friction appears only at the margins (restorationist currents pressing past it, secular attrition, historical voices weighting preservation over fulfillment). Both series share one seven-point grid (200, 500, 800, 1180, 1560, 1948, 2026).
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience the same settlement differently, and the engine computes the divergence from the structural data. From the interpretive authority's seat the settlement is the framework's successful self-repair: an impossibility absorbed without any surfaced revision of the canon, the obligation kept live, the office kept necessary. From the bearer's seat it is a fulfillment path — the difference between daily breach and daily discharge. From the priestly line's seat it is the quiet conversion of a hereditary office into common subject matter. From the analytical seat it is a textbook post-destruction adaptation mechanism. No seat is a victim; the divergence runs between fulfillment, administration, displacement, and explanation — not between benefit and extraction of any magnitude.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared and no victims are: bearers, authority, and study institutions all sit toward the beneficiary end of directionality. The bearers sit nearest symmetric (they pay directed study-time against the fulfillment they receive); the institutions sit lowest (they collect enrollment and curricular centrality while paying nothing specific to the arrangement); the authority sits near the beneficiary end but not at zero — it also bears the maintenance cost of the interpretive apparatus, and its gain is the coordination function itself. No directionality overrides are authored: the derivation from beneficiary declarations plus exit profiles captures the seats, and the one differentiated seat the declarations do not cover — the priestly line, mildly displaced but outside both arrays — cannot be singled out by a per-power-atom override without colliding with same-power beneficiaries, so its position is documented here rather than forced into the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two misreadings. Reading the settlement as extraction would require a victim set; there is none — the arrangement's participants are net beneficiaries, which is the rope signature. Reading it as inertial survival would require an atrophied function; the function is live — study of the sacrificial orders remains an active, valued practice. The mandatrophy question — has the founding problem outlived the arrangement? — is answered no: the founding problem (fidelity to an unperformable commandment) is live for as long as the Temple is absent and the obligation is held binding, and the R5 consumer reads founding_problem_status (live) against disappearance_verdict (world_rearranges) as matched — no capture or zombie flag. The genuine obsolescence risk sits at the restoration horizon: if the Temple were restored, the settlement's function would end without any declared sunset — that contingency is carried in the messianic_horizon_sunset_status omega rather than forced into a scaffold claim the tradition itself does not make.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_settlement,
    'This constraint is one reading of the temple_sacrifice_obligation kernel — the study_as_occupation reading. What would change structurally if a sibling reading were adopted instead, and where exactly is the disagreement located?',
    'The disagreement is located in one structural element: whether study discharges the obligation in the present (this reading), merely preserves knowledge for restoration (study_as_archiving), or leaves the obligation suspended (messianic_suspension). Adoption of a sibling is resolvable only by the framework''s own interpretive authorities; external data cannot settle it.',
    'Under study_as_archiving the arrangement leaves a standing unfulfilled obligation with no discharge path (higher extraction referent; study becomes instrumental). Under messianic_suspension the present-tense obligation dissolves entirely (no fulfillment structure, no occupation to assess). This story''s epsilon, beneficiary set, and type are valid only for the occupation reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_settlement, conceptual, 'Committer structure: which kernel reading this constraint instantiates and where sibling readings diverge.').

omega_variable(
    divine_acceptance_grounding,
    'Is the study-equivalence claim grounded theologically (study is accepted in place of offering because the tradition transmits divine acceptance) or conventionally (the equivalence binds because the community accepts rabbinic interpretive authority)?',
    'Reception-history analysis of the talmudic equivalence dicta against critical philology on their formation; comparative evidence from communities that weight the interpretive chain differently.',
    'If theological, the equivalence is fixed within the framework and the settlement''s authority is not renegotiable by practice; if conventional, communal practice could re-weight what counts as occupation, shifting both the extraction residue and the theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_acceptance_grounding, conceptual, 'Epistemic grounding of the study-equals-offering axiom: revealed acceptance or enacted convention.').

omega_variable(
    recitation_sufficiency_ambiguity,
    'Does liturgical recitation of the daily sacrifice order count as the occupation the settlement names, or does only engaged study discharge the obligation?',
    'Analysis of legal rulings on the sufficiency of recitation versus study; observation of what mass practice actually does and what the authorities count it as.',
    'If recitation suffices, mass practice discharges the obligation cheaply and the theater ratio measures thinning engagement; if only study counts, most bearers carry a daily unfulfilled remainder the settlement''s own terms conceal — raising effective extraction and resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recitation_sufficiency_ambiguity, conceptual, 'Whether the occupation in practice is study or recitation.').

omega_variable(
    messianic_horizon_sunset_status,
    'Is the messianic restoration horizon a functional sunset clause for this settlement, or an indefinite horizon under which study-occupation is the steady state?',
    'Analysis of whether the tradition''s own texts frame study-occupation as transitional preparation for a restored service or as a standing form of fidelity; examination of whether sacrifice study is assigned value independent of restoration.',
    'If the horizon functions as a sunset, the arrangement is transitional and reclassifies toward a scaffold-like profile; if indefinite, the rope-like steady-state reading holds and no sunset is owed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_horizon_sunset_status, conceptual, 'Whether the restoration horizon operates as a built-in termination condition.').

omega_variable(
    authority_necessity_residue,
    'Is the legitimacy and adjudicative necessity the settlement sustains for the interpretive authority a genuine coordination cost (someone must fix what counts as occupation) or an extractive residue (the authority''s role persists because the settlement keeps the obligation live)?',
    'Counterfactual analysis: whether study norms could be self-administered without an adjudicative office — whether the settlement''s function survives with the authority structure removed.',
    'If separable, the authority''s gain is rent and the arrangement carries a tangled-rope residue; if inseparable, the gain is the price of the coordination itself and the rope reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_necessity_residue, conceptual, 'Whether the authority''s sustained role is coordination cost or extraction residue.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 200, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tso_occupation_tr_t200, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 200, 0.05).
narrative_ontology:measurement(tso_occupation_tr_t500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 500, 0.06).
narrative_ontology:measurement(tso_occupation_tr_t800, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 800, 0.09).
narrative_ontology:measurement(tso_occupation_tr_t1180, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1180, 0.1).
narrative_ontology:measurement(tso_occupation_tr_t1560, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1560, 0.12).
narrative_ontology:measurement(tso_occupation_tr_t1948, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1948, 0.12).
narrative_ontology:measurement(tso_occupation_tr_t2026, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(tso_occupation_be_t200, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 200, 0.08).
narrative_ontology:measurement(tso_occupation_be_t500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 500, 0.1).
narrative_ontology:measurement(tso_occupation_be_t800, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 800, 0.12).
narrative_ontology:measurement(tso_occupation_be_t1180, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1180, 0.13).
narrative_ontology:measurement(tso_occupation_be_t1560, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1560, 0.14).
narrative_ontology:measurement(tso_occupation_be_t1948, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(tso_occupation_be_t2026, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 2026, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__study_as_occupation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrifice obligation after the destruction' decomposes into three structurally distinct constraints — this story (study discharges the obligation), study_as_archiving (study preserves knowledge for restoration without fulfilling), and messianic_suspension (the obligation is held in abeyance pending restoration). Each carries its own stable epsilon, fulfillment structure, and beneficiary set; no averaging occurs. This reading's epsilon is low because the obligation is discharged in its present form. The readings are linked both as kernel siblings via cs_structure.reading_relations and as constraint-family members via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
