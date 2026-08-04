% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This story instantiates the contraction_reading of the
 *   dueling_disappearance_mechanism kernel: it holds that dueling did not
 *   lose out to competing institutions (courts, banking, libel law) so much
 *   as the underlying cultural axioms that made honor a defensible,
 *   externally-held good were displaced by an equal-dignity substrate. Under
 *   this reading, the relevant constraint is not a coordination mechanism
 *   that got out-competed (rope) but a substrate-level moral architecture
 *   that, once installed, made honor-culture's categories increasingly
 *   unintelligible rather than merely unfashionable or illegal — closer to
 *   mountain than rope. The victim set under this reading includes
 *   honor-culture practitioners and displaced gentry whose framework became
 *   illegible to the surrounding culture, not merely disadvantaged within it.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: primary victims (moderate/trapped) — bear the loss of an intelligible framework, not merely a banned practice
 *   - displaced_aristocratic_gentry: secondary victims (powerful/identity_locked) — lose the status logic honor policed
 *   - dignity_culture_institutions: primary beneficiaries (institutional/arbitrage) — become the ambient, unquestioned substrate
 *   - bourgeois_professional_class: secondary beneficiaries (organized/mobile) — win under the new status logic
 *   - historical_sociologists: analytical observers who adjudicate between this reading and its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.28).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.62).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '34c6ed8a-7b37-4367-9e67-105a84b214ec').
narrative_ontology:cs_kernel_codification('34c6ed8a-7b37-4367-9e67-105a84b214ec', distributed).
narrative_ontology:cs_authority_grounding('34c6ed8a-7b37-4367-9e67-105a84b214ec', practice).
narrative_ontology:cs_interpretation_layer_present('34c6ed8a-7b37-4367-9e67-105a84b214ec').
narrative_ontology:cs_reading_relation('34c6ed8a-7b37-4367-9e67-105a84b214ec', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('34c6ed8a-7b37-4367-9e67-105a84b214ec', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('34c6ed8a-7b37-4367-9e67-105a84b214ec', foundational, worth_is_intrinsic_and_universal_not_externally_contestable).
narrative_ontology:cs_axiom_status(worth_is_intrinsic_and_universal_not_externally_contestable, holdable).
narrative_ontology:cs_axiom_grounding('34c6ed8a-7b37-4367-9e67-105a84b214ec', worth_is_intrinsic_and_universal_not_externally_contestable, deontological).
narrative_ontology:cs_axiom('34c6ed8a-7b37-4367-9e67-105a84b214ec', foundational, honor_as_defensible_external_good_became_categorically_unintelligible).
narrative_ontology:cs_axiom_status(honor_as_defensible_external_good_became_categorically_unintelligible, holdable).
narrative_ontology:cs_axiom_grounding('34c6ed8a-7b37-4367-9e67-105a84b214ec', honor_as_defensible_external_good_became_categorically_unintelligible, conventional).
narrative_ontology:cs_reference_frame('34c6ed8a-7b37-4367-9e67-105a84b214ec', honor_as_externally_defensible_good).
narrative_ontology:cs_drift_state('34c6ed8a-7b37-4367-9e67-105a84b214ec', post_dignity_culture_ascendance, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('34c6ed8a-7b37-4367-9e67-105a84b214ec', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, displaced_aristocratic_gentry).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, universal_equal_dignity_doctrine).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen, officers, and provincial elites whose entire social grammar of reputation, insult, and satisfaction depended on honor as a defensible, external, contestable good. As dignity culture displaced honor's axioms, their framework did not lose an argument — it became unintelligible to the surrounding culture. They cannot exit into a world that still recognizes their categories; the substrate beneath their practice was replaced.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    moderate, generational, trapped, national).

% Landed and titled families whose claim to social precedence rested partly on the credible willingness to duel. As dignity culture universalized worth (equal dignity regardless of rank), the differential status the duel policed lost its cultural referent. Their identity as a class was partly constituted by honor's logic; the displacement did not merely outlaw a practice, it dissolved the categories that made their rank legible as a defensible thing.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, displaced_aristocratic_gentry, payer,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, displaced_aristocratic_gentry, excluded).

% Churches, courts, medical/psychiatric authorities, and emerging middle-class civic associations that propagated the equal-dignity axiom (worth is intrinsic, not won or defended). They did not administer a rule so much as become the ambient substrate within which honor's claims stopped registering as coherent. They benefit by having their moral vocabulary become the only available one.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, dignity_culture_institutions, agenda_setter).

% Rising merchants, lawyers, and professionals whose status depended on contract, credentialing, and reputation-as-record rather than reputation-as-defended-honor. Dignity culture's ascendance validated their status logic over the aristocracy's, converting a contest they were structurally disadvantaged to win (dueling favored military/aristocratic training) into a contest (professional credentialing) they were positioned to win.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).

% Scholars (in the Pinker/Appiah/honor-culture-decline tradition) who read the disappearance of dueling as evidence of a substrate-level moral change rather than a mere policy substitution. They compile comparative data across regions and eras to test whether dignity displacement, rather than institutional competition, best explains the near-total and largely irreversible collapse of dueling as an intelligible practice.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the relevant sense — this reading holds that honor-culture's original coordination function (publicly verifiable, self-enforced reputation defense in the absence of trusted courts) was not out-competed by a better coordination mechanism; it was rendered categorically unintelligible by a substrate change in what 'worth' and 'insult' even mean. There is no coordination problem left for dueling to solve because the terms of the problem dissolved.
% TRANSFER_FUNCTION: Nothing is transferred in the ordinary rope/tangled-rope sense. What moves is intelligibility itself: the capacity to treat honor-based insult-and-satisfaction as a coherent claim migrates away from honor-culture practitioners and toward dignity-culture institutions and the professional class whose status logic dignity culture validates. Honor's vocabulary becomes a private, illegible remainder rather than a live public claim.
% ABSENT_VOICES: Contemporary honor-culture apologists and duelists themselves are absent from the historical record's dominant narration — their own accounts of what they believed they were defending are preserved mostly as artifacts (letters, codes duello, satire) rather than as live testimony taken seriously by the dignity-culture historiography that succeeded them. Their objection — that something real (a defensible, external good) was lost, not merely a bad institution retired — is structurally excluded from the framework now used to narrate the change.
% DISAPPEARANCE_RATIONALE: Under this reading, dignity culture's displacement of honor axioms is treated as substrate-level and largely irreversible (mountain-like): reversing it would require re-manufacturing an entire cultural cosmology of externally defensible worth, not merely repealing anti-dueling statutes or defunding courts. But the verdict is contested against the sibling readings, which hold the disappearance is reversible in principle if institutions atrophied (institutional_displacement_reading) or was multiply sufficient and would have happened anyway by other paths (overdetermined_composite_reading).
% FOUNDING_PROBLEM: Honor culture's dueling code was originally built to solve a real coordination problem: in the absence of reliable courts and neutral arbiters, a public, costly, self-enforced protocol for settling insult and restoring reputation gave elites a way to resolve status disputes without endless private feuding. This reading holds that this founding problem no longer exists in a form dueling could address, because the axioms that made 'insult to honor' a meaningful injury were themselves displaced.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the dignity-culture beneficiary set by comparative historical sociologists (e.g. work in the Pinker/civilizing-process tradition and honor-culture ethnography of surviving pockets — parts of the American South, certain Mediterranean and Caucasus regions, some student dueling fraternities in German-speaking Europe) who document that where honor-culture axioms persist relatively intact, willingness to duel or its structural analogs (violent retaliation for insult) persists alongside them — suggesting the founding problem is not universally dead, only dead where dignity culture's substrate displacement has actually occurred. This is a live cross-check, not self-report by dignity institutions.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, contested).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at interval end) because this reading treats the change as a substrate shift rather than an extraction mechanism — no party is depicted as actively harvesting rents from honor-culture practitioners' loss; the loss is a byproduct of substrate displacement, not a captured transfer. Suppression is authored moderate-high (0.62) because dignity culture's ascendance did involve real coercive elements (anti-dueling statutes, court-martial, excommunication threats, social ostracism of duelists) layered atop the substrate change — the mountain reading does not deny enforcement existed, only that enforcement is the primary causal mechanism. Accessibility collapse is authored very high (0.88): once dignity axioms are installed, honor's alternative framework becomes nearly unrecoverable as a live public option, which is the mountain-like signature this reading claims. Resistance is authored low (0.12): honor-culture practitioners largely could not mount effective resistance because they lacked a framework from which to contest the change once its terms had shifted — this is a symptom of substrate displacement rather than successful suppression of resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-culture practitioners and displaced gentry are declared victims because the constraint (dignity-culture ascendance) actively renders their prior framework illegible — high derived directionality toward the target end, amplified by trapped/identity_locked exit options (they cannot simply choose to keep believing in externally defensible honor once the surrounding culture stops recognizing the category). Dignity-culture institutions and the professional class are declared beneficiaries because the new substrate validates their status logic — low derived directionality, reinforced by arbitrage/mobile exit options reflecting their structural advantage under the new regime.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in a specific direction: it refuses to describe dueling's decline as pure policy success (a rope whose coordination function was cleanly and beneficially retired) by insisting that a real class of practitioners suffered a substrate-level loss of framework, not merely the loss of a bad habit. At the same time it refuses to describe the change as pure extraction (a snare where dignity institutions captured rents from honor-culture practitioners) because no concentrated beneficiary is shown harvesting the victims' loss as a resource — the benefit accrues diffusely to an entire successor culture. The founding_problem_status is authored 'dead' with outside corroboration (surviving honor-culture enclaves) specifically to prevent this reading from being self-servingly asserted only by the dignity-culture beneficiaries themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_vs_institution_ambiguity,
    'Is dueling''s disappearance best explained by a substrate-level change in moral axioms (this reading) or by institutional substitution that happened to correlate with dignity-culture rhetoric (institutional_displacement_reading)?',
    'Comparative case analysis of regions/eras where dignity-culture rhetoric spread without corresponding institutional development (courts, credit/banking systems, libel law) versus regions where institutions developed without dignity-culture rhetoric — if dueling declines with rhetoric alone, this reading gains support; if it declines only with institutional development, the sibling reading gains support.',
    'If institutional substitution is the true driver, this story''s mountain classification and honor-culture-practitioner victim framing overstate the causal role of cultural substrate change; the constraint would properly reclassify toward rope/tangled_rope with courts/banking as the coordinating beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_vs_institution_ambiguity, empirical, 'Whether cultural substrate change or institutional substitution is the operative causal mechanism.').

omega_variable(
    reversibility_of_dignity_substrate,
    'Is the dignity-culture substrate genuinely irreversible (mountain-grade), or could a sufficiently strong institutional collapse (courts, policing, credit systems failing) cause a reversion to honor-culture axioms and a re-emergence of duel-like practices?',
    'Observation of contexts with acute institutional collapse (failed states, post-conflict zones with weak courts) to see whether honor-culture dueling-analog practices re-emerge, and whether this correlates with institutional absence or with pre-existing honor-culture substrate never having been fully displaced.',
    'Evidence of honor-culture practices re-emerging readily under institutional collapse in populations that had ostensibly adopted dignity-culture axioms would undermine the mountain classification and suggest the substrate change is itself downstream of institutional stability — supporting the institutional_displacement_reading instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_dignity_substrate, conceptual, 'Whether dignity-culture substrate displacement is truly irreversible or contingent on institutional conditions this reading treats as secondary.').

omega_variable(
    single_mechanism_vs_overdetermination,
    'Does privileging the substrate-displacement mechanism as explanatorily primary (as this reading does) misrepresent a genuinely overdetermined historical process in which legal prohibition, institutional modernization, cultural shift, and Civil War trauma were each independently sufficient?',
    'Counterfactual historical analysis: for each candidate sufficient condition, assess whether dueling''s decline would plausibly have occurred in that condition''s absence, holding the others fixed — this is inherently difficult given the single historical timeline, but comparative national cases (dueling decline timing relative to each candidate condition across different countries) provide partial evidence.',
    'If overdetermination holds, this reading''s classification of the constraint as primarily mountain-like substrate change is an artifact of choosing one strand of a multi-causal rope; the overdetermined_composite_reading would be the more defensible single account, and this story should be read as a partial, reading-indexed account rather than the full explanation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_mechanism_vs_overdetermination, conceptual, 'Whether isolating the dignity-culture substrate mechanism from co-occurring sufficient causes is analytically defensible or artificially privileges one causal strand.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1750, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(duel_tr_t1790, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1790, 0.06).
narrative_ontology:measurement(duel_tr_t1830, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1830, 0.09).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1860, 0.11).
narrative_ontology:measurement(duel_tr_t1890, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1890, 0.13).
narrative_ontology:measurement(duel_tr_t1930, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1930, 0.15).

% Extraction over time
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.15).
narrative_ontology:measurement(duel_be_t1790, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1790, 0.18).
narrative_ontology:measurement(duel_be_t1830, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1830, 0.22).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1860, 0.25).
narrative_ontology:measurement(duel_be_t1890, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1890, 0.27).
narrative_ontology:measurement(duel_be_t1930, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1930, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dueling_disappearance_mechanism kernel. contraction_reading (this story) treats the decline as substrate-level cultural axiom displacement, classified mountain, with honor-culture practitioners as victims of illegibility. institutional_displacement_reading treats it as institutional substitution (courts, banking, libel law out-competing dueling as a dispute-resolution mechanism), properly classified as rope or tangled_rope with different beneficiary/victim structure centered on institutional actors. overdetermined_composite_reading treats the decline as multiply and independently caused, refusing to privilege any single mechanism. Each carries its own stable epsilon per the ε-invariance principle; they are linked here rather than merged into one hedged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
