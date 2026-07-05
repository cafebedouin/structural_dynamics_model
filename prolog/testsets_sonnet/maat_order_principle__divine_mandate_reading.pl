% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at as Divine Mandate Flowing Through Pharaoh (Ruler-as-Source Reading)
 *   domain: religious/political/historical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Ma'at kernel: the divine
 *   mandate reading, in which the Pharaoh is not a party subject to Ma'at's
 *   demands but is Ma'at's terrestrial source and embodiment. Under this
 *   framing, no royal act can by definition constitute a violation of cosmic
 *   order — the category of 'Pharaoh violates Ma'at' is theologically
 *   incoherent within the doctrine itself. This is structurally distinct from
 *   the reciprocity reading (Pharaoh owes obligations that could fail) and
 *   the distributed maintenance reading (Pharaoh is one sustaining actor
 *   among many). Each is a separate constraint with its own ε and stakeholder
 *   structure per the ε-invariance principle; this file claims only the
 *   ruler-as-source variant.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.72).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.81).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, mountain).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at as Divine Mandate Flowing Through Pharaoh (Ruler-as-Source Reading)").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "religious/political/historical").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).
domain_priors:emerges_naturally(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '65c34861-fcdf-43b8-9f09-84f620c6a695').
narrative_ontology:cs_kernel_codification('65c34861-fcdf-43b8-9f09-84f620c6a695', distributed).
narrative_ontology:cs_authority_grounding('65c34861-fcdf-43b8-9f09-84f620c6a695', extraction).
narrative_ontology:cs_interpretation_layer_present('65c34861-fcdf-43b8-9f09-84f620c6a695').
narrative_ontology:cs_reading_relation('65c34861-fcdf-43b8-9f09-84f620c6a695', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('65c34861-fcdf-43b8-9f09-84f620c6a695', maat_order_principle__distributed_maintenance_reading, forecloses).
narrative_ontology:cs_axiom('65c34861-fcdf-43b8-9f09-84f620c6a695', foundational, ruler_ontologically_identical_with_cosmic_order).
narrative_ontology:cs_axiom_status(ruler_ontologically_identical_with_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('65c34861-fcdf-43b8-9f09-84f620c6a695', ruler_ontologically_identical_with_cosmic_order, theological).
narrative_ontology:cs_axiom('65c34861-fcdf-43b8-9f09-84f620c6a695', secondary, royal_acts_cannot_be_evaluated_against_external_standard).
narrative_ontology:cs_axiom_status(royal_acts_cannot_be_evaluated_against_external_standard, holdable).
narrative_ontology:cs_axiom_grounding('65c34861-fcdf-43b8-9f09-84f620c6a695', royal_acts_cannot_be_evaluated_against_external_standard, conventional).
narrative_ontology:cs_reference_frame('65c34861-fcdf-43b8-9f09-84f620c6a695', primordial_cosmic_order_pre_creation).
narrative_ontology:cs_drift_state('65c34861-fcdf-43b8-9f09-84f620c6a695', intermediate_period_fragmentation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('65c34861-fcdf-43b8-9f09-84f620c6a695', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaonic_office).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, royal_court_officials).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, temple_priesthood).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, corvee_laborers).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, provincial_peasantry).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, conquered_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declared the living embodiment of Ma'at itself — not a subject of the order but its terrestrial source. Commissions monuments, taxation, and labor levies as cosmic necessity, immune by definition from any charge of violating Ma'at since the ruler's acts constitute what Ma'at is. Bears no structural cost from the arrangement; every extraction is recast as maintenance of order.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaonic_office, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, pharaonic_office, beneficiary).

% Administer the mandate downward — collecting tribute, organizing labor levies, adjudicating disputes in the ruler's name. Their authority and material position depend entirely on the doctrine that the Pharaoh's word is Ma'at; they have strong incentive to police any rival reading that would make royal conduct answerable to reciprocal obligation.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, royal_court_officials, beneficiary,
    powerful, biographical, mobile, national).

% Controls the ritual apparatus (coronation rites, temple inscriptions, funerary theology) that stages the Pharaoh-as-source doctrine as cosmological fact. Receives land grants, labor, and offerings in exchange for producing and reproducing the theological legitimation; their institutional survival is bound to this specific reading persisting over the reciprocity or distributed readings.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, temple_priesthood, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, temple_priesthood, agenda_setter).

% Conscripted for monument construction, irrigation works, and military campaigns justified as sustaining cosmic order. Cannot appeal to any standard of reciprocal obligation because this reading places the ruler's demands outside the space of things that could be measured against Ma'at — the demand IS Ma'at. No juridical or theological ground exists from which to contest a royal command under this framing.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, corvee_laborers, payer,
    powerless, biographical, trapped, local).

% Surrenders grain surplus and labor obligations to provincial administrators acting under royal mandate. Famine, maldistribution, or arbitrary taxation cannot be framed as royal failure under this reading, since failure would require the ruler to be a subject of Ma'at rather than its source; hardship is instead read as cosmic misfortune, insulating the ruler from accountability.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, provincial_peasantry, payer,
    powerless, generational, trapped, regional).

% Subjugated in campaigns presented as extending Ma'at (order) against isfet (chaos) associated with foreign peoples. Their conquest, tribute extraction, and enslavement are narrated as cosmological necessity rather than political choice, foreclosing any framing of the ruler's war-making as a violable act requiring justification against a standard external to the ruler.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, conquered_populations, payer,
    powerless, biographical, trapped, regional).

% Wisdom literature occasionally voices grievance against unjust rule (e.g. the Eloquent Peasant, admonitions texts), implying rulers CAN fail Ma'at. Under the divine mandate reading such texts are theologically incoherent and are marginalized or reframed as complaints against corrupt officials rather than the crown itself; their voice is structurally excluded from the dominant doctrinal frame.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, dissenting_scribes_and_sages, excluded,
    moderate, generational, constrained, national).

% Reconstruct the doctrine from royal inscriptions, temple reliefs, and coronation texts, and compare the ruler-as-source framing against reciprocity and distributed-maintenance textual evidence to assess which reading dominated at which periods and courts.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, modern_egyptologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unambiguous source of legitimate authority that eliminates succession disputes and jurisdictional ambiguity — everyone knows whose word settles what counts as order, removing a genuine coordination problem in a large agrarian state without an independent judiciary.
% TRANSFER_FUNCTION: Moves grain, labor, and military service from peasantry and conquered populations upward to the crown, temple, and court, cast as flowing toward the maintenance of cosmic order rather than toward particular human beneficiaries.
% ABSENT_VOICES: Peasants, corvee laborers, and conquered peoples have no doctrinal standing from which to declare a royal act a violation of Ma'at, since the reading defines the ruler as Ma'at's source rather than its subject; wisdom-literature complaints exist but are structurally reframed as complaints against officials, not the crown.
% DISAPPEARANCE_RATIONALE: If the divine-mandate reading collapsed, tribute and corvee demands would need independent justification against a standard external to the ruler (as the reciprocity reading requires), succession and taxation disputes would become adjudicable, and the priesthood's staged legitimation apparatus would lose its function — the entire extraction structure currently insulated from challenge would become contestable.
% FOUNDING_PROBLEM: Early Egyptian state formation needed a mechanism to prevent recurring succession crises and provincial fragmentation (as during intermediate periods) by grounding authority in something beyond contestable human consensus — cosmic order itself.
% FOUNDING_PROBLEM_CORROBORATION: Temple and court inscriptions (produced by the beneficiary institutions themselves) attest the doctrine as settled cosmological fact. Independent corroboration is thin: wisdom literature and intermediate-period royal apologetics (e.g. texts justifying restored kingship after fragmentation) implicitly concede that order can and did collapse under specific rulers, undermining the claim that the ruler's acts are definitionally identical with Ma'at. Modern Egyptological synthesis reads the doctrine as a legitimation strategy intensified precisely during and after periods when royal authority visibly failed.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(maat_order_principle__divine_mandate_reading),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.72) and rises over the interval because the doctrine's insulating function increasingly covers demonstrable material extraction (corvee labor, tribute, monument construction) that has no independent justification once the ruler cannot be a violator. Suppression is authored very high (0.81) because the doctrine survives specifically by foreclosing the reciprocity and distributed readings as live alternatives — wisdom-literature dissent is marginalized, not engaged. Theater ratio is moderate-high and rising (0.58) because an increasing share of royal and temple activity (coronation liturgy, monumental inscription, funerary theology) exists to stage the doctrine rather than to perform any independent coordination function. Accessibility collapse is high (0.7): once the ruler-as-source framing is internalized, alternative standards for judging royal conduct become nearly unthinkable within the doctrinal frame. Resistance is authored low (0.28) precisely because the doctrine's core move is to remove itself from the space of things that can be resisted — dissent is reframed as complaint against officials, not the crown, or as impiety rather than political grievance.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaonic and priestly seats, this is not a constraint at all but a description of cosmic fact — the ruler's word simply IS Ma'at, so there is no gap to perceive. From the corvee laborer or provincial peasant seat, the same arrangement operates as an unappealable extraction structure with no available standard of judgment. The engine's per-seat computation should reflect this asymmetry structurally, from the declared power/exit data, not from any narrative adjudication of which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh, court, and priesthood are declared beneficiaries who additionally set and administer the arrangement; they face no structural cost and their positions (temple land grants, court office, monumental legacy) depend on the doctrine's persistence. Corvee laborers, provincial peasantry, and conquered populations are victims with trapped exit options — there is no doctrinal or juridical standing from which they could contest a royal demand as a violation, since the reading defines such a violation as impossible. This is the key structural delta from sibling readings: in the reciprocity reading the same peasants would have a doctrinal basis for grievance (failed reciprocal obligation); here that basis is foreclosed by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (succession crises, provincial fragmentation, need for unambiguous authority) may have been genuinely live during early state formation, but the divine-mandate reading's specific insulation of the ruler from any possibility of failure has no natural expiration and instead intensifies over the interval — precisely the pattern where a coordination-adjacent doctrine (need for a legitimate authority) is used to launder an extraction structure (unaccountable royal extraction) that persists long after unification crises were resolved. The founding_problem_status is authored 'contested' because temple/court sources (self-interested) attest continuity while independent evidence (wisdom literature, intermediate-period apologetics) suggests the doctrine hardened specifically when royal authority was empirically failing — the opposite of what a genuine, functioning natural order would predict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_reading_dominated_when,
    'Was the divine-mandate reading (ruler as unaccountable source of Ma''at) the dominant doctrinal framing throughout Egyptian history, or did it wax and wane relative to the reciprocity and distributed-maintenance readings depending on dynastic strength?',
    'Comparative textual analysis across periods: Old Kingdom royal inscriptions versus Middle Kingdom wisdom literature versus New Kingdom temple theology versus Late Period apologetics, tracking which framing dominates court and temple production in each era, especially around intermediate periods and restorations.',
    'If the divine-mandate reading intensifies specifically during and after periods of demonstrated royal failure (as the intermediate-period record suggests), that supports reading the doctrine as compensatory legitimation rather than a stable, continuously-held cosmological belief — strengthening the case that extraction is the doctrine''s operative function rather than a side effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_dominated_when, empirical, 'Whether the ruler-as-source reading was continuously dominant or a compensatory response to legitimacy crises.').

omega_variable(
    genuine_belief_vs_instrumental_theology,
    'Did royal and priestly actors genuinely believe the ruler was ontologically identical with cosmic order, or was the doctrine understood even by its authors as instrumentally necessary theater to secure compliance?',
    'Analysis of private versus public register texts (if any survive distinguishing court private communication from public monumental inscription), and comparison with known cases of usurpation where the doctrine was retroactively applied to legitimate a ruler who had just violated the prior doctrinal order to seize power.',
    'If usurpers routinely required retroactive doctrinal legitimation (rather than the doctrine self-evidently applying), this suggests even insiders treated the ruler-as-source claim as constructed rather than discovered — strengthening the false-summit-adjacent reading that this ''mountain'' framing is authored consciously by beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_belief_vs_instrumental_theology, conceptual, 'Whether the doctrine was sincerely held cosmology or consciously instrumental legitimation.').

omega_variable(
    natural_law_vs_constructed_extraction_fsm,
    'This reading is claimed as a mountain (emerges_naturally: true, per the doctrine''s own self-presentation as cosmic fact) yet declares concentrated beneficiaries (crown, court, priesthood) — is the natural-law framing itself the constructed artifact, making this a false summit rather than a genuine mountain?',
    'Compare against archaeological and administrative record of actual resource flows (tomb goods, temple land registries, corvee rosters) to determine whether the material extraction pattern tracks the doctrinal claim of cosmic necessity or tracks ordinary patterns of elite rent extraction seen in comparable ancient states without such doctrines.',
    'If the extraction pattern is structurally indistinguishable from ordinary elite rent-seeking in states lacking a divine-mandate doctrine, the ''natural law'' framing is decorative rather than descriptive, and the false_summit_mountain signature should fire, reclassifying this constraint toward tangled_rope despite the claimed_type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_extraction_fsm, conceptual, 'Whether the mountain claim is itself the extraction mechanism (false summit) rather than genuine natural order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 2600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement_basis(maat_tr_t0, projected).
narrative_ontology:measurement(maat_tr_t500, maat_order_principle__divine_mandate_reading, theater_ratio, 500, 0.46).
narrative_ontology:measurement_basis(maat_tr_t500, projected).
narrative_ontology:measurement(maat_tr_t1000, maat_order_principle__divine_mandate_reading, theater_ratio, 1000, 0.5).
narrative_ontology:measurement_basis(maat_tr_t1000, projected).
narrative_ontology:measurement(maat_tr_t1500, maat_order_principle__divine_mandate_reading, theater_ratio, 1500, 0.55).
narrative_ontology:measurement_basis(maat_tr_t1500, projected).
narrative_ontology:measurement(maat_tr_t2000, maat_order_principle__divine_mandate_reading, theater_ratio, 2000, 0.56).
narrative_ontology:measurement_basis(maat_tr_t2000, projected).
narrative_ontology:measurement(maat_tr_t2600, maat_order_principle__divine_mandate_reading, theater_ratio, 2600, 0.58).
narrative_ontology:measurement_basis(maat_tr_t2600, projected).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(maat_be_t0, projected).
narrative_ontology:measurement(maat_be_t500, maat_order_principle__divine_mandate_reading, base_extractiveness, 500, 0.55).
narrative_ontology:measurement_basis(maat_be_t500, projected).
narrative_ontology:measurement(maat_be_t1000, maat_order_principle__divine_mandate_reading, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement_basis(maat_be_t1000, projected).
narrative_ontology:measurement(maat_be_t1500, maat_order_principle__divine_mandate_reading, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement_basis(maat_be_t1500, projected).
narrative_ontology:measurement(maat_be_t2000, maat_order_principle__divine_mandate_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement_basis(maat_be_t2000, projected).
narrative_ontology:measurement(maat_be_t2600, maat_order_principle__divine_mandate_reading, base_extractiveness, 2600, 0.72).
narrative_ontology:measurement_basis(maat_be_t2600, projected).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(maat_su_t0, projected).
narrative_ontology:measurement(maat_su_t500, maat_order_principle__divine_mandate_reading, suppression_requirement, 500, 0.68).
narrative_ontology:measurement_basis(maat_su_t500, projected).
narrative_ontology:measurement(maat_su_t1000, maat_order_principle__divine_mandate_reading, suppression_requirement, 1000, 0.72).
narrative_ontology:measurement_basis(maat_su_t1000, projected).
narrative_ontology:measurement(maat_su_t1500, maat_order_principle__divine_mandate_reading, suppression_requirement, 1500, 0.76).
narrative_ontology:measurement_basis(maat_su_t1500, projected).
narrative_ontology:measurement(maat_su_t2000, maat_order_principle__divine_mandate_reading, suppression_requirement, 2000, 0.79).
narrative_ontology:measurement_basis(maat_su_t2000, projected).
narrative_ontology:measurement(maat_su_t2600, maat_order_principle__divine_mandate_reading, suppression_requirement, 2600, 0.81).
narrative_ontology:measurement_basis(maat_su_t2600, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the maat_order_principle kernel, decomposed per the ε-invariance principle because the natural-language label 'Ma'at' conflates structurally distinct claims about where the constraint's authority originates and who it binds. divine_mandate_reading places the ruler outside the constraint system as source (highest ε, highest suppression, lowest resistance — this file). reciprocity_reading retains an external standard (mutual obligation) against which the ruler can fail, producing lower suppression and higher contestability. distributed_maintenance_reading distributes the maintenance obligation across all social stations, producing the lowest concentration of beneficiaries and the most rope-like structure of the three. All three share the same underlying kernel text/tradition but instantiate different authority structures and different victim/beneficiary sets, hence different ε values — they must be authored as separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
