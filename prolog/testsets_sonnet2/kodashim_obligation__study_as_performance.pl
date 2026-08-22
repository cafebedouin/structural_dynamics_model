% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Study of Kodashim as Cosmic Performance (Talmudic Substitution Reading)
 *   domain: religious/textual/liturgical
 *
 * SUMMARY:
 *   This story instantiates the 'study-as-performance' reading of the
 *   kodashim_obligation kernel: the rabbinic doctrine, grounded in Hosea 14:3
 *   and elaborated in the Talmud (Menachot 110a), that verbal study and
 *   recitation of the laws of sacrifice (Kodashim) IS the cosmic act of
 *   sacrifice, not a substitute awaiting fulfillment and not a mere memorial
 *   of a defunct system. On this reading, the Temple's physical absence is
 *   structurally irrelevant — the cosmic function the sacrificial system
 *   existed to perform is already, fully, being performed through study. This
 *   is a clean, self-contained constraint: it does not describe or average
 *   over the sibling readings (study_as_preparation, which holds the law
 *   binding-but-dormant awaiting messianic restoration; study_as_archive,
 *   which treats the same texts as historical preservation without live
 *   obligation or cosmic function). Those are different constraints with
 *   different beneficiary structures and different ε — see the omega
 *   variables and cs_structure below for how they relate.
 *
 * KEY AGENTS:
 *   - yeshiva_scholars: Primary practitioners and structural beneficiaries (moderate/mobile) — perform the study that constitutes the cosmic act
 *   - diaspora_communities: Beneficiaries (moderate/mobile) — receive full spiritual participation without geographic or institutional dependency
 *   - cosmic_order: Non-agent beneficiary named for completeness — the metaphysical object the practice maintains
 *   - temple_restorationist_factions: Excluded voice (organized/constrained) — hold a competing framing this reading does not engage
 *   - textual_critics: Analytical observer (analytical) — documents the doctrine's historical function without adjudicating its truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.03).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.08).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Study of Kodashim as Cosmic Performance (Talmudic Substitution Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious/textual/liturgical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, 'f7fe116d-6657-4c56-b4bf-fdc595904f78').
narrative_ontology:cs_kernel_codification('f7fe116d-6657-4c56-b4bf-fdc595904f78', fixed_text).
narrative_ontology:cs_authority_grounding('f7fe116d-6657-4c56-b4bf-fdc595904f78', lineage).
narrative_ontology:cs_interpretation_layer_present('f7fe116d-6657-4c56-b4bf-fdc595904f78').
narrative_ontology:cs_reading_relation('f7fe116d-6657-4c56-b4bf-fdc595904f78', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('f7fe116d-6657-4c56-b4bf-fdc595904f78', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_axiom('f7fe116d-6657-4c56-b4bf-fdc595904f78', foundational, study_constitutes_present_cosmic_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_present_cosmic_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('f7fe116d-6657-4c56-b4bf-fdc595904f78', study_constitutes_present_cosmic_fulfillment, theological).
narrative_ontology:cs_axiom('f7fe116d-6657-4c56-b4bf-fdc595904f78', foundational, physical_temple_not_required_for_efficacy).
narrative_ontology:cs_axiom_status(physical_temple_not_required_for_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('f7fe116d-6657-4c56-b4bf-fdc595904f78', physical_temple_not_required_for_efficacy, theological).
narrative_ontology:cs_reference_frame('f7fe116d-6657-4c56-b4bf-fdc595904f78', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('f7fe116d-6657-4c56-b4bf-fdc595904f78', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f7fe116d-6657-4c56-b4bf-fdc595904f78', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, cosmic_order).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, torah_study_communities).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, yeshiva_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, diaspora_communities).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, study_substitutes_for_sacrifice).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, oral_recitation_efficacy_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, hoshea_prophecy_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study tractates of Kodashim (Zevachim, Menachot, Chullin, and others) as ordinary Torah learning, following the Talmudic teaching (grounded in Hosea 14:3, 'let our lips substitute for bulls') that verbal recitation of sacrificial procedure counts as though the sacrifice were offered. They gain the merit and cosmic participation the study is said to confer; they are free to study other tractates instead and lose nothing structurally by not specializing in Kodashim, so their engagement is voluntary rather than coerced.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, yeshiva_scholars, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_performance, yeshiva_scholars, agenda_setter).

% The metaphysical order that sacrificial performance was understood to maintain (atonement, cosmic balance, the flow of blessing) is, on this reading, sustained by the act of study itself rather than by physical offering. Named for completeness as the object the practice is oriented toward; it is not an actor with interests to be extracted from.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, cosmic_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_performance, cosmic_order).

% Communities without access to Jerusalem or any prospect of Temple service receive, on this reading, full spiritual participation in the sacrificial order through study alone. Nothing about their situation is diminished by Temple absence; the practice travels with them wherever they can open a text.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, diaspora_communities, beneficiary,
    moderate, generational, mobile, global).

% Groups oriented toward literal Temple rebuilding and resumed sacrificial practice (some tied to contemporary Temple Mount activism) find their project's urgency undercut by this reading, since it holds that the cosmic function is already fully achieved through study. They are not part of the study-as-performance interpretive community and their preparatory framing is not addressed by this constraint.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, temple_restorationist_factions, excluded,
    organized, civilizational, constrained, regional).

% Scholars of religious studies and Talmudic history who analyze the substitution doctrine's origins (post-70 CE rabbinic response to Temple destruction) and its function in sustaining textual community without asserting or denying its theological truth.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, textual_critics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides continuity of religious meaning and practice after the Temple's destruction by relocating the sacrificial system's efficacy from a physical site and priesthood to an act (study) that any literate community member can perform anywhere, at any time.
% TRANSFER_FUNCTION: No extraction is transferred between parties; the arrangement redirects devotional and cognitive labor (time spent studying Zevachim and Menachot) toward the same cosmic end that sacrificial performance formerly served, without moving resources from one party to another.
% ABSENT_VOICES: Restorationist factions who hold that only physical sacrificial resumption fulfills the law would object that study-as-performance drains the political and religious urgency needed to rebuild the Temple; they are not in dialogue with this reading's interpretive community, which does not treat their objection as a live counter-consideration within its own framework.
% DISAPPEARANCE_RATIONALE: If this specific interpretive doctrine vanished, communities would simply revert to a different available framing (preparation-for-restoration, or historical-archive) of the same underlying practice of studying the same texts; no material arrangement, resource flow, or institution depends on this reading being the one held, since the study itself is not undertaken to prevent any external process from unwinding.
% FOUNDING_PROBLEM: After 70 CE, sacrificial atonement and cosmic maintenance could no longer be performed physically; the rabbis needed a theologically coherent account of how these functions continued without a Temple, priesthood, or altar.
% FOUNDING_PROBLEM_CORROBORATION: The doctrine is attested within rabbinic literature itself (Talmud Bavli, Menachot 110a, citing Hosea) and is corroborated as a historically documented post-destruction theological adaptation by academic scholars of rabbinic Judaism (e.g., historians of the Yavneh period) who are outside the community that benefits devotionally from holding the doctrine true; those scholars describe it as a functional response to institutional loss rather than adjudicating its metaphysical claim.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, world_unchanged).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).
:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because on this reading's own terms there is no transfer from a payer to a beneficiary — no party bears a cost that another collects. Suppression is low (0.08) because no one is coerced into holding this reading; it is a devotional-interpretive stance adopted voluntarily by study communities, with exit as easy as picking up a different tractate or a different theological account. Theater ratio is low (0.10): the study genuinely occurs and is not organized around the appearance of study for some other end. Accessibility collapse is authored moderately high (0.72) because, WITHIN the interpretive community that holds this reading, alternative accounts of what the study is 'for' become largely unnecessary once this doctrine is accepted — the doctrine is totalizing for its adherents even though it coexists calmly with rival readings held by other communities. Resistance is low (0.15): the doctrine draws on well-established classical sources and meets little internal contest from its own practitioners, though it is not adopted by restorationist or archive-reading communities.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal seat divergence here by design: no stakeholder in this story occupies a payer role, so the engine should not find an extractive seat. The one structural tension is between the beneficiary/agenda-setter seat (yeshiva_scholars, who both perform and propagate the doctrine) and the excluded seat (temple_restorationist_factions), who experience the doctrine's prevalence as a competing framework that reduces the political salience of physical restoration — but this is a contest between frameworks, not an extraction relationship, and no base_properties.victims are declared because none exist on this reading's own terms.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (yeshiva_scholars, diaspora_communities, cosmic_order) are declared with no corresponding victims, which should derive low-to-symmetric directionality for the human beneficiaries and leaves cosmic_order outside the directionality computation entirely since it is marked agent:false. No override is needed: the derivation chain already produces the intended near-beneficiary result because no victim group exists to pull any seat toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy classification almost by construction: because the founding problem (how to continue cosmic-atoning function without a Temple) is declared, on this reading, to be FULLY and CONTINUOUSLY solved by study (not merely preserved-for-later, as in study_as_preparation), there is no mandate that has outlived its function — the function is asserted to be presently, fully live every time study occurs. mandatrophy_resolved is correctly left undeclared (false by omission) because there is no drift between original purpose and current operation to resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmic_efficacy_verifiability,
    'Is the claim that study substitutes cosmically for sacrifice a genuine metaphysical fact (making this reading correct and the others mistaken), or is it an unfalsifiable theological framing whose truth cannot be adjudicated from outside the tradition that holds it?',
    'No empirical resolution mechanism exists for the metaphysical claim itself; the closest available evidence is textual-historical (whether Menachot 110a and its Hosea citation were originally intended as literal metaphysical substitution or as consolation rhetoric for a grieving post-Temple community), which historians of rabbinic Judaism can investigate even though it cannot settle the theological question.',
    'If treated as a live metaphysical claim internal to the tradition, this reading stands as a coherent, self-sufficient constraint with beneficiaries but no victims. If treated as consolation rhetoric retrofitted with cosmic claims, the doctrine functions more like study_as_archive (identity-maintenance dressed in stronger metaphysical language), which would push this story''s claimed_type consideration toward a more skeptical framing without changing its authored ε here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_efficacy_verifiability, conceptual, 'Whether study-as-performance is a genuine theological claim or a retrofitted consolation doctrine — irreducible to empirical resolution.').

omega_variable(
    restorationist_delegitimization_pressure,
    'Does the widespread acceptance of study-as-performance structurally reduce the political and material urgency behind Temple restoration movements, and if so, is that a side effect or an unstated function of the doctrine''s popularity?',
    'Comparative sociological study of restorationist movement strength and funding in communities where study-as-performance is dominant versus communities where study-as-preparation or messianic-restoration framings dominate.',
    'If restorationist activity is measurably suppressed where this reading prevails, the reading''s influence relation to the sibling study_as_preparation reading (declared as ''influences'' below) would need to be reconsidered as a stronger, more adversarial structural pressure rather than passive coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restorationist_delegitimization_pressure, empirical, 'Whether the performance doctrine''s prevalence measurably dampens restorationist urgency.').

omega_variable(
    beneficiary_status_of_cosmic_order,
    'Can a non-agent entity (cosmic order/atonement) coherently occupy the beneficiary role in a directionality computation, or does naming it as beneficiary merely launder what is actually a claim about human psychological/communal benefit (meaning, continuity, identity) into cosmic language?',
    'Compare outcomes if cosmic_order is removed from beneficiaries entirely and only human beneficiary groups remain; check whether the engine''s computed classification changes.',
    'If removing the non-agent beneficiary changes nothing (because it is marked agent:false and excluded from directionality by design), the naming is purely narrative/descriptive and carries no structural risk. If it were ever treated as agent:true, it could artificially inflate the appearance of a coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_status_of_cosmic_order, conceptual, 'Whether naming a non-agent cosmic beneficiary is safe narrative completeness or a laundering risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.08).
narrative_ontology:measurement(koda_tr_t8, kodashim_obligation__study_as_performance, theater_ratio, 8, 0.08).
narrative_ontology:measurement(koda_tr_t16, kodashim_obligation__study_as_performance, theater_ratio, 16, 0.09).
narrative_ontology:measurement(koda_tr_t24, kodashim_obligation__study_as_performance, theater_ratio, 24, 0.09).
narrative_ontology:measurement(koda_tr_t32, kodashim_obligation__study_as_performance, theater_ratio, 32, 0.1).
narrative_ontology:measurement(koda_tr_t40, kodashim_obligation__study_as_performance, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(koda_be_t8, kodashim_obligation__study_as_performance, base_extractiveness, 8, 0.02).
narrative_ontology:measurement(koda_be_t16, kodashim_obligation__study_as_performance, base_extractiveness, 16, 0.03).
narrative_ontology:measurement(koda_be_t24, kodashim_obligation__study_as_performance, base_extractiveness, 24, 0.03).
narrative_ontology:measurement(koda_be_t32, kodashim_obligation__study_as_performance, base_extractiveness, 32, 0.03).
narrative_ontology:measurement(koda_be_t40, kodashim_obligation__study_as_performance, base_extractiveness, 40, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
