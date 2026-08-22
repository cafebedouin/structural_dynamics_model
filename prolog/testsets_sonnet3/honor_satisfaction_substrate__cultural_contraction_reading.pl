% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor-Satisfaction Substrate (Cultural Contraction Reading): Collapse of Honor Code as Interpretive Ground for Dueling
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This story instantiates the cultural-contraction reading of the
 *   honor-satisfaction substrate kernel. In this reading, dueling's
 *   disappearance is not a story of an external authority successfully
 *   suppressing a persistent practice; it is a story of the interpretive
 *   ground that made dueling meaningful as 'satisfaction' eroding out from
 *   under it as cultures of honor gave way to cultures of dignity. The
 *   theater_ratio series shows a bulge in the mid-to-late 19th century
 *   (peaking around 1910 at 0.5) representing the period when dueling
 *   persisted mainly as increasingly ceremonial, survivalist performance —
 *   officers and aristocrats going through motions whose original
 *   interpretive stakes had already begun to hollow out — before dropping
 *   sharply as even the ceremonial form became unintelligible and effectively
 *   vanished by 1930. This differs sharply from the sibling
 *   practice_decline_reading, where suppression/enforcement metrics would be
 *   the dynamic story; here the honor code's own collapse as shared cultural
 *   grammar is the mechanism, and legal prohibition is at most a lagging
 *   codification of an already-dead norm rather than the cause of decline.
 *
 * KEY AGENTS:
 *   - professional_middle_class: beneficiary (organized/mobile) — status logic displaces honor entirely, benefits without administering the collapse
 *   - bourgeois_civil_institutions: beneficiary (institutional/analytical) — absorbs reputational adjudication by default as honor substrate withers
 *   - former_dueling_class_aristocracy_officer_corps: payer (powerful/trapped) — loses the interpretive ground for a practice central to their status vocabulary
 *   - dignity_culture_intelligentsia: agenda_setter (institutional/analytical) — supplies the replacement moral vocabulary that displaces honor
 *   - historians_and_sociologists_of_honor: observer (analytical/analytical) — traces the substrate-collapse mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.12).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor-Satisfaction Substrate (Cultural Contraction Reading): Collapse of Honor Code as Interpretive Ground for Dueling").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, 'de412552-19b4-4098-aa44-20b505f84dae').
narrative_ontology:cs_kernel_codification('de412552-19b4-4098-aa44-20b505f84dae', implicit).
narrative_ontology:cs_authority_grounding('de412552-19b4-4098-aa44-20b505f84dae', practice).
narrative_ontology:cs_interpretation_layer_present('de412552-19b4-4098-aa44-20b505f84dae').
narrative_ontology:cs_reading_relation('de412552-19b4-4098-aa44-20b505f84dae', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('de412552-19b4-4098-aa44-20b505f84dae', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('de412552-19b4-4098-aa44-20b505f84dae', foundational, honor_is_a_shared_interpretive_construction_not_a_fixed_norm).
narrative_ontology:cs_axiom_status(honor_is_a_shared_interpretive_construction_not_a_fixed_norm, holdable).
narrative_ontology:cs_axiom_grounding('de412552-19b4-4098-aa44-20b505f84dae', honor_is_a_shared_interpretive_construction_not_a_fixed_norm, conventional).
narrative_ontology:cs_axiom('de412552-19b4-4098-aa44-20b505f84dae', foundational, practice_disappearance_tracks_substrate_collapse_not_enforcement_success).
narrative_ontology:cs_axiom_status(practice_disappearance_tracks_substrate_collapse_not_enforcement_success, holdable).
narrative_ontology:cs_axiom_grounding('de412552-19b4-4098-aa44-20b505f84dae', practice_disappearance_tracks_substrate_collapse_not_enforcement_success, empirically_contingent).
narrative_ontology:cs_reference_frame('de412552-19b4-4098-aa44-20b505f84dae', honor_as_defensible_public_standing).
narrative_ontology:cs_drift_state('de412552-19b4-4098-aa44-20b505f84dae', post_bourgeois_dignity_consolidation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('de412552-19b4-4098-aa44-20b505f84dae', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, professional_middle_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, bourgeois_civil_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, former_dueling_class_aristocracy_officer_corps).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, dignity_culture_ascendancy_thesis).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, honor_code_obsolescence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As commercial and professional life expanded, this class's status derived from credentials, contracts, and legal standing rather than personal honor defended by arms. The collapse of the honor-code substrate freed them from a status economy that had never been built to serve them and let them consolidate status through institutions they already controlled. They do not administer the collapse; it simply removes a rival status logic that would otherwise have constrained or endangered them.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, professional_middle_class, beneficiary,
    organized, generational, mobile, national).

% Courts, insurance regimes, professional associations, and civil-defamation law absorbed the reputational-adjudication function that dueling had once performed extra-legally. As the honor code lost interpretive force, these institutions did not need to fight it; they simply became the only remaining venue capable of adjudicating reputational injury, and their jurisdiction expanded by default.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, bourgeois_civil_institutions, beneficiary,
    institutional, civilizational, analytical, national).

% For the aristocratic and military-officer strata whose entire status vocabulary was honor-satisfaction, the collapse of the code was not a rule change but a loss of the ground beneath their feet. They could not simply choose to keep dueling once the shared cultural grammar that made a duel intelligible as satisfaction — rather than as assault, eccentricity, or crime — dissolved around them. Their exit option was not to leave the constraint but to watch the interpretive world that gave their actions meaning disappear.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, former_dueling_class_aristocracy_officer_corps, payer,
    powerful, generational, trapped, national).

% Clergy, novelists, physicians, and moral reformers articulated and popularized the new vocabulary of inherent, equal, non-forfeitable dignity that displaced honor as the operative status metaphysics. They did not enforce the transformation with sanctions; they supplied the replacement interpretive substrate that made honor-satisfaction increasingly unintelligible as a serious adult practice rather than illegal barbarism or self-parody.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, dignity_culture_intelligentsia, agenda_setter,
    institutional, civilizational, analytical, national).

% Scholars (Pitt-Rivers, Nisbett and Cohen, Wyatt-Brown, Appiah) study the transition as a case of moral revolution driven by shifts in shared interpretive frameworks rather than by external suppression alone. Their analytical position lets them see the substrate-collapse mechanism as distinct from, though entangled with, legal prohibition.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, historians_and_sociologists_of_honor, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the honor code, dueling coordinated reputational disputes among status-equals by providing a shared, legible ritual for resolving affronts to standing — a genuine (if violent) coordination mechanism that avoided open-ended, unbounded retaliation cycles by substituting a bounded, rule-governed encounter.
% TRANSFER_FUNCTION: In this reading nothing is transferred by an enforcing party — the substrate that made the transfer intelligible simply erodes. What had moved (risk of death or injury, in exchange for restored standing) becomes unavailable not because someone stops it but because the shared meaning system that made it a coherent exchange dissolves.
% ABSENT_VOICES: The formerly dueling classes are not silenced by an opponent; there is no opponent in this reading. Their voice becomes structurally unintelligible rather than suppressed — they can still speak the old vocabulary, but the audience capable of receiving it as serious has disappeared. This is a different absence than exclusion: it is audience collapse, not gag.
% DISAPPEARANCE_RATIONALE: In the cultural-contraction reading, the honor-satisfaction substrate did not disappear because some agent removed it — it eroded the way a shared social fact erodes when the population that sustains it stops treating it as real. Asking what would happen 'if it disappeared overnight' is nearly a category error for this reading: the substrate's disappearance WAS gradual and already happened, driven by generational turnover in interpretive frameworks, and no single administrator could have reversed or accelerated it at will. The mountain-erosion framing implies the world already rearranged itself slowly, not that removing an enforcer would rearrange it now.
% FOUNDING_PROBLEM: The honor code emerged to provide status-equals a bounded, legible mechanism for resolving affronts without either submitting to public humiliation or triggering unbounded blood-feud escalation — a genuine coordination problem in societies lacking strong centralized adjudication of reputational injury.
% FOUNDING_PROBLEM_CORROBORATION: Historians of honor culture (Pitt-Rivers, Wyatt-Brown, Appiah's 'The Honor Code') and comparative sociologists (Nisbett and Cohen on herding-economy honor cultures) attest from outside any beneficiary group that centralized state adjudication, insurance, and civil law had already substantially displaced the coordination function well before dueling became socially unthinkable — meaning the practice's disappearance tracked substrate collapse rather than the founding problem remaining unsolved. No dueling-class descendant group corroborates persistence of the founding problem; the corroboration is uniformly from analytical observers outside the formerly dueling class.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction and suppression are both authored low (0.12, 0.08) because this reading denies that dueling's end was accomplished through coercive extraction of compliance from a resisting population — nobody profits from enforcing dignity culture, and no active suppression apparatus was required once the honor-code substrate no longer supplied the interpretive resources that made dueling intelligible as satisfaction rather than crime. Accessibility_collapse is very high (0.88) because once the cultural substrate shifted, the alternative of dueling became not merely illegal but unthinkable — the collapse of alternatives is total and mind-independent in the way mountain-type constraints characteristically show, even though the underlying phenomenon (a social practice) is obviously human-constructed rather than physical law. Resistance is authored low (0.1) reflecting that by the terminal period there was little active defense of dueling left to overcome — the substrate had already eroded past the point where defenders could mount resistance in the old vocabulary.
 *
 * PERSPECTIVAL GAP:
 *   The former dueling class experiences the transition as loss of an entire meaning-system, not as being defeated by a rule; the professional middle class and civil institutions experience it as background drift that happened to redound to their advantage. The engine should compute genuinely different seat classifications here — this is not extraction disagreement but a disagreement about whether anything was done to anyone at all, which is the substantive content of choosing the contraction reading over the practice-decline reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (professional middle class, bourgeois civil institutions) are structural beneficiaries not because they administered the honor code's collapse but because the collapse removed a rival, exclusionary status economy that had never served them and handed status-adjudication authority to institutions they already controlled — this yields low derived d. The former dueling class is the payer group: their exit options are 'trapped' not because anyone locks them in, but because there is no available exit from a substrate collapse — you cannot choose to keep believing in a shared cultural grammar once your society at large no longer shares it. This is the erosion-not-suppression signature the reading is built to capture.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the honor code's founding problem (bounded resolution of reputational disputes among status-equals absent strong centralized adjudication) as dead by the interval's end, corroborated by observers outside the beneficiary set. Framing this as mountain-erosion rather than as an ongoing mandatrophy-in-need-of-declaration is deliberate: there is no institution here still claiming a live mandate over a dead function — the honor code did not persist as zombie machinery collecting rents on a spent justification (that would be the composite or practice-decline reading's territory, where legal-suppression apparatus might outlive its stated purpose). In the pure contraction reading, the substrate simply stopped being there, which is why no victim group and no active enforcement are declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_suppression_indistinguishability,
    'Can historical evidence actually distinguish substrate collapse (the honor code became unintelligible) from mere suppression (the honor code persisted but dueling was successfully deterred by law and institutions) given that both predict the same observable outcome — dueling''s disappearance?',
    'Look for evidence of counterfactual behavior under reduced enforcement: did dueling re-emerge in jurisdictions or periods where legal enforcement lapsed but general culture had already shifted, versus jurisdictions where enforcement lapsed but honor-culture attitudes persisted (e.g., certain American Southern subcultures per Nisbett and Cohen)? Persistence under lapsed enforcement supports the practice_decline_reading; non-recurrence even under lapsed enforcement supports this contraction_reading.',
    'If evidence favors the practice_decline pattern (recurrence when enforcement lapses), this reading''s mountain-erosion classification would be undermined — the substrate would not have collapsed, only been suppressed, reclassifying the constraint as closer to a tangled_rope or snare with active enforcement rather than mountain erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_suppression_indistinguishability, empirical, 'Whether the mountain-erosion signature is empirically distinguishable from persistent-norm-plus-suppression.').

omega_variable(
    which_reading_is_the_default_historiography,
    'Is the cultural-contraction reading (dignity culture displacing honor culture) the dominant historiographical consensus, or is it itself a retrospective narrative constructed by the beneficiary classes (bourgeois professionals, civil institutions) to naturalize what was actually accomplished through legal coercion and class-interest suppression?',
    'Comparative historiography: examine whether contemporaneous (19th-century) sources from the dueling class themselves describe the change as coercive suppression (supporting practice_decline) or as a genuine loss of shared meaning (supporting contraction) — self-description from the payer seat is more probative than retrospective beneficiary-class narrative.',
    'If 19th-century dueling-class sources predominantly describe coercive suppression, the FSM-adjacent worry applies: framing this as natural mountain-erosion could itself function as a legitimating myth for what was actually institutional victory by rising professional classes over aristocratic status monopolies — precisely the ambiguity a mountain-with-beneficiaries declaration is meant to flag.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_default_historiography, conceptual, 'Whether the contraction narrative is itself potentially a beneficiary-serving retrospective construction — the natural-law-vs-constructed ambiguity required for FSM-eligible mountains with declared beneficiaries.').

omega_variable(
    boundary_of_the_kernel_across_national_cultures,
    'Does the honor-to-dignity transition happen at a uniform pace and through a uniform mechanism across the different national contexts (French, German, American Southern, British) where dueling was practiced, or does the contraction reading hold in some contexts (e.g., Britain, where dueling collapsed rapidly and near-completely by mid-19th century) while the practice_decline reading holds better in others (e.g., the American South, where honor-culture attitudes persisted for generations after dueling itself was suppressed, per Nisbett and Cohen''s culture-of-honor research)?',
    'Cross-national comparative study using the same kernel with region-specific reading assignment; treat divergent national patterns as evidence that the correct reading is itself context-dependent rather than universal.',
    'If different readings hold in different national contexts, this single constraint''s scope should be narrowed to the contexts where contraction genuinely fits (likely Britain and possibly urban Continental Europe), with a separate story authored for contexts like the American South where practice_decline or composite readings fit better.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_of_the_kernel_across_national_cultures, empirical, 'Whether the contraction reading''s scope should be geographically bounded rather than treated as a general account of dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1750, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(hono_tr_t1790, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1790, 0.06).
narrative_ontology:measurement(hono_tr_t1830, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1830, 0.1).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1860, 0.2).
narrative_ontology:measurement(hono_tr_t1890, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1890, 0.35).
narrative_ontology:measurement(hono_tr_t1910, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1910, 0.5).
narrative_ontology:measurement(hono_tr_t1930, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1930, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hono_be_t1790, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1790, 0.11).
narrative_ontology:measurement(hono_be_t1830, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1830, 0.12).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1860, 0.13).
narrative_ontology:measurement(hono_be_t1890, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1890, 0.12).
narrative_ontology:measurement(hono_be_t1910, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1910, 0.12).
narrative_ontology:measurement(hono_be_t1930, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1930, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_satisfaction_substrate__cultural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_satisfaction_substrate kernel. cultural_contraction_reading (this file) authors near-mountain metrics (low extraction/suppression, very high accessibility_collapse) reflecting the claim that dueling's disappearance is substrate erosion, not suppression. practice_decline_reading authors the opposite metric profile (high suppression, lower accessibility_collapse, active enforcement) for the claim that the honor code persisted and was suppressed exogenously. composite_overdetermined_reading sits between the two, authoring moderate values on both suppression and accessibility_collapse to reflect non-independent, overdetermined causal pathways. All three share the same historical referent (the decline of dueling, c. 1750-1930) but instantiate structurally distinct constraints per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
