% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Protection with Proportional Harm Balancing
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the harm-balancing reading of the
 *   speech-harm-boundary kernel: a framework where speech protection is
 *   presumptive but yields when demonstrable harm is shown and proportional
 *   to the restriction. Judicial bodies adjudicate the balance on a
 *   case-by-case basis, determining whether speech crosses from protected
 *   expression into unprotected harm (hate speech, incitement, harassment,
 *   group defamation). Unlike the absolutist sibling reading (which treats
 *   harm override as nearly impossible) or the dignity sibling reading (which
 *   subordinates speech to dignity protection), this reading threads a middle
 *   path: protection for speakers, but accessible boundaries where targets
 *   can petition for restriction if harm is proven. The constraint is
 *   actively contested—all three readings coexist in contemporary
 *   jurisprudence across different jurisdictions and litigated routinely. The
 *   authored metrics reflect moderate extractiveness (0.48), suggesting the
 *   balancing framework captures some genuine coordination benefit but also
 *   contains asymmetric cost distribution that benefits speakers and burdens
 *   targets.
 *
 * KEY AGENTS:
 *   - speakers_asserting_protection: Benefit from the presumption; face restriction only when harm is adjudged to cross the threshold; can exit to less-regulated venues.
 *   - targets_of_harmful_speech: Bear costs until harm is demonstrated; must clear a high evidentiary bar; trapped until adjudication.
 *   - marginalized_groups_bearing_harassment_costs: Systemically targeted; identity-locked (cannot exit the identity being attacked); excluded from rule-setting; bear costs diffusely across many incidents.
 *   - judicial_adjudicators: Set and administer the harm threshold; shift the boundary through case law; accumulate power to define what counts as unprotected speech.
 *   - legislatures: Codify harm categories in statute; can tighten or loosen the boundary; can constrain or expand judicial discretion.
 *   - absolutist_defenders: Excluded from the consensus grounding this reading; reject harm-balancing as violating categorical speech protection.
 *   - dignity_advocates: Excluded from the consensus; argue the balance is set wrong and dignity should be categorical, not bargainable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.62).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Protection with Proportional Harm Balancing").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, 'a1037b46-43cb-4bcf-818e-31881a71d7a7').
narrative_ontology:cs_kernel_codification('a1037b46-43cb-4bcf-818e-31881a71d7a7', fixed_text).
narrative_ontology:cs_authority_grounding('a1037b46-43cb-4bcf-818e-31881a71d7a7', lineage).
narrative_ontology:cs_interpretation_layer_present('a1037b46-43cb-4bcf-818e-31881a71d7a7').
narrative_ontology:cs_reading_relation('a1037b46-43cb-4bcf-818e-31881a71d7a7', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1037b46-43cb-4bcf-818e-31881a71d7a7', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('a1037b46-43cb-4bcf-818e-31881a71d7a7', foundational, harm_override_requires_demonstration).
narrative_ontology:cs_axiom_status(harm_override_requires_demonstration, holdable).
narrative_ontology:cs_axiom_grounding('a1037b46-43cb-4bcf-818e-31881a71d7a7', harm_override_requires_demonstration, deontological).
narrative_ontology:cs_axiom('a1037b46-43cb-4bcf-818e-31881a71d7a7', foundational, proportionality_test_balances_values).
narrative_ontology:cs_axiom_status(proportionality_test_balances_values, holdable).
narrative_ontology:cs_axiom_grounding('a1037b46-43cb-4bcf-818e-31881a71d7a7', proportionality_test_balances_values, instrumental).
narrative_ontology:cs_reference_frame('a1037b46-43cb-4bcf-818e-31881a71d7a7', speech_protection_presumptive_harm_justifies_restriction).
narrative_ontology:cs_drift_state('a1037b46-43cb-4bcf-818e-31881a71d7a7', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1037b46-43cb-4bcf-818e-31881a71d7a7', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, speakers_with_protected_speech).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, public_discourse_participants).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, targets_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, marginalized_groups_bearing_harassment_costs).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the constraint contains genuine coordination (a shared adjudicatory framework beats bilateral contests) but asymmetric cost allocation (speakers get a presumption; targets must prove harm). The measurement series show extractiveness rising from 0.35 to 0.48 over the interval—a steady drift upward as more speech is litigated and the harm threshold experiences pressure from both absolutist and dignitarian directions. Theater rises from 0.25 to 0.41, suggesting enforcement increasingly performs harm-finding (public declarations that speech is harmful) rather than materially changing speech distribution. Suppression rises from 0.48 to 0.62, indicating that maintaining the constraint requires increasing active defense against both sibling readings. The plateau from t=32 onward suggests the constraint has reached a semi-stable state where its internal contradictions are managed theatrically rather than resolved. Accessibility collapse is moderate (0.58) because alternatives exist—speakers can go to other forums, targets can organize counter-speech, legislatures can change the boundary—but the judicial framework's legitimacy closes off pure exit. Resistance is high (0.72) because all three readings have organized constituencies that actively resist the others; the harm-balancing reading must defend against absolutist claims that it violates freedom and dignity claims that it is insufficiently protective.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's classification should diverge sharply by seat: from the speaker's seat (moderate power, mobile exit, presumption), it computes as protective coordination—a rope or legitimately balanced tangled rope. From the target's seat (powerless, identity-locked, burden of proof), it computes as extractive and suppressive—a snare where the coordination serves only speakers and the target's costs are the price of that coordination. From the judicial seat, it is an agenda-setting framework that distributes authority and allows discretionary boundary-shifting, making the constraint itself a mechanism for institutional power accumulation. This perspectival divergence is NOT a defect—it is the measurement the engine is designed to detect. The authored claim (tangled rope) reflects the framework's official self-description; the authored metrics reflect that it functions asymmetrically across seats. The engine should compute tangled rope from the beneficiary/victim data but show high per-seat divergence when it disaggregates.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers claiming protection are structurally beneficiaries (presumption applies to them; they use it; d near 0.2-0.3, full beneficiary end). Targets of harmful speech are structural victims (burden of proof falls on them; they must prove harm to get relief; d near 0.8-0.9, full target end). Marginalized groups are doubly victims: targeted identities (cannot exit) + procedurally burdened (few resources to litigate). Judicial adjudicators derive their power from the constraint—they are the mechanism through which it operates, giving them a dual role between agenda-setter and beneficiary. Legislatures have similar duality: they codify the harm categories, shaping who qualifies for protection, and can redistribute costs if political pressure rises. Absolutist and dignity-advocate seats are excluded, not positioned within the constraint—they reject its premise. The directionality overrides would be unnecessary if the structural data (beneficiary: speakers + public_discourse_participants; victims: targets + marginalized_groups) accurately captures the asymmetry. No overrides are warranted; the derived d values should map cleanly to the seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure extraction (snare) by virtue of its genuine coordination function: the harm-balancing framework does solve a real coordination problem that bilateral speech contests would not. It avoids pure coordination (rope) by virtue of asymmetric cost allocation and the burden of proof falling on targets. Tangled rope captures this: coordination (shared adjudicatory framework) + extraction (targets must prove harm while speakers enjoy presumption). The mandate is not dead (harmless speech continues to flow; harmful speech continues to be litigated), but the mandate is contested at the foundational level—all three readings claim to be the correct interpretation of the kernel. This contestation is not a mandate-death; it is active mandate-governance where the three readings struggle to define the constraint's scope. The theater_ratio plateau suggests that at interval end (t=40), the constraint is maintained partly by performances of harm-finding and judicial pronouncements on the boundary, not purely by organic compliance with the underlying principle. This is not yet piton degradation (the framework still does coordinate adjudication), but it is approaching the boundary where theaters ratio could indicate functional atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_definition_contest,
    'What counts as demonstrable harm sufficient to override speech protection—immediate incitement to violence, emotional distress, systemic group defamation, epistemic harm, or dignitary injury? Does the proportionality test admit empirical evidence of psychological harm or only legally codified injury categories?',
    'Case-law trajectory showing how courts define and expand/narrow the harm categories; comparative analysis of jurisdictions with explicit harm definitions; empirical studies of harm causation that courts do or do not admit as evidence.',
    'If harm is defined narrowly (imminent incitement only), extractiveness drops because targets must clear a high bar; extraction is then minimal. If harm includes systemic harassment, group libel, or dignitary injury, extractiveness rises and the constraint approaches the dignity reading. The threshold is not neutral—it distributes costs between speakers and targets. The oscillation in theater_ratio suggests enforcement is drifting toward performance of harm-finding rather than substance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_threshold_definition_contest, empirical, 'Contestation over what evidence and categories define demonstrable harm.').

omega_variable(
    presumption_asymmetry_extraction,
    'Does the presumption of protection for speakers, coupled with the burden of proof on targets, constitute an asymmetric extraction mechanism that benefits speakers and burdens targets, or is it a legitimate allocation of epistemic burden in adversarial adjudication?',
    'Empirical audit of outcomes: do speakers proportionally win cases where harm is asserted? Do targets with fewer resources fail to meet the evidentiary burden? Comparative study of symmetric vs. asymmetric burden regimes and their effects on discourse access.',
    'If the presumption systematically advantages speakers over targets (especially resource-poor targets), the constraint contains a structural extraction element that is authorized by its own logic. If the burden is neutrally applied and targets succeed proportionally to their case merits, the asymmetry is procedural, not extractive. High probability the asymmetry persists because harm is hard to demonstrate for dispersed targets; this is where the constraint''s extractiveness concentrates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presumption_asymmetry_extraction, empirical, 'Whether the protection presumption creates structural extraction against targets.').

omega_variable(
    identity_lock_vs_exit_for_targets,
    'For targets of group-directed speech (marginalized identities), is the exit option truly constrained, or do they have viable workarounds (platform switching, community migration, counter-speech)?',
    'Ethnographic study of harassment targets'' actual options and choices; measure the psychological, economic, and relational costs of each exit pathway; compare exit availability across identity groups.',
    'If exit is genuinely identity-locked (cannot exit the identity being targeted), the target is structurally trapped and suppression is high. If viable alternatives exist (platform exit, counter-speech communities, legal remedy), targets have more agency than the model suggests. The directionality would shift—targets might move from d=1.0 (fully targeted) toward d=0.7 (constrained but with leverage). This shapes both the type certification and the proportionality balance''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_exit_for_targets, empirical, 'Whether marginalized-group targets are identity-locked or have viable exit paths.').

omega_variable(
    kernel_reading_stability_question,
    'Does this harm-balancing reading constitute a stable, enduring alternative interpretation of the speech-protection kernel, or is it an unstable intermediate state subject to pressure from the absolutist and dignity readings?',
    'Historical institutional analysis of the reading''s doctrinal coherence; measurement of how often each reading is cited, adopted, and abandoned in case law; examination of whether courts deploy the harm-balancing framework consistently or instrumentally (as a tool to reach predetermined outcomes that could be justified under any reading).',
    'If the reading is stable and coherent, it deserves classification as a distinct constraint with its own persistence dynamics and should remain extant in the corpus. If pressure from sibling readings is forcing it toward degradation or compromise, the constraint may be approaching piton status (maintained theatrically while its foundational premise is eroded). The measurement plateau from t=32 onward could indicate either stability or the beginning of theatrical maintenance. The high theater_ratio (0.41) suggests performance is increasingly central to the constraint''s operation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability_question, conceptual, 'Structural stability of the harm-balancing reading as a distinct kernel interpretation.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.62) primarily structural—legal barriers, adjudicatory burden, procedural gatekeeping—or partly internalized, such that targets internalize the ''high burden of proof'' norm and self-suppress before adjudication?',
    'Post-harm-removal trajectory study: when legal protection becomes available for a speech category (e.g., legislative expansion of unprotected categories), do targets of that speech immediately cease self-suppression, or do they persist in muted participation? Compare voice and participation rates before and after remedy availability.',
    'If suppression is primarily structural, removing legal barriers (lowering the harm threshold) would directly increase target voice. If suppression is internalized, targets would require additional support (community rebuilding, confidence restoration, counterspeech resources) to re-engage. If suppression is mixed (structure + internalization), different target groups show different ratios—organized groups more structural-suppression-affected, isolated targets more internalized. This shapes both the extractiveness measure (if internalized, the actual extraction is higher than the structural barriers suggest) and the remedy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in the harm-balancing constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__harm_balancing_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__harm_balancing_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__harm_balancing_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__harm_balancing_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__harm_balancing_reading, 0.18).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% The speech_harm_boundary kernel is instantiated in three distinct constraint stories, each with a different reading of where the balance between protection and harm should lie. This story (harm_balancing_reading) represents the middle ground: protection presumptive but yielding to proportionally justified harm restrictions. The sibling stories (absolutist_reading and dignity_reading) represent the poles of the contest. These are not the same constraint viewed from different angles—they are materially different structural arrangements with different ε values, different beneficiary/victim configurations, and different classifications. They are linked here as members of the same constraint family because they instantiate the same kernel and contend with each other in legal and political discourse. The absolutist reading (constraint_id: speech_harm_boundary__absolutist_reading) has very low extractiveness and near-zero suppression of protected speakers; the dignity reading (constraint_id: speech_harm_boundary__dignity_reading) has higher extractiveness against speakers but lower extractiveness against targets. The harm-balancing reading balances moderately in between, which is why its metrics show mid-range extractiveness and why the stakeholder divergence is acute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
