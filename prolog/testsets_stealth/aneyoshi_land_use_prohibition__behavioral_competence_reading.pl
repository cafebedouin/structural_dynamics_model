% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster anthropology / commitment systems / temporal institutional analysis
 *
 * SUMMARY:
 *   After the 1896 Meiji Sanriku tsunami, stone tablets were raised along the
 *   Sanriku ria coast marking the high-water line and commanding that no
 *   house be built below it. The hamlet of Aneyoshi rebuilt above its stone;
 *   the 1933 Showa Sanriku tsunami confirmed the line; and the practice was
 *   carried forward by ordinary transmission — elder instruction, memorial
 *   ceremony, siting arbitration — until the 2011 Tohoku tsunami stopped just
 *   below the stone and spared the hamlet. This file instantiates the
 *   behavioral_competence_reading of the aneyoshi_land_use_prohibition
 *   kernel: the stone as a live, behaviorally operative land-use rule across
 *   the 78-year interval (T=0 is 1933, when the operational regime
 *   consolidated; T=78 is 2011). Per the epsilon-invariance principle, the
 *   underlying tsunami physics is a separate mountain-class constraint
 *   (sanriku_tsunami_inundation_physics) that this social encoding points to
 *   but does not contain; the sibling commemorative_husk_reading is likewise
 *   a different constraint with its own profile, not a hedge folded into this
 *   one. The claim/metric gap discipline applies: claimed_type is authored
 *   from what I believe structurally true (pure coordination), and the
 *   metrics from what I believe descriptively true (very low extraction, low
 *   suppression, functionally load-bearing practice) — independently, without
 *   tuning either to a predicted engine verdict.
 *
 * KEY AGENTS:
 *   - aneyoshi_village_households: net beneficiary-payer (organized/constrained) — bears the rule's opportunity cost and receives its survival margin; the governed and the governors are the same small pool
 *   - aneyoshi_elders_and_memorial_keepers: agenda setter (organized/identity_locked) — administers transmission and arbitration; their communal office is fused with the practice
 *   - younger_fisher_households: bearer of the daily opportunity cost (moderate/mobile) — latent deviation pressure with a real outside option
 *   - neighboring_sanriku_villages: excluded counterexample holders (organized/mobile) — their lowland losses are the standing refutation absent from the hamlet's deliberations
 *   - iwate_prefectural_authorities: analytical observer (institutional/analytical) — post-2011 evaluator, enforces and collects nothing here
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.14).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.07).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.07).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster anthropology / commitment systems / temporal institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'efde67a4-34a6-419e-b7fe-aeb347e423dc').
narrative_ontology:cs_kernel_codification('efde67a4-34a6-419e-b7fe-aeb347e423dc', fixed_text).
narrative_ontology:cs_authority_grounding('efde67a4-34a6-419e-b7fe-aeb347e423dc', lineage).
narrative_ontology:cs_interpretation_layer_present('efde67a4-34a6-419e-b7fe-aeb347e423dc').
narrative_ontology:cs_reading_relation('efde67a4-34a6-419e-b7fe-aeb347e423dc', aneyoshi_land_use_prohibition__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('efde67a4-34a6-419e-b7fe-aeb347e423dc', foundational, stone_line_binding_on_siting_decisions).
narrative_ontology:cs_axiom_status(stone_line_binding_on_siting_decisions, holdable).
narrative_ontology:cs_axiom_grounding('efde67a4-34a6-419e-b7fe-aeb347e423dc', stone_line_binding_on_siting_decisions, empirically_contingent).
narrative_ontology:cs_axiom('efde67a4-34a6-419e-b7fe-aeb347e423dc', secondary, lineage_transmission_without_state_apparatus).
narrative_ontology:cs_axiom_status(lineage_transmission_without_state_apparatus, holdable).
narrative_ontology:cs_axiom_grounding('efde67a4-34a6-419e-b7fe-aeb347e423dc', lineage_transmission_without_state_apparatus, empirically_contingent).
narrative_ontology:cs_reference_frame('efde67a4-34a6-419e-b7fe-aeb347e423dc', binding_operative_boundary).
narrative_ontology:cs_drift_state('efde67a4-34a6-419e-b7fe-aeb347e423dc', post_tohoku_tsunami_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('efde67a4-34a6-419e-b7fe-aeb347e423dc', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_households).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__behavioral_competence_reading, younger_fisher_households).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, elevated_settlement_survival_principle).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, intergenerational_hazard_memory_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live above the inscribed line, farm and fish from the heights, and rebuild there after each tsunami. They give up easy shore access and flat lowland plots — the price of the rule falls on their own siting choices — and receive the survival margin the line preserves. Leaving the practice would mean abandoning ancestral ground and the hamlet's common life; conforming requires paying nothing to anyone.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_households, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_households, payer).

% Maintain the stone's meaning: instruct children in the line's origin, lead the periodic ceremonies that renew it, and arbitrate siting disputes by appeal to the inscription. Their standing in the hamlet rests on keepership of this knowledge; the practice is not something they administer from outside but something they are. Setting aside the keeper role would dissolve their communal office along with it.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_elders_and_memorial_keepers, agenda_setter,
    organized, generational, identity_locked, local).

% Work the boats and feel the climb from harbor to house daily; flat land near the water would save labor and room for gear and expansion. They have not organized to challenge the line — deference to elders and the 2011 outcome weigh heavily — but their working lives carry the rule's opportunity cost most concretely, and leaving for urban work is a real option their elders lack.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, younger_fisher_households, payer,
    moderate, immediate, mobile, local).

% Coastal communities along the same ria coastline, many with warning stones of their own and many that rebuilt on the flats after 1896 and 1933 for the sake of fishing convenience. Their losses in 2011 are the standing counterexample to lowland siting, yet they were never part of Aneyoshi's deliberations; their testimony would complicate the hamlet's tidy account of foresight rewarded.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, neighboring_sanriku_villages, excluded,
    organized, biographical, mobile, regional).

% Study the hamlet after 2011 as a case of vernacular hazard governance, commission surveys comparing the stone line against measured run-up, and weigh whether folk boundaries should be folded into formal zoning. They enforce nothing here and collect nothing; their interest is evidentiary and administrative.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, iwate_prefectural_authorities, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__behavioral_competence_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a memorized settlement boundary above the highest recorded tsunami run-up, solving the collective-action problem that rare catastrophes pose to short-horizon siting choices: each generation's individually reasonable creep toward the shore would cumulatively re-expose the hamlet to a hazard no living member had personally seen.
% TRANSFER_FUNCTION: Moves almost nothing between agents: each household surrenders lowland siting options from its own choice set, and the surrendered option value pools into a shared survival margin enjoyed by those same households. The only asymmetric flow is deference and standing to the keeper-elders.
% ABSENT_VOICES: Younger fishers who bear the daily cost of the climb have never formally contested the line; neighboring villages that lost everything to lowland rebuilding are absent from the hamlet's self-account; and the generations who died out during the long depopulation cannot say whether their conformity was conviction or resignation. The hamlet's unanimity partly reflects attrition — dissenters left or died — rather than settled persuasion.
% DISAPPEARANCE_RATIONALE: Without the operative line, siting decisions revert to short-horizon economics: gear storage, boat access, and flat ground pull construction downslope within a generation or two, and the next run-up finds the hamlet re-exposed. The village's survival configuration — houses clustered on the high ground above the stone — is an artifact of the rule; remove it and the arrangement unwinds gradually rather than instantly.
% FOUNDING_PROBLEM: The 1896 Meiji Sanriku tsunami, which killed thousands along the ria coast after a brief sea withdrawal drew people onto the exposed flats; survivors needed a device that would hold settlement above the observed run-up even for descendants who would never witness such a sea.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: paleotsunami geology (the 869 Jogan sand sheet and later deposits) establishes recurrent giant tsunamis on this coast independent of any tradition; instrumental seismology identifies the trench source; and the 2011 Tohoku event re-demonstrated the hazard at instrumented magnitude. None of these sources benefits from the stone's authority.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.05 because the only cost the rule imposes is the symmetric opportunity cost of highland siting, borne by the same households who receive the benefit; no seat collects from anyone else's compliance, hence gain_flow 'diffuse'. Suppression is 0.14: conformity rests on deference and elder arbitration, not apparatus — lowland building remained physically and legally open (neighboring villages exercised it), so alternatives were discouraged, not suppressed. Theater_ratio is 0.07 because the practice is load-bearing: the line was vindicated in 1933 and again in 2011, when the water stopped below the stone. Accessibility_collapse is 0.5 — once the hazard is understood, lowland building collapses as a safe option but remains a choosable one, which is exactly the rope band. Resistance is 0.15: grumbling and shore-access temptation, never organized challenge. All three series share one seven-point grid (T=0,13,26,39,52,65,78). The theater series shows a mild sawtooth: ritual share rises during long quiet stretches as ceremony drifts toward rote, then drops after each confirmation event (1960 Chile-source tsunami near T=26-39; 2011 at T=78) re-functionalizes the practice. The suppression_requirement series traces transmission burden, not coercion: active maintenance effort peaked mid-interval (T=39) as the last living memory of 1896/1933 lapsed and keeping the line required deliberate instruction rather than inherited certainty, then eased as the 1960 and 2011 confirmations re-anchored the norm. The oscillation is driven by exogenous hazard recurrence, not intermittent reinforcement — it is not itself an extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the household seat the arrangement is near-symmetric mild coordination (beneficiary and payer roles coincide). From the elder seat the same rule is constitutive office: an identity_locked administrator for whom questioning the line is not a policy position but a self-dissolution, so the constraint registers as nearly costless from inside. From the younger fisher seat it is a daily tax with a mobile exit the elders lack. From the neighboring-village seat it reads as a rebuke. From the prefectural seat it is evidence. One stone, five computed experiences; the engine derives this divergence from the structural data, and the authored rope claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declaration names the participant pool, not an extracting seat — households benefit collectively and symmetrically, which is why the manifest's 'no beneficiary structure' expectation and a declared beneficiary are compatible: there is no asymmetric beneficiary. The households' dual beneficiary/payer positioning pulls their derived directionality toward the symmetric middle rather than the subsidy end. The elders derive low directionality as administrators, but their identity_locked exit amplifies the personal stakes of the constraint's persistence without making them collectors of anyone's payment. No victim class exists anywhere in the structure — the signature that separates this rope from a tangled_rope. No directionality overrides are used: the derivation from declared roles and exit options captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (centennial-scale hazard recurrence, corroborated by paleotsunami geology and the 2011 event), and the disappearance verdict is world_rearranges — status-live plus world_rearranges produces no mismatch flag, correctly. Classifying this as rope guards against two mislabels: snare (there is no victim and no captured gain — the receipt surface is affirmatively diffuse) and piton (theater_ratio 0.07 and the 2011 demonstration show the function is intact, not performed). The sibling commemorative_husk_reading, if the behavioral record supported it, would generate precisely the piton-flavored profile — high theater, inertial persistence — so the family pair lets the corpus measure which description the record sustains rather than presuming either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition,
    'Which reading of the aneyoshi kernel does the behavioral record support: live rule (this file) or commemorative husk (sibling)?',
    'Settlement-record audit: plot dwelling sites against the stone line across 1933-2011; sustained above-line siting under real economic pressure confirms behavioral force.',
    'If siting never tracked the line under pressure, this reading fails and the husk reading''s piton-flavored profile becomes the correct description; epsilon barely moves (both readings are non-extractive) but type and theater_ratio diverge sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_partition, empirical, 'Partition of the kernel between the live-rule and husk readings.').

omega_variable(
    attrition_vs_compliance_confound,
    'Does the 78-year observance record demonstrate rule-following, or merely the absence of anyone left willing or able to build low?',
    'Compare siting behavior during population peaks (pre-1955) against troughs; interview diaspora households on why they did not take up lowland plots.',
    'If demographic attrition explains observance, the enforcement claim weakens without moving epsilon; the constraint drifts toward the husk profile even within this file''s own data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attrition_vs_compliance_confound, empirical, 'Whether conformity reflects the rule''s force or the hamlet''s depopulation.').

omega_variable(
    counterfactual_shoreward_pull,
    'How strong is the counterfactual pull toward lowland siting that the prohibition actually restrains?',
    'Cross-village comparison with Sanriku communities lacking effective stone transmission: measure how far settlement creeps downslope where the line is absent or ignored.',
    'Calibrates the constraint''s binding force; a weak counterfactual pull reduces the coordination achievement toward triviality (households would not have built low anyway), shifting weight toward the physics-mountain reading alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_shoreward_pull, empirical, 'Size of the shoreward temptation the stone holds back.').

omega_variable(
    formal_zoning_substitution,
    'Does post-2011 formal hazard zoning substitute for the stone''s function, ending the vernacular rule''s independent behavioral force?',
    'Track whether siting decisions after the zoning maps cite the stone or the map, and whether the hamlet maintains the transmission ceremonies once a state boundary exists.',
    'If substitution is complete, this reading''s reference frame survives only as heritage — the live-rule claim becomes historically true but presently inert, pushing the kernel''s present-day instantiation toward the sibling''s description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_zoning_substitution, conceptual, 'Whether formal zoning displaces the vernacular rule going forward.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_bcr_tr_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(aneyoshi_bcr_tr_t0, observed).
narrative_ontology:measurement(aneyoshi_bcr_tr_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 13, 0.09).
narrative_ontology:measurement_basis(aneyoshi_bcr_tr_t13, observed).
narrative_ontology:measurement(aneyoshi_bcr_tr_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 26, 0.12).
narrative_ontology:measurement_basis(aneyoshi_bcr_tr_t26, observed).
narrative_ontology:measurement(aneyoshi_bcr_tr_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 39, 0.09).
narrative_ontology:measurement_basis(aneyoshi_bcr_tr_t39, observed).
narrative_ontology:measurement(aneyoshi_bcr_tr_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 52, 0.11).
narrative_ontology:measurement_basis(aneyoshi_bcr_tr_t52, observed).
narrative_ontology:measurement(aneyoshi_bcr_tr_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 65, 0.13).
narrative_ontology:measurement_basis(aneyoshi_bcr_tr_t65, observed).
narrative_ontology:measurement(aneyoshi_bcr_tr_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 78, 0.07).
narrative_ontology:measurement_basis(aneyoshi_bcr_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(aneyoshi_bcr_be_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(aneyoshi_bcr_be_t0, observed).
narrative_ontology:measurement(aneyoshi_bcr_be_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 13, 0.08).
narrative_ontology:measurement_basis(aneyoshi_bcr_be_t13, observed).
narrative_ontology:measurement(aneyoshi_bcr_be_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 26, 0.07).
narrative_ontology:measurement_basis(aneyoshi_bcr_be_t26, observed).
narrative_ontology:measurement(aneyoshi_bcr_be_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 39, 0.07).
narrative_ontology:measurement_basis(aneyoshi_bcr_be_t39, observed).
narrative_ontology:measurement(aneyoshi_bcr_be_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 52, 0.06).
narrative_ontology:measurement_basis(aneyoshi_bcr_be_t52, observed).
narrative_ontology:measurement(aneyoshi_bcr_be_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 65, 0.06).
narrative_ontology:measurement_basis(aneyoshi_bcr_be_t65, observed).
narrative_ontology:measurement(aneyoshi_bcr_be_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 78, 0.05).
narrative_ontology:measurement_basis(aneyoshi_bcr_be_t78, observed).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_bcr_su_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(aneyoshi_bcr_su_t0, observed).
narrative_ontology:measurement(aneyoshi_bcr_su_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 13, 0.15).
narrative_ontology:measurement_basis(aneyoshi_bcr_su_t13, observed).
narrative_ontology:measurement(aneyoshi_bcr_su_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 26, 0.18).
narrative_ontology:measurement_basis(aneyoshi_bcr_su_t26, observed).
narrative_ontology:measurement(aneyoshi_bcr_su_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 39, 0.2).
narrative_ontology:measurement_basis(aneyoshi_bcr_su_t39, observed).
narrative_ontology:measurement(aneyoshi_bcr_su_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 52, 0.19).
narrative_ontology:measurement_basis(aneyoshi_bcr_su_t52, observed).
narrative_ontology:measurement(aneyoshi_bcr_su_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 65, 0.17).
narrative_ontology:measurement_basis(aneyoshi_bcr_su_t65, observed).
narrative_ontology:measurement(aneyoshi_bcr_su_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 78, 0.14).
narrative_ontology:measurement_basis(aneyoshi_bcr_su_t78, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, commemorative_husk_reading).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, sanriku_tsunami_inundation_physics).

% DUAL FORMULATION NOTE:
% Constraint family for the aneyoshi kernel. Upstream: sanriku_tsunami_inundation_physics, a genuine mountain (run-up follows topography; negligible extraction, no parties) whose operation the stone encodes. Downstream: two readings of the inscribed prohibition — this behavioral_competence_reading (live rule, rope, epsilon ~0.05, theater 0.07) and commemorative_husk_reading (decayed memorial, piton-flavored profile, high theater). The colloquial label 'the tsunami stone' conflates the two; their epsilons are similarly negligible but their types, theater ratios, and failure modes differ, so they are authored as separate stories linked here, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
