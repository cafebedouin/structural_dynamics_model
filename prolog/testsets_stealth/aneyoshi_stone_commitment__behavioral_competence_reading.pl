% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Siting Directive (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   Aneyoshi is a hamlet of roughly thirty households in a steep rias valley
 *   of the Sanriku coast, Miyako City, Iwate Prefecture. After the 1896 Meiji
 *   Sanriku tsunami devastated the hamlet and the 1933 Shōwa Sanriku tsunami
 *   struck again, survivors erected stone stelae above the wave's reach,
 *   inscribed with directives: remember the calamity of the great tsunamis;
 *   do not build any homes below this point; high dwellings are the peace and
 *   harmony of our descendants. The hamlet rebuilt above the line and kept
 *   every dwelling there for seventy-eight years. On 11 March 2011 the Tōhoku
 *   tsunami ran up the valley to just below the village; every resident in
 *   the hamlet survived, and the four children who died had descended to the
 *   shore. This file instantiates the behavioral_competence_reading of the
 *   aneyoshi_stone_commitment kernel: the stone as a live land-use rule that
 *   retained operational force in building-location decisions across the
 *   whole interval. The ε referent is the standing arrangement under contest
 *   — the siting-restriction regime maintained by the stones and their
 *   transmission practice — assessed by this reading's own lights, as a
 *   functioning intergenerational regulation. Claimed type and metrics are
 *   authored independently. KEY AGENTS (by structural relationship): -
 *   aneyoshi_households: governed collective and principal beneficiary
 *   (organized/constrained) — pays the siting opportunity cost, collects the
 *   survival payoff - village_memory_keepers: agenda-setting administrator of
 *   transmission (moderate/identity_locked) - village_descendant_generations:
 *   intended cross-time beneficiary (powerless/mobile) -
 *   coastal_tourism_developers: excluded seat denied shorefront optionality
 *   (moderate/arbitrage) - municipal_hazard_authorities: analytical observer
 *   corroborating the line (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Tsunami Stone Siting Directive (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'd7929537-cc47-46a1-a051-180ad3bd3981').
narrative_ontology:cs_kernel_codification('d7929537-cc47-46a1-a051-180ad3bd3981', fixed_text).
narrative_ontology:cs_authority_grounding('d7929537-cc47-46a1-a051-180ad3bd3981', lineage).
narrative_ontology:cs_interpretation_layer_present('d7929537-cc47-46a1-a051-180ad3bd3981').
narrative_ontology:cs_reading_relation('d7929537-cc47-46a1-a051-180ad3bd3981', aneyoshi_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('d7929537-cc47-46a1-a051-180ad3bd3981', foundational, inscribed_warning_retains_behavioral_force).
narrative_ontology:cs_axiom_status(inscribed_warning_retains_behavioral_force, holdable).
narrative_ontology:cs_axiom_grounding('d7929537-cc47-46a1-a051-180ad3bd3981', inscribed_warning_retains_behavioral_force, empirically_contingent).
narrative_ontology:cs_axiom('d7929537-cc47-46a1-a051-180ad3bd3981', secondary, disaster_memory_transmission_preserves_compliance).
narrative_ontology:cs_axiom_status(disaster_memory_transmission_preserves_compliance, holdable).
narrative_ontology:cs_axiom_grounding('d7929537-cc47-46a1-a051-180ad3bd3981', disaster_memory_transmission_preserves_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('d7929537-cc47-46a1-a051-180ad3bd3981', binding_land_use_directive).
narrative_ontology:cs_drift_state('d7929537-cc47-46a1-a051-180ad3bd3981', post_2011_validation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d7929537-cc47-46a1-a051-180ad3bd3981', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_households).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, village_descendant_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_households).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, historical_runup_bounds_future_inundation).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_hazard_memory_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Roughly thirty households farming, fishing, and working woodland in a steep rias valley on the Sanriku coast. After the 1933 tsunami they rebuilt every dwelling above the elevation marked by the stones and have kept it there since. Each generation of builders pays the opportunity cost of forgoing shorefront convenience and easy access to boats and gear; the same households collect the survival payoff. Leaving the hamlet means abandoning livelihood, kin networks, and family graves, so exit is costly even though some out-migration has occurred with aging.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_households, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_households, payer).

% Elders and household-association heads who tend the stones, retell the 1896 and 1933 calamities, walk children past the markers, and repeat the inscription's charge that high dwellings are the peace and harmony of descendants. They administer the arrangement by keeping its meaning legible rather than by policing anyone. Stepping out of the keeper role would mean breaking a duty they understand as owed to the dead and the unborn; the role is bound up with standing in the village, and no one occupies it neutrally.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, village_memory_keepers, agenda_setter,
    moderate, generational, identity_locked, local).

% The people the inscription addresses directly: children born into a siting regime decided before they could speak, and the not-yet-born across successive generations. They inherit the safe-side placement of every dwelling and the preserved hazard knowledge. As adults they may move away, but they arrive without having chosen the arrangement that protects them.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, village_descendant_generations, beneficiary,
    powerless, generational, mobile, local).

% Regional development interests of the Shōwa and Heisei decades who looked at the Sanriku coastline for resorts, marinas, and second homes. Hamlet commons and the siting norm kept Aneyoshi's shorefront fringe off the market, and the developers never gained a deliberative seat in village affairs. Capital simply moved to stretches of coast without such commitments, so their loss is optionality, not assets.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, coastal_tourism_developers, excluded,
    moderate, biographical, arbitrage, regional).

% Miyako City and Iwate Prefecture officials who survey and catalog the stones, publish inundation maps that independently mark the same elevations, and after 2011 cite the hamlet in hazard-education programs. They take no part in village siting decisions; their records and post-tsunami surveys corroborate where the water stopped.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, municipal_hazard_authorities, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_households).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a rare-event hazard datum — the maximum historical runup elevation — as a durable public standard, and aligns every household's siting choice on the safe side of it. It solves the twin problems that individual builders discount low-probability catastrophe and that hazard knowledge dies with its witnesses.
% TRANSFER_FUNCTION: Moves almost nothing material. It reallocates building sites upslope: each generation of builders pays the opportunity cost of forgone shorefront convenience and receives reduced mortality risk for itself and its successors. Across generations it transfers hazard knowledge from the dead to the unborn.
% ABSENT_VOICES: Households in neighboring Sanriku hamlets that received comparable warnings and rebuilt below their stones are absent because they died in 2011 — their absence is the counterfactual this reading rests on. Closer to home, any villager who would have preferred a shorefront dwelling, and the regional developers whose plans the commons foreclosed, were never given a deliberative seat: the rule was set by the founding generation and inherited, not negotiated.
% DISAPPEARANCE_RATIONALE: Without the stone and the norm it anchors, post-1933 reconstruction drifts downslope as economic pressure grows and living memory fades — the pattern every non-complying Sanriku hamlet followed. The 2011 wave would have found dwellings where it found graves elsewhere. Building locations, inheritance expectations, and the hamlet's continuity all depend on the line holding.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami devastated the hamlet and the 1933 Shōwa Sanriku tsunami struck again, the founding problem was how to keep the knowledge of maximum wave reach alive and behaviorally effective after the witnesses died — how to make a rare-event memory outlast human memory.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated entirely outside the benefiting parties: Meiji-era national tsunami runup surveys, sediment-core studies confirming the 1896 and 1933 inundation extents, Miyako City and prefectural hazard maps drawn independently of the village that mark the same elevations, and the 2011 joint survey teams' runup data. The village's own testimony concerns only its compliance; the hazard and its magnitude are attested by state archives, geology, and post-event measurement.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very low (0.12 at interval end) because the burden the rule imposes is a real but small opportunity cost — forgone shorefront siting — dwarfed by the survival payoff it delivers to the same people who pay it. Suppression is minimal (0.06): there is no enforcement machinery, no sanction schedule, no barred alternative; compliance is sustained by salience, communal norm, and repeated validation (the 1960 Chile tsunami, which again flooded the coast below the hamlet, refreshed the rule's credibility mid-interval). Theater ratio is low and gently rising (0.10 to 0.20): memorial ceremony around the stones coexists with function throughout, but as the last 1933 witnesses died, a growing share of stone-related activity became purely commemorative even while siting behavior stayed on the safe side — the rise traces memory decay, not function loss, which is precisely what separates this reading from the sibling husk reading. Accessibility collapse is low (0.25): building below the line remained physically open and legally possible the entire interval; what closed was its perceived viability once the stone's meaning was understood — the partial alternative-collapse typical of a working rope. Resistance is minimal (0.08): occasional grumbling about inconvenience, no organized defiance. The temporal series run on one shared seven-point grid (t=0,13,27,39,52,65,78 mapping 1933 to 2011) so every tracked metric is authored at every examined time point; a suppression_requirement series is deliberately omitted because the enforcement picture is static — there was never an enforcement apparatus to build up or decay — and the base_properties.suppression scalar carries that fact.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the household seat the arrangement is a mild self-imposed restriction that returned everything paid and more in 2011 — near-pure benefit. From the memory-keeper seat it is a constitutive duty whose 'cost' is inseparable from identity. From the excluded developer seat the same stones are a denial of optionality — a target-side experience of a rule the developers never agreed to, damped by arbitrage exit along the rest of the coast. From the descendant seat it is an unearned subsidy arriving from the dead. The engine computes these divergent per-seat classifications from the structural data; the authored rope claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries are aneyoshi_households and village_descendant_generations, so the derivation places both near the beneficiary end (d near 0.0): the constraint subsidizes them. No victims are declared because none exist — the rule extracts no identifiable group's resources for another's benefit; the closest thing to a target seat is the excluded developer seat, which bears denial of access but sits outside the enforcement perimeter with arbitrage-grade exit, damping its effective extraction. The memory keepers administer the arrangement and share its benefit, placing them low-d as well. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the local spatial scope (small scope, minimal amplification).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite errors. Against the sibling husk reading: calling the stone a mere memorial erases its coordination function and misprices ε as vacuously zero when the behavioral record shows seventy-eight years of binding force. Against romanticization: the stone is not a mountain — it is a constructed commitment that requires continuous transmission labor by identifiable keepers, and it would decay without them. The mandatrophy question is whether the founding problem still lives: it does, because Sanriku recurrence intervals exceed any institutional memory span, so the mandate regenerates with each generation. The R5 mismatch consumer finds founding_problem_status=live paired with disappearance_verdict=world_rearranges — function and persistence aligned, no zombie flag. Persistence here is by efficacy, not inertia: removal would be cheap, and the hamlet does not remove it because it works.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This file instantiates the behavioral_competence_reading of kernel aneyoshi_stone_commitment; the sibling commemorative_husk_reading asserts the same stones decayed to symbolic observance without behavioral constraint. Which reading does the 1933-2011 record support?',
    'Siting-record analysis: building permits, land-transaction registers, oral histories, and archival/aerial imagery establishing whether any dwelling was sited below the stone line between 1933 and 2011 by households with the economic means to build there; systematic contrast with Sanriku hamlets whose comparable stones were ignored.',
    'If the husk reading is correct, this constraint''s epsilon collapses toward zero vacuously (nothing binds) and the classification migrates toward inertial, theatrically maintained arrangement; if this reading is correct, the rope classification with genuine coordination function stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading of the aneyoshi stone kernel the behavioral record supports.').

omega_variable(
    survival_causal_attribution,
    'Is Aneyoshi''s 2011 survival causally attributable to compliance with the stone directive, or confounded by valley topography, poverty, and depopulation that would have kept dwellings high regardless?',
    'Matched-valley comparison: identify Iwate hamlets with comparable slope profiles and economic conditions that rebuilt shoreward after 1933 and measure differential 2011 mortality; sediment and runup reconstruction of the 1896, 1933, and 2011 events to confirm the line''s predictive content.',
    'If confounded, the stone''s regulatory force is overstated and epsilon drops further (the constraint was not binding); if causal, the vindicated propositions gain standing and the coordination function is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_causal_attribution, empirical, 'Whether 2011 survival is attributable to compliance or to confounding geography and economics.').

omega_variable(
    bindingness_marginal_effect,
    'Did the stone ever marginally change a siting decision — stop a household that would otherwise have built low — or did it merely codify what every household would have chosen after 1933 anyway?',
    'Oral-history interviews with families who considered shore lots; village land-transaction records; comparison of pre-1896 settlement patterns (which hugged the shore) with post-1933 patterns.',
    'A binding constraint supports the live-rule reading and a real coordination function; pure codification of existing preference would reduce the stone to an information standard with negligible behavioral content and move this story toward the sibling reading''s territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bindingness_marginal_effect, empirical, 'Whether the directive was binding at the margin or merely aligned with unanimous preference.').

omega_variable(
    directive_scope_ambiguity,
    'Does the directive bind permanent dwellings only, or all habitation and presence below the line? The four 2011 child deaths occurred below the line during transient shore access.',
    'Philological analysis of the inscriptions and of keeper retellings across generations; documentary evidence of what the 1933 founding generation treated the directive as covering.',
    'A dwellings-only scope keeps compliance complete and epsilon minimal; a broader scope reveals partial compliance, raises effective extraction slightly, and complicates the survival-causation claim by separating dwelling safety from full behavioral coverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(directive_scope_ambiguity, conceptual, 'Framing ambiguity in what the inscribed directive covers.').

omega_variable(
    transmission_durability_horizon,
    'Can the commitment retain operational force across another 78 years as the last witnesses of 1933 and 2011 die, or does transmission decay toward the sibling husk reading?',
    'Longitudinal monitoring of keeper succession, inclusion of the stones in school curricula, and migration-driven turnover in the hamlet''s resident households.',
    'Transmission failure would date a rope-to-piton drift (function atrophies, maintenance turns theatrical); successful renewal extends the live-rule reading indefinitely and keeps the vindicated propositions in force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_durability_horizon, empirical, 'Forward durability of the commitment''s behavioral force beyond living memory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_behavioral_tr_t0, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t0, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t13, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 13, 0.11).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t13, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t27, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 27, 0.12).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t27, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t39, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 39, 0.14).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t39, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t52, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 52, 0.16).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t52, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t65, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 65, 0.18).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t65, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t78, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 78, 0.2).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(aneyoshi_behavioral_be_t0, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.07).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t0, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t13, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 13, 0.08).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t13, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t27, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 27, 0.08).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t27, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t39, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 39, 0.09).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t39, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t52, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 52, 0.1).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t52, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t65, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 65, 0.11).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t65, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t78, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 78, 0.12).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t78, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the Aneyoshi tsunami stone' conflates two structurally distinct claims. This file authors the behavioral_competence_reading — the stone as a live land-use rule with retained operational force (epsilon approximately 0.12, rope, genuine coordination function, 2011 survival causally linked to compliance). The sibling file authors the commemorative_husk_reading — the stone as memorial artifact whose directive no longer constrains land use (near-zero binding force, maintenance largely ceremonial, piton-flavored profile). Same physical stones, different epsilon, different failure modes, different stakeholder surfaces. The behavioral reading is the upstream claim post-2011: its evidentiary success (water stopped below the compliant hamlet) is routinely cited against the husk reading, while the husk reading draws its force from the majority of Sanriku stones that were ignored. Both files link each other through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
