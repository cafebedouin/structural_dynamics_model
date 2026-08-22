% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Aneyoshi Tsunami Stone as Operative Land-Use Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/land_use_governance
 *
 * SUMMARY:
 *   In 1933, survivors of a catastrophic tsunami in the small Sanriku hamlet
 *   of Aneyoshi erected a stone marker inscribed with an explicit land-use
 *   directive: do not build homes below this point. Unlike many disaster
 *   memorials that function purely commemoratively, this reading holds that
 *   the marker's directive retained active regulatory force in real household
 *   building decisions across roughly three generations, reinforced by oral
 *   transmission from elders rather than any legal or governmental
 *   enforcement. The 2011 Tōhoku tsunami provides an unusually clean natural
 *   test: structures sited above the marker's line in Aneyoshi survived while
 *   comparable exposure elsewhere on the coast, absent an equivalently
 *   operative marker, produced severe losses. This reading treats that
 *   outcome as evidence of a functioning, low-extraction coordination
 *   mechanism — an informal, non-coercive commitment device that solved a
 *   genuine intergenerational information problem (rare, high-consequence
 *   hazards exceed the horizon of direct experiential learning) at very low
 *   ongoing cost to those who complied.
 *
 * KEY AGENTS:
 *   - aneyoshi_households: primary beneficiaries and primary compliers (powerless/constrained) — bear the modest cost of less convenient siting in exchange for hazard avoidance
 *   - local_elders_and_stone_stewards: informal agenda-setters (moderate/constrained) — maintain the norm through retelling and example, with no coercive apparatus
 *   - future_coastal_residents: downstream beneficiaries (powerless/constrained, civilizational horizon) — inherit both the physical siting pattern and the residual narrative
 *   - municipal_and_prefectural_planners: analytical observers (institutional/analytical) — study but did not administer the Aneyoshi arrangement itself
 *   - developers_and_land_users_downhill: excluded voice — a hypothetical preference for convenient low-ground siting that the record does not show organizing into a dissenting position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.18).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Tsunami Stone as Operative Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'b2c0544a-ee29-4ac1-bac0-573bbd03ccec').
narrative_ontology:cs_kernel_codification('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', fixed_text).
narrative_ontology:cs_authority_grounding('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', practice).
narrative_ontology:cs_interpretation_layer_present('b2c0544a-ee29-4ac1-bac0-573bbd03ccec').
narrative_ontology:cs_reading_relation('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', aneyoshi_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', foundational, inscribed_directive_retains_causal_force_across_generations).
narrative_ontology:cs_axiom_status(inscribed_directive_retains_causal_force_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', inscribed_directive_retains_causal_force_across_generations, empirically_contingent).
narrative_ontology:cs_axiom('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', secondary, oral_transmission_without_legal_enforcement_can_sustain_compliance).
narrative_ontology:cs_axiom_status(oral_transmission_without_legal_enforcement_can_sustain_compliance, holdable).
narrative_ontology:cs_axiom_grounding('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', oral_transmission_without_legal_enforcement_can_sustain_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', post_1933_founding_directive_as_operative_rule).
narrative_ontology:cs_drift_state('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', immediate_post_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b2c0544a-ee29-4ac1-bac0-573bbd03ccec', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_households).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_descendants).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, future_coastal_residents).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_hazard_memory_can_bind_land_use).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, non_legal_commitments_can_retain_operational_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households in the small hamlet of Aneyoshi built and rebuilt homes above the line marked by the 1933 tsunami stone, treating its inscription ('Do not build your homes below this point') as a live siting rule rather than a historical curiosity. In March 2011, the hamlet's structures above the marker survived the tsunami inundation that destroyed settlements below comparable markers elsewhere on the Sanriku coast. Exit from the rule would mean building on flatter, more convenient low ground nearer the harbor and road; households chose not to across at least three generations.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_households, beneficiary,
    powerless, generational, constrained, local).

% Children and grandchildren of the original stone-setters inherited the siting norm as household practice and local oral instruction, reinforced by elders' repeated retelling of 1896 and 1933 losses. They did not independently verify the hazard model; they inherited compliance as a default and, in most recorded cases, did not deviate from it even as land pressure and convenience favored lower sites.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_descendants, beneficiary,
    powerless, generational, constrained, local).

% Community elders maintained the stone, retold the narrative of past disasters at household and hamlet gatherings, and socially reinforced the norm of building above the line. They administered the constraint informally — no legal authority, no permitting power — through repetition, example, and local reputation, making continued observance a matter of community standing rather than statute.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, local_elders_and_stone_stewards, agenda_setter,
    moderate, generational, constrained, local).

% People not yet born into the hamlet, or moving in after 2011, inherit a physically instantiated siting pattern (existing structures already above the line) that channels future construction decisions even without active retelling, because the built environment itself now encodes the rule.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, future_coastal_residents, beneficiary,
    powerless, civilizational, constrained, local).

% Regional disaster-planning bodies studied Aneyoshi's 2011 outcome as a case for intergenerational hazard memory and, in some post-2011 reconstruction debates elsewhere in Tōhoku, cited it in arguments for statutory relocation-above-marker rules. They neither imposed nor administered the Aneyoshi stone's rule; they observe and sometimes cite it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, municipal_and_prefectural_planners, observer,
    institutional, generational, analytical, regional).

% Hypothetical or actual parties who might prefer to build on the flatter, more accessible low ground near the harbor road are effectively counseled out by community norm and family precedent rather than by any zoning ordinance; their preference for convenience is not formally recorded or debated in any planning process — they simply do not build there, or face informal social friction if they try.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, developers_and_land_users_downhill, excluded,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone and its attached narrative solve a genuine multi-generational information problem: hazard frequency for a given coastal site is low enough (once or twice per century) that direct experiential learning fails across a human lifespan, so a durable physical marker plus oral transmission substitutes for lived memory and coordinates household siting decisions around a shared, low-cost early-warning heuristic.
% TRANSFER_FUNCTION: The arrangement does not move resources between parties in the ordinary sense; it moves a costly, once-in-generations lesson (correct siting elevation) forward in time from the disaster survivors of 1896/1933 to descendants who did not experience the event, at the cost of somewhat less convenient home sites for those who comply.
% ABSENT_VOICES: Individual households who might have privately preferred lower, more convenient building sites are not recorded as a dissenting faction in the available oral-history and journalistic record; if such preferences existed, they are absent from the account because compliance was socially total enough that dissent was never institutionalized as a position to be argued against.
% DISAPPEARANCE_RATIONALE: If the stone and its associated household-level compliance disappeared without leaving physical structures already sited above the line, subsequent building decisions would very plausibly gravitate toward the more convenient low ground, restoring exposure comparable to neighboring settlements that lacked an operative marker and suffered severe 2011 losses. The 2011 outcome divergence between Aneyoshi and comparably exposed nearby settlements is the empirical signal that something real was doing work here, not merely commemorative.
% FOUNDING_PROBLEM: Recurrent tsunami inundation (recorded 1896 and 1933) destroyed the hamlet and killed most residents each time; survivors erected the stone specifically to prevent future generations from repeating the fatal siting error once direct memory of the disasters faded.
% FOUNDING_PROBLEM_CORROBORATION: The problem's continued liveness is corroborated by an actor outside the beneficiary group: the 2011 Tōhoku tsunami itself, an external physical event whose inundation line and the resulting differential survival of structures above versus below the marker constitutes independent, non-self-reported evidence that the hazard the stone warns against remains active and that compliance had measurable consequence.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored very low (0.06 at 2011) because under this reading no party captures a rent from others' compliance — the households bearing the modest inconvenience of higher, less convenient siting are the same households who receive the hazard-avoidance benefit; there is no separate collecting party. Suppression is modest (0.18) and social rather than coercive: informal community pressure and inherited default, not legal sanction, account for observed compliance, and even that pressure is a minor share of why people comply — most compliance under this reading is genuine assent to a credible warning, not enforced conformity. Theater ratio stays low and only creeps upward slightly (0.05 to 0.08) reflecting the ordinary drift by which any multi-generational institution accumulates some ceremonial retelling alongside its functional core, but the functional core (actual siting constraint) is authored as dominant throughout the interval, culminating in the 2011 differential-survival evidence that most directly corroborates the reading's causal claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi households, descendants, and future residents are declared beneficiaries because the constraint's entire operation, under this reading, redounds to their own hazard avoidance — there is no asymmetric extraction structure to derive a high-d target from. The elders/stewards sit as agenda-setters with moderate power but the same constrained exit as everyone else in the hamlet, since they too live under the siting norm they help transmit; their agenda-setting is stewardship of a shared good, not administration of an extractive apparatus. No victims are declared because this reading holds that the arrangement's function has not decayed into a cost imposed on some party for another's benefit; that possibility is precisely what the sibling commemorative_husk_reading and this reading's own omega variable exist to probe.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding_problem_status is authored as live, not dead, which forecloses a mandatrophy reading at this reading's own level: the 2011 event is treated as direct, external (non-self-reported) evidence that the hazard the stone addresses remains real and that the mechanism built to address it continues to do the job it was built for. This is the central structural claim that distinguishes this reading from commemorative_husk_reading, where the analogous founding-problem question would be answered dead-or-contested with corroboration only from inside the beneficiary community's own commemorative practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_siting_without_stone,
    'Would Aneyoshi households have sited their homes above the 1933 line anyway, absent the stone and its oral tradition, simply due to independently transmitted local hazard knowledge or terrain preference — making the stone a marker of a decision that would have been made regardless rather than a cause of it?',
    'Comparative case study against demographically and geographically similar Sanriku hamlets that lacked an equivalently prominent, explicitly instructional marker, controlling for terrain gradient, land value differentials, and population pressure, to isolate the marginal causal contribution of the stone and its retelling practice versus other transmission channels.',
    'If siting behavior would have converged on high ground regardless of the stone, the constraint''s effective extraction and coordination-function attribution should be revised toward the commemorative_husk_reading; if the comparative cases show markedly worse outcomes absent an equivalent marker, this reading''s causal claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_siting_without_stone, empirical, 'Whether the stone caused compliant siting or merely accompanied it.').

omega_variable(
    which_reading_is_the_true_kernel_state,
    'Is the Aneyoshi stone, across its full 78-year interval, better characterized as a continuously live behavioral rule (this reading) or as an arrangement that had already substantially decayed into symbolic observance by some point before 2011, with the 2011 survival outcome being a residual effect of already-built structures rather than of ongoing active compliance decisions?',
    'Oral-history interviews with multiple generations of Aneyoshi residents specifically probing whether building-siting decisions in the decades preceding 2011 were made with active reference to the stone''s directive versus simply inheriting an already-built pattern without renewed deliberation; supplemented by any available land-use or permitting records.',
    'If active deliberation had ceased well before 2011 and compliance was purely inertial (built environment already fixed), the constraint''s classification shifts toward piton (form persisting without live function) rather than rope; if deliberation remained active into recent decades, this reading''s rope classification and low epsilon are supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_true_kernel_state, conceptual, 'Whether the kernel''s operative reading holds continuously across the full interval or transitions partway through — the central disagreement this reading and commemorative_husk_reading are staking out at opposite poles.').

omega_variable(
    informal_suppression_composition,
    'Of the modest suppression authored (0.18), what proportion is genuine social pressure to conform (an internalized or community-enforced cost of deviation) versus simple absence of any competing incentive strong enough to motivate deviation in the first place?',
    'Interview-based reconstruction of any documented instances of a household considering or attempting to build below the line, and what social or practical friction, if any, they encountered.',
    'If no such instances exist in the historical record, the suppression figure may be overstated relative to a arrangement that was never actually tested against a countervailing preference — suggesting the low extraction figure rests on an assumption of consent that was never actually contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(informal_suppression_composition, empirical, 'Structural vs. simply-untested suppression in a community with no recorded dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1933, observed).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1950, observed).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1970, 0.06).
narrative_ontology:measurement_basis(aney_tr_t1970, observed).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement_basis(aney_tr_t1990, observed).
narrative_ontology:measurement(aney_tr_t2005, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2005, 0.08).
narrative_ontology:measurement_basis(aney_tr_t2005, observed).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.08).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.03).
narrative_ontology:measurement_basis(aney_be_t1933, observed).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1950, 0.04).
narrative_ontology:measurement_basis(aney_be_t1950, observed).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement_basis(aney_be_t1970, observed).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement_basis(aney_be_t1990, observed).
narrative_ontology:measurement(aney_be_t2005, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2005, 0.06).
narrative_ontology:measurement_basis(aney_be_t2005, observed).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.06).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__behavioral_competence_reading, 0.03).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint and aneyoshi_stone_commitment__commemorative_husk_reading are two readings of the same kernel (the Aneyoshi tsunami stone and its inscribed directive), not two constraints about different objects. This reading (behavioral_competence) authors extractiveness at 0.06 and claims rope, holding that the directive retained live operational force in siting decisions through 2011. The sibling reading authors the same physical artifact as having decayed into symbolic/commemorative status with no live behavioral constraint, which would produce a different — likely piton-leaning — classification with a different beneficiary/victim structure (or none at all). Per the ε-invariance principle, these are authored as separate stories because they would otherwise require assigning two incompatible ε values to what a casual observer might call 'the same constraint.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
