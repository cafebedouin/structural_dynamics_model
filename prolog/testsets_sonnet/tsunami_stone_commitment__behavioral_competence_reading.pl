% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone as Live Behavioral Commitment (Competence Reading)
 *   domain: disaster_anthropology/institutional_memory
 *
 * SUMMARY:
 *   Centuries-old tsunami warning stones along parts of the Japanese coast
 *   (and structurally similar markers elsewhere) carry inscriptions
 *   instructing descendants not to build below a marked line and to run to
 *   high ground upon strong shaking. In villages where the associated oral
 *   tradition, festivals, and elder instruction remained active, this reading
 *   holds that the stone functioned as a live, low-cost coordination
 *   mechanism transmitting disaster-response competence across generational
 *   gaps — a piton-adjacent but still-functioning constraint, not a decayed
 *   one. The 2011 Tohoku tsunami is read here as an outcome trace of that
 *   still-live mechanism, not as the test itself.
 *
 * KEY AGENTS:
 *   - coastal_village_residents: beneficiary (moderate/constrained) — receive the behavioral competence the tradition transmits
 *   - descendant_households: beneficiary/agenda_setter (moderate/constrained) — administer transmission through ritual and social correction
 *   - village_elders: agenda_setter (moderate/constrained) — hold and narrate the oral tradition
 *   - local_government_disaster_offices: observer/beneficiary (institutional/analytical) — study and indirectly benefit from the mechanism
 *   - skeptical_planners: excluded (moderate/mobile) — favor formal infrastructure, outside the transmission loop
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone as Live Behavioral Commitment (Competence Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/institutional_memory").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '0de325d0-c180-44aa-9c3b-6e66b3edf550').
narrative_ontology:cs_kernel_codification('0de325d0-c180-44aa-9c3b-6e66b3edf550', fixed_text).
narrative_ontology:cs_authority_grounding('0de325d0-c180-44aa-9c3b-6e66b3edf550', lineage).
narrative_ontology:cs_interpretation_layer_present('0de325d0-c180-44aa-9c3b-6e66b3edf550').
narrative_ontology:cs_reading_relation('0de325d0-c180-44aa-9c3b-6e66b3edf550', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('0de325d0-c180-44aa-9c3b-6e66b3edf550', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('0de325d0-c180-44aa-9c3b-6e66b3edf550', foundational, transmission_chain_remained_causally_active).
narrative_ontology:cs_axiom_status(transmission_chain_remained_causally_active, holdable).
narrative_ontology:cs_axiom_grounding('0de325d0-c180-44aa-9c3b-6e66b3edf550', transmission_chain_remained_causally_active, empirically_contingent).
narrative_ontology:cs_axiom('0de325d0-c180-44aa-9c3b-6e66b3edf550', secondary, ritual_repetition_constitutes_genuine_coordination_not_theater).
narrative_ontology:cs_axiom_status(ritual_repetition_constitutes_genuine_coordination_not_theater, holdable).
narrative_ontology:cs_axiom_grounding('0de325d0-c180-44aa-9c3b-6e66b3edf550', ritual_repetition_constitutes_genuine_coordination_not_theater, empirically_contingent).
narrative_ontology:cs_reference_frame('0de325d0-c180-44aa-9c3b-6e66b3edf550', active_intergenerational_transmission_chain).
narrative_ontology:cs_drift_state('0de325d0-c180-44aa-9c3b-6e66b3edf550', post_2011_tsunami_observation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0de325d0-c180-44aa-9c3b-6e66b3edf550', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_village_residents).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, descendant_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, local_government_disaster_offices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live at or above the stone-marked line and are taught from childhood, through recurring ritual retelling, festival observance, and elder instruction, to build above the marker and to run uphill immediately upon strong shaking. In 2011 many households in stone-observant villages moved to high ground promptly and survived; this reading treats that behavior as the direct causal product of a still-functioning transmission chain, not coincidence.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_village_residents, beneficiary,
    moderate, biographical, constrained, local).

% Are the specific lineages charged with re-cutting inscriptions, leading annual commemorations, and correcting neighbors who build below the line. They administer the norm through direct social correction rather than formal law, and they receive no material rent from doing so — only the shared benefit of a community that still knows to run.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, descendant_households, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, descendant_households, agenda_setter).

% Hold the oral tradition attached to the stone and actively narrate it at shrine visits, school talks, and disaster drills. Their enforcement is social (reputational correction, storytelling repetition) rather than coercive; they cannot compel compliance by force, only by sustaining the transmission chain's credibility.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, village_elders, agenda_setter,
    moderate, generational, constrained, local).

% Study post-2011 survival differentials between stone-observant and non-observant settlements to calibrate evacuation policy and hazard-marker placement. They benefit from the stone's continued behavioral force as a low-cost, self-renewing complement to formal early-warning infrastructure, but do not administer the stone tradition themselves.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, local_government_disaster_offices, observer,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, local_government_disaster_offices, beneficiary).

% Argue that modern seawalls, sirens, and GPS-linked alerts should supersede folk markers and are rarely consulted by village elders on siting decisions near the old stones. Their preferred framing — that the stones are symbolic and formal infrastructure should lead — is not part of the village-level decision process this reading describes.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, skeptical_planners, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a spatially fixed, intergenerationally transmitted behavioral rule (build above this line, run uphill on strong shaking) that solves the genuine coordination problem of preserving disaster-response knowledge across generational gaps longer than living memory, without requiring continuous formal institutional maintenance.
% TRANSFER_FUNCTION: Transfers behavioral competence (not material resources) from elder generations to descendant generations via ritual repetition, storytelling, and social correction; the 'payment' is the ongoing labor of transmission (festivals, retelling, chastisement of violators), borne by the same community that benefits.
% ABSENT_VOICES: Modern disaster-planning professionals who favor engineered infrastructure over folk markers are not part of the village transmission process and are not consulted on stone-adjacent siting; they would argue for supersession by formal systems but are structurally outside this norm's operating loop.
% DISAPPEARANCE_RATIONALE: If the transmission chain broke — if elders stopped narrating, festivals lapsed, and descendant households stopped correcting encroachment below the line — construction would drift downslope within a generation or two, as it demonstrably did in villages where the tradition lapsed prior to 2011. The 2011 outcome differential is the observable trace of this rearrangement counterfactual.
% FOUNDING_PROBLEM: Coastal communities needed a way to preserve tsunami-response knowledge (where is safe, how fast must you move) across intervals far longer than any single lifetime, in the absence of continuous written record-keeping or centralized disaster bureaucracy.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 field surveys and disaster-anthropology research (conducted by academic researchers and NGO disaster-response teams outside the village lineages themselves) documented correlation between stone-marker observance and survival/evacuation-speed outcomes, corroborating that the transmission chain was behaviorally live at the time of the event rather than merely commemorative.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.06) because under this reading no party profits from the constraint at another's expense — the labor of transmission (storytelling, festivals, social correction) is borne by the same community that receives the safety benefit, a textbook coordination-cost rather than extraction. Suppression is low (0.12) because compliance is sustained by social credibility and habituated practice, not coercion; villagers who build below the line face reputational friction, not force. Theater ratio is kept low (0.08) precisely because this reading's defining claim is that the ritual apparatus (festivals, retelling) is NOT hollow performance — it is the mechanism itself, doing real transmission work, which is the structural point distinguishing this reading from the husk sibling. Accessibility collapse is moderate-high (0.62): once a household internalizes the rule, alternative behavioral models (build wherever convenient) are substantially foreclosed by social monitoring, though not completely, since land pressure and modernization still tempt encroachment. Resistance is near-zero (0.05) because there is no organized party resisting the norm from within the village — resistance, where it exists, comes from outside planners who are excluded from the loop entirely, not from constrained insiders paying a cost.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (elders, descendant households) the constraint reads as functioning coordination they actively steward. From the excluded planner seat, the same structure could easily be dismissed as unverifiable folklore — but that seat is not consulted on siting decisions and does not bear the tradition's transmission costs, so its skepticism does not translate into a payer relationship. There is no payer seat in this reading, which is itself the structural signature distinguishing the competence reading from a tangled or extractive account.
 *
 * DIRECTIONALITY LOGIC:
 *   Every named party under this reading is a beneficiary or a neutral administrator, not a target. Coastal residents and descendant households sit near the beneficiary end of directionality because the constraint subsidizes their survival odds at low ongoing cost; there is no victim group because the transmission cost is diffuse, voluntary, and falls on the same population that benefits — this is why victims[] is empty and why the tangled_rope gate does not apply here. Local government sits near symmetric-to-beneficiary: it gains a low-cost complement to formal systems without funding or administering the folk mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving disaster-response knowledge across generational gaps) is authored as still live, and the mismatch check (status=live paired with disappearance_verdict=world_rearranges) shows a coherent, non-capture pattern: the constraint's stated function and its structural necessity agree. This is the opposite signature from a zombie/mandatrophy case (status=dead + world_rearranges), which would flag capture. Under this reading, the mechanism has not outlived its function — it continues to do the work it was built for, which is precisely the structural claim that makes this a piton-adjacent but non-degraded case rather than a genuine piton or a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_liveness_vs_survivorship,
    'Did the stone-linked oral tradition causally produce the observed 2011 evacuation behavior, or did surviving villages simply happen to retain the tradition for reasons uncorrelated with its causal efficacy (e.g., more cohesive communities also happened to preserve folklore AND independently evacuate faster)?',
    'Controlled comparison of villages matched on geography, seismic exposure, and modern warning infrastructure access, varying only stone-tradition observance strength, with process-tracing interviews establishing whether residents cite the stone/elder narration as their proximate decision reason.',
    'If the tradition is shown causally sufficient (residents who evacuated cite the specific narrated rule), this reading is strongly corroborated as low-ε genuine coordination. If evacuation correlates with tradition observance only through a confound (general community cohesion), the commemorative_husk_reading becomes better supported and this story''s claimed_type should be revisited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_liveness_vs_survivorship, empirical, 'Whether the stone tradition was causally operative or merely correlated with survival.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the choice to treat this as the behavioral_competence_reading (rather than the commemorative_husk_reading) driven by genuine ethnographic evidence of active transmission, or by a retrospective halo effect where 2011 survival is read backward into the tradition''s vitality?',
    'Pre-2011 ethnographic records (if they exist) documenting festival attendance, elder-narration frequency, and construction-siting enforcement BEFORE the tsunami, independent of outcome knowledge, would settle whether liveness was established prospectively or only inferred after the fact.',
    'If pre-2011 documentation is thin, the selection of this reading over the husk reading rests partly on outcome-driven inference, which would weaken (though not eliminate) confidence in the very-low-ε classification authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether this reading was selected on prospective evidence or retrospective outcome-halo.').

omega_variable(
    modernization_pressure_on_transmission,
    'As villages depopulate and modernize, does the transmission chain (festival attendance, elder narration, youth uptake) remain robust, or is it visibly thinning in ways that would eventually convert this competence reading into the husk reading over time?',
    'Longitudinal tracking of festival attendance rates, youth participation in commemorations, and construction-siting compliance in the decades following 2011.',
    'A declining trend would suggest this story''s current low-ε classification is a snapshot of a constraint mid-transition toward the husk reading, not a stable steady state — relevant for future re-authoring rather than for this story''s present values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_pressure_on_transmission, empirical, 'Whether the live-transmission mechanism is durable or currently eroding toward symbolic status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsun_tr_t20, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(tsun_tr_t40, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(tsun_tr_t60, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.07).
narrative_ontology:measurement(tsun_tr_t80, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tsun_be_t20, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(tsun_be_t40, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 40, 0.06).
narrative_ontology:measurement(tsun_be_t60, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.06).
narrative_ontology:measurement(tsun_be_t80, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 80, 0.06).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.06).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(tsun_su_t20, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(tsun_su_t40, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 40, 0.11).
narrative_ontology:measurement(tsun_su_t60, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 60, 0.11).
narrative_ontology:measurement(tsun_su_t80, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 80, 0.12).
narrative_ontology:measurement(tsun_su_t100, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__behavioral_competence_reading, 0.08).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the tsunami_stone_commitment kernel. behavioral_competence_reading (this file) claims the stone's warning retained live causal force through active transmission (very low ε, near-rope/piton-adjacent, no victims). commemorative_husk_reading claims the marker decayed to symbolic status and 2011 compliance was coincidental (implies higher ε via unaccounted causal mechanisms and weaker coordination-function grounding). catastrophe_validation_axis treats the 2011 event itself as the decisive empirical test distinguishing the other two, rather than describing an ongoing behavioral mechanism — it is structurally about evidentiary status, not about the constraint's operation. All three are linked here per the network-decomposition rule; each carries its own ε and stakeholder structure and must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
