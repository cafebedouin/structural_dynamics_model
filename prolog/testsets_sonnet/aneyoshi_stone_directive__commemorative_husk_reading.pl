% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone as Commemorative Husk (Directive Decayed to Memorial)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone, inscribed after the 1933 Showa Sanriku
 *   disaster with an instruction not to build below its mark, is credited
 *   with guiding the small hamlet's survival in the 2011 Tohoku tsunami. This
 *   story takes the commemorative-husk reading: across the 78-year
 *   inter-catastrophe interval, the stone's behavioral force decayed even as
 *   its symbolic prestige rose. Land-use decisions came to be governed by
 *   ordinary municipal zoning and market pressure, not by the inscription,
 *   while the stone itself was increasingly folded into disaster-heritage
 *   tourism and post-2011 media narrative. The sibling reading
 *   (behavioral_competence_reading) holds that the directive remained a
 *   binding, if informal, land-use constraint the whole 78 years and that its
 *   2011 vindication proves continuous institutional competence. This story
 *   does not adjudicate between the two readings — it instantiates the husk
 *   reading as its own structurally distinct constraint, with its own epsilon
 *   and its own beneficiary/victim structure, per the ε-invariance principle.
 *   The two readings are linked only through the shared kernel_id and
 *   cs_structure.reading_relations, not through any shared metric.
 *
 * KEY AGENTS:
 *   - coastal_development_interests: primary beneficiary of decayed directive (organized/arbitrage) — gains buildable low-elevation land
 *   - tourism_and_heritage_operators: secondary beneficiary (moderate/mobile) — monetizes stone as static monument rather than live rule
 *   - future_coastal_residents: primary target (powerless/trapped) — inherits unmanaged catastrophic risk
 *   - descendants_of_1933_survivors: primary target/excluded (powerless/constrained) — watch ancestral intent lose governance force
 *   - municipal_land_use_authority: agenda-setter (institutional/constrained) — administers the ambiguity rather than resolving it
 *   - disaster_researchers: analytical observer (analytical/analytical) — documents the memory-decay pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.68).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.22).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Tsunami Stone as Commemorative Husk (Directive Decayed to Memorial)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, 'c171b476-7fb1-467f-8ec5-301c74a6f1b0').
narrative_ontology:cs_kernel_codification('c171b476-7fb1-467f-8ec5-301c74a6f1b0', fixed_text).
narrative_ontology:cs_authority_grounding('c171b476-7fb1-467f-8ec5-301c74a6f1b0', practice).
narrative_ontology:cs_interpretation_layer_present('c171b476-7fb1-467f-8ec5-301c74a6f1b0').
narrative_ontology:cs_reading_relation('c171b476-7fb1-467f-8ec5-301c74a6f1b0', aneyoshi_stone_directive__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('c171b476-7fb1-467f-8ec5-301c74a6f1b0', foundational, institutional_memory_requires_active_reinforcement).
narrative_ontology:cs_axiom_status(institutional_memory_requires_active_reinforcement, holdable).
narrative_ontology:cs_axiom_grounding('c171b476-7fb1-467f-8ec5-301c74a6f1b0', institutional_memory_requires_active_reinforcement, empirically_contingent).
narrative_ontology:cs_axiom('c171b476-7fb1-467f-8ec5-301c74a6f1b0', secondary, commemoration_and_governance_are_functionally_distinct).
narrative_ontology:cs_axiom_status(commemoration_and_governance_are_functionally_distinct, holdable).
narrative_ontology:cs_axiom_grounding('c171b476-7fb1-467f-8ec5-301c74a6f1b0', commemoration_and_governance_are_functionally_distinct, conventional).
narrative_ontology:cs_reference_frame('c171b476-7fb1-467f-8ec5-301c74a6f1b0', literal_survivor_instruction_binding_across_generations).
narrative_ontology:cs_drift_state('c171b476-7fb1-467f-8ec5-301c74a6f1b0', post_2011_heritage_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c171b476-7fb1-467f-8ec5-301c74a6f1b0', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, tourism_and_heritage_operators).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, future_coastal_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, descendants_of_1933_survivors).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, institutional_memory_decays_without_reinforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developers, municipal planners, and property owners who gain when the inscribed line ('do not build below this point') is treated as a historical curiosity rather than a live siting rule. They benefit from the stone's decay into a photographed monument because it frees valuable low-elevation coastal land for construction without the political cost of publicly repealing a directive credited with saving the village in 2011.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, beneficiary,
    organized, biographical, arbitrage, regional).

% Operators who monetize the stone as a heritage and disaster-tourism site — bus tours, plaques, media features. They have an interest in the stone remaining a commemorative object (a story to visit) rather than a binding land-use instrument that would constrain the very settlement patterns that make the visited village a going concern.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, tourism_and_heritage_operators, beneficiary,
    moderate, biographical, mobile, regional).

% People who will move into or build in the reclaimed low-elevation zones between now and the next major tsunami, without living memory of 1933 or 2011 and without a binding rule protecting them. They bear the tail-risk cost of the directive's loss of behavioral force but have no voice in present-day zoning decisions, since the stone's warning function has already lapsed into symbolism by the time they arrive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, future_coastal_residents, payer,
    powerless, generational, trapped, local).

% Local families whose ancestors carved and erected the stone as literal, binding instruction to future generations. They watch the inscription's authority erode across the inter-catastrophe decades (1933-2011 and again post-2011) as memory attenuates, land pressure rises, and no institutional mechanism re-activates the directive's force between disasters. Their intended meaning is preserved in granite but not in governance.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, descendants_of_1933_survivors, payer,
    powerless, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, descendants_of_1933_survivors, excluded).

% The body that could formally codify the stone's line into zoning law or could formally retire it as a folk marker, but does neither — it lets the ambiguity persist because either resolution has political cost: codification blocks development revenue, retirement looks like disrespecting disaster victims. It administers the status quo of a directive that governs nothing but survives as a landmark.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, municipal_land_use_authority, agenda_setter,
    institutional, biographical, constrained, local).

% Anthropologists and disaster-risk scholars who study the stone as a case of institutional memory decay across inter-catastrophe intervals. They document the gap between the artifact's inscribed intent and its lived governance function without holding power to change either.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None currently active at the behavioral level — the stone no longer coordinates settlement decisions in any binding sense. Its residual function is symbolic coordination: it lets the community perform continuity with disaster memory without paying the cost of enforcing the boundary it names.
% TRANSFER_FUNCTION: Moves long-tail catastrophic risk from present-day developers and land-use authorities (who avoid the political and economic cost of restrictive zoning) onto future residents and the descendants of the original survivors (who inherit an unenforced warning and, eventually, the exposure it was meant to prevent).
% ABSENT_VOICES: Future coastal residents who will occupy the low-elevation land have no seat in the present decision to let the marker lapse into commemoration; the 1933 survivors who carved the warning are dead and cannot correct the drift from directive to monument.
% DISAPPEARANCE_RATIONALE: If the stone were removed tomorrow, almost nothing in present land-use practice would change: no permitting process currently treats the inscription as binding, no enforcement body cites it, and development below the marked line already proceeds on ordinary zoning grounds. Its disappearance would only remove a photogenic waypoint on tourist routes and a rhetorical touchstone in post-2011 media narratives — the governance function it once might have anchored has already vacated the artifact.
% FOUNDING_PROBLEM: In 1933, survivors of a catastrophic tsunami erected the stone with an explicit, literal instruction — do not build homes below this point — intended to bind the behavior of descendants who would not personally remember the disaster.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster-anthropology researchers and post-2011 journalistic investigations (outside both the development interests and the heritage-tourism operators who benefit from the current arrangement) document that most nearby communities' warning markers were built over or ignored well before 2011, and that Aneyoshi's own survival was as much a matter of the marker's active local retelling by a small number of committed elders as of institutional enforcement — a mechanism that itself lapsed in comparable villages within one to two generations.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 at T=78) because, under this reading, the directive's decay is not neutral drift — it structurally transfers catastrophic-risk exposure from present development interests to future, voiceless residents, which is a real extraction even though no single actor coerces anyone. Suppression is authored low (0.22): nothing actively prevents descendants or researchers from re-asserting the directive's force; the decay is passive, not enforced. Theater ratio is authored very high and rising (0.10 to 0.81) because the stone's function increasingly shifts from land-use instrument to commemorative photo-op and disaster-tourism prop — the classic piton signature of performative maintenance replacing functional operation. Accessibility collapse is moderate (0.35): the original directive's alternative (formal zoning codification) remains available and cheap, it simply isn't exercised. Resistance is moderate (0.58): descendants and researchers do push back rhetorically, but without institutional standing to compel re-codification.
 *
 * PERSPECTIVAL GAP:
 *   From the municipal authority's agenda-setting seat, the arrangement looks like harmless heritage preservation with no live governance stakes. From the future-resident payer seat (a seat that cannot yet speak, since its members have not yet arrived), the same structure is a silent transfer of tail risk. The engine should register this asymmetry: the agenda_setter and beneficiary seats see low-stakes continuity; the payer seats, once populated, would see an unenforced catastrophe warning they never got to ratify or reject.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal development interests and tourism operators sit near the beneficiary end of directionality: they gain usable land and marketable heritage narrative respectively from the directive's non-enforcement. Future coastal residents and descendants of 1933 survivors sit near the target end: the former inherit undisclosed catastrophic exposure with no exit (trapped, since they will live wherever development has already occurred by the time they exist as stakeholders), the latter carry the symbolic and generational cost of watching a literal ancestral instruction go unenforced (constrained, since they retain some voice through heritage and advocacy channels but no legal standing to compel zoning change).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the stone's founding problem (protecting future generations from tsunami inundation via a literal, binding siting rule) as dead — the mechanism that would keep the problem live (institutional re-affirmation each generation) never developed, and the 2011 survival is read as a fortunate coincidence of local elder-driven storytelling rather than durable institutional competence. Classifying this as piton rather than mountain or rope prevents two mislabeling errors: treating the arrangement as natural/inevitable (which would hide the beneficiary structure gaining from its decay), and treating it as a functioning coordination mechanism (which would overstate the protection future residents actually have). The high theater_ratio and moderate resistance without any concentrated capturing beneficiary large enough to warrant 'snare' status is exactly the piton signature: administered by an authority that could re-codify it but bears no concentrated cost from not doing so, while diffuse future victims bear a cost too remote and unorganized to force the fix.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_kernel_ambiguity,
    'Did the Aneyoshi stone''s directive retain genuine behavioral force as an informal land-use constraint across the 1933-2011 interval (behavioral_competence_reading), or had it already decayed into a commemorative artifact whose 2011 ''success'' was incidental to actual settlement patterns (commemorative_husk_reading, this story)?',
    'Historical land-registry and settlement-pattern analysis for the specific parcels below the inscribed line across 1933-2011, cross-referenced against whether any building permits, sales, or informal settlement decisions in that zone were actually blocked or discouraged citing the stone, versus occurring anyway and simply not occurring due to unrelated factors (population decline, geography, economics).',
    'If land-registry data shows the below-line zone was in fact never developed BECAUSE of active local deference to the stone, the competence reading gains support and this husk reading''s high-epsilon claim would be overstated. If the zone was simply undesirable or depopulated for unrelated reasons, this reading''s claim that the directive lost behavioral force stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_kernel_ambiguity, empirical, 'Whether settlement non-development below the stone reflects active directive compliance or incidental non-development.').

omega_variable(
    post_2011_reactivation_ambiguity,
    'Did the 2011 tsunami and subsequent media attention re-activate the stone''s behavioral force going forward, or did it only intensify the commemorative/tourism reading while leaving actual post-2011 zoning practice unchanged?',
    'Comparison of municipal zoning ordinances and building permits issued in the marked zone before and after 2011; interviews with the land-use authority about whether the stone is cited in any post-2011 planning documents.',
    'If post-2011 zoning was formally amended to codify the stone''s line, the husk reading weakens for the post-2011 period specifically (the directive regained behavioral force) even if it holds for 1933-2011. If zoning remained governed by ordinary market and administrative logic despite the marker''s renewed fame, the husk reading is corroborated across the full interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2011_reactivation_ambiguity, empirical, 'Whether 2011 media attention converted symbolic fame into renewed binding land-use force.').

omega_variable(
    beneficiary_structure_intentionality,
    'Is the shift toward commemorative framing a passive drift (institutional memory simply decaying without any actor''s design) or a structure that development and tourism interests have some incentive to preserve rather than resolve?',
    'Examine whether any development or tourism-sector lobbying, public commentary, or municipal planning debate explicitly favors leaving the stone''s status ambiguous rather than formally codifying or formally retiring it.',
    'If explicit lobbying for ambiguity is found, this shifts the classification pressure from piton (no concentrated beneficiary large enough to warrant snare) toward snare (an identifiable actor actively works to keep the directive unenforced for profit). Absent such evidence, piton remains the more accurate read: no one designed the decay, but several parties passively benefit from not fixing it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_intentionality, conceptual, 'Whether the decay is passive institutional drift or a structure actively maintained by beneficiary inaction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(aney_tr_t0, observed).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 13, 0.25).
narrative_ontology:measurement_basis(aney_tr_t13, observed).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 26, 0.4).
narrative_ontology:measurement_basis(aney_tr_t26, observed).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 39, 0.55).
narrative_ontology:measurement_basis(aney_tr_t39, observed).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 52, 0.68).
narrative_ontology:measurement_basis(aney_tr_t52, observed).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 65, 0.76).
narrative_ontology:measurement_basis(aney_tr_t65, observed).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 78, 0.81).
narrative_ontology:measurement_basis(aney_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(aney_be_t0, observed).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 13, 0.28).
narrative_ontology:measurement_basis(aney_be_t13, observed).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 26, 0.41).
narrative_ontology:measurement_basis(aney_be_t26, observed).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 39, 0.52).
narrative_ontology:measurement_basis(aney_be_t39, observed).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 52, 0.6).
narrative_ontology:measurement_basis(aney_be_t52, observed).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 65, 0.64).
narrative_ontology:measurement_basis(aney_be_t65, observed).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 78, 0.68).
narrative_ontology:measurement_basis(aney_be_t78, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_stone_directive__behavioral_competence_reading are the two declared readings of the aneyoshi_stone_directive kernel. They share a founding text (the inscribed stone) and a founding problem (protecting future generations from tsunami inundation) but diverge in claimed behavioral continuity, epsilon, and beneficiary/victim structure. This story (commemorative_husk_reading) claims piton with substantial diffuse extraction (0.68) and an identifiable development/tourism beneficiary set; the sibling claims sustained behavioral force with correspondingly lower authored extraction. Per the ε-invariance principle, they are authored as two separate constraints with two separate ε values, linked only via network.affects_constraints and cs_structure.reading_relations — never merged into one story with an observable-dependent epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
