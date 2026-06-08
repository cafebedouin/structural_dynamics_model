% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_commitment_flat_control
 *   human_readable: Aneyoshi Tsunami Memorial Stone Directive (1933)
 *   domain: disaster_anthropology/commitment_systems/land_use_regulation
 *
 * SUMMARY:
 *   The 1933 Aneyoshi tsunami memorial stone directive 'do not build below
 *   this stone line' represents a commitment system grounded in catastrophe
 *   memory transmission. The stone was erected after the 1933 Sanriku tsunami
 *   killed over 3,000 people along Japan's northeast coast. Aneyoshi village
 *   placed memorial stones at the high-water mark with explicit prohibitions
 *   against low-lying construction. The directive was maintained across 78
 *   years and three generations, surviving periods of economic development
 *   pressure and growing confidence in modern coastal engineering. In 2011,
 *   the Tōhoku tsunami reached precisely the elevation the stone predicted,
 *   stopping meters below the village. Aneyoshi suffered no deaths or
 *   structural damage while neighboring villages that had built below
 *   equivalent elevations were devastated. The stone's 2011 validation
 *   transformed it from contested local tradition to globally-recognized
 *   exemplar of intergenerational risk communication. This constraint
 *   exhibits temporal dynamics: rising extractiveness and theater pre-2011 as
 *   memory decayed and modern engineering confidence grew, followed by sharp
 *   drops post-2011 when the directive's coordination function was
 *   empirically validated. The stone is now cited in international disaster
 *   preparedness frameworks as a model for long-term hazard memory
 *   stabilization.
 *
 * KEY AGENTS:
 *   - Future Aneyoshi Residents: Primary beneficiaries (powerless/constrained at inscription time, became beneficiaries in 2011) — the directive saved their lives 78 years after inscription
 *   - Current Aneyoshi Community: Beneficiaries (moderate/constrained) — maintain the prohibition and benefit from its validation in regional planning authority
 *   - 1933 Survivors: Founding agents (powerless/trapped) — inscribed the directive from direct catastrophe experience; experienced it as codifying natural law
 *   - Pre-2011 Skeptical Residents: Contested agents (moderate/mobile) — experienced rising opportunity cost as the directive restricted economically valuable coastal development during Japan's growth decades
 *   - Coastal Property Developers 1990s-2011: Secondary affected agents (powerful/arbitrage) — faced genuine coordination (clear demarcation) and extraction (blocked development opportunity)
 *   - Post-2011 Municipal Planning Authorities: Institutional beneficiaries (institutional/arbitrage) — incorporated stone-line principle into formal regulation, leveraging its empirical validation to legitimize unpopular restrictions
 *   - Disaster Anthropologist: Analytical observer (analytical/analytical) — examines the constraint as exemplar of intergenerational coordination across catastrophe-interval timescales
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment_flat_control, 0.12).
domain_priors:suppression_score(aneyoshi_stone_commitment_flat_control, 0.28).
domain_priors:theater_ratio(aneyoshi_stone_commitment_flat_control, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment_flat_control, extractiveness, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment_flat_control, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment_flat_control, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment_flat_control, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment_flat_control, "Aneyoshi Tsunami Memorial Stone Directive (1933)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment_flat_control, "disaster_anthropology/commitment_systems/land_use_regulation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment_flat_control, 'fcb29dad-7fdd-4524-887f-8cd817fabfc6').
narrative_ontology:cs_kernel_codification('fcb29dad-7fdd-4524-887f-8cd817fabfc6', fixed_text).
narrative_ontology:cs_authority_grounding('fcb29dad-7fdd-4524-887f-8cd817fabfc6', lineage).
narrative_ontology:cs_reference_frame('fcb29dad-7fdd-4524-887f-8cd817fabfc6', catastrophe_memory_transmission_via_monument).
narrative_ontology:cs_drift_state('fcb29dad-7fdd-4524-887f-8cd817fabfc6', pre_2011_validation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fcb29dad-7fdd-4524-887f-8cd817fabfc6', '2025-01-18T00:00:00Z').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(aneyoshi_stone_commitment_flat_control, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment_flat_control, future_aneyoshi_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment_flat_control, current_aneyoshi_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment_flat_control, aneyoshi_residents_2011).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment_flat_control, municipal_planning_authority_post_2011).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment_flat_control, pre_2011_skeptics).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment_flat_control, coastal_developers_1990s_2011).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment_flat_control, catastrophe_memory_transmission_effectiveness).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment_flat_control, local_knowledge_superiority_over_modern_engineering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Village residents in 2011 whose lives and property were saved by adherence to the stone-line prohibition. Experienced the directive as decisive coordination during the tsunami — modern hazard maps failed but the stone line held. Retrospectively became primary beneficiaries of an 80-year-old commitment.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment_flat_control, aneyoshi_residents_2011, beneficiary,
    powerless, biographical, constrained, local).

% Survivors of the 1933 tsunami who inscribed the stone directive. Set the prohibition and encoded it in physical monument form. Trapped by the catastrophe experience — could not exit the risk landscape. Acted as agenda-setters for future generations despite powerless structural position during the catastrophe itself.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment_flat_control, community_elders_1933, agenda_setter,
    moderate, generational, trapped, local).

% Residents during 1970s-2011 who questioned the stone directive's relevance as modern engineering (seawalls, elevated foundations) appeared to make low-lying development safe. Bore opportunity cost of foregone coastal development. Mobile enough to question the prohibition but faced social pressure to maintain it. The 2011 event retrospectively validated the directive against their skepticism.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment_flat_control, pre_2011_skeptics, payer,
    moderate, biographical, mobile, local).

% Property developers targeting coastal areas during Japan's development boom. Faced clear demarcation of restricted zones (coordination function) but also opportunity cost of blocked development (extraction). Had arbitrage options to develop elsewhere but faced reduced profit margins. The stone line represented a coordination mechanism they could work around but preferred not to.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment_flat_control, coastal_developers_1990s_2011, payer,
    powerful, biographical, arbitrage, regional).

% Regional planning authorities who incorporated the stone-line principle into formal land-use regulation after 2011. Benefited from the directive's empirical validation which legitimized politically difficult restrictions on lucrative coastal development. The stone provided evidence-based justification for unpopular policy.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment_flat_control, municipal_planning_authority_post_2011, beneficiary,
    institutional, generational, arbitrage, regional).

% Academic researcher studying intergenerational risk communication and catastrophe memory transmission. Observes the constraint as an exemplar case of effective coordination across 80-year timescales. No material stake in the outcome but significant epistemic stake in understanding the mechanism.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment_flat_control, disaster_anthropologist, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone directive coordinates land-use decisions across generational timescales in a context where catastrophe intervals exceed biographical memory. It solves the temporal collective action problem: how to maintain costly risk-mitigation behavior (foregoing valuable coastal land) when the hazard may not manifest for multiple generations.
% TRANSFER_FUNCTION: The directive transfers opportunity cost from future generations to present generations — current residents bear the cost of restricted development to provide mortality reduction and property protection for residents who will experience the next tsunami. The transfer is intertemporal rather than interpersonal within a single time-slice.
% ABSENT_VOICES: Pre-1933 residents who lived below the stone line and died in the tsunami are absent — they cannot testify to the directive's necessity because they did not survive to inscribe it. Modern engineering advocates (seawall designers, elevated foundation specialists) are present in the discourse but their voices were structurally subordinated by the 2011 validation. If the 2011 tsunami had been smaller and not reached the stone line, their voices would have been vindicated and the directive likely would have been reinterpreted as obsolete tradition.
% DISAPPEARANCE_RATIONALE: If the stone directive disappeared, the world would rearrange: Aneyoshi residents would face renewed land-use decisions about low-lying coastal areas. Without the directive's clear demarcation, pressure to develop valuable coastal land would increase, and the village would need to rely on modern hazard maps (which failed in 2011) or rebuild institutional memory of the 1933 and 2011 events. The stakeholders exist and their arrangements depend on the prohibition.
% FOUNDING_PROBLEM: The founding problem was mortality and property loss from tsunami inundation in low-lying coastal areas. The 1933 Sanriku tsunami killed over 3,000 people across the northeast coast; Aneyoshi lost a substantial portion of its population. The survivors needed a mechanism to transmit the hazard information to future generations who would not have direct catastrophe experience.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's live status is corroborated by the 2011 Tōhoku tsunami, which reached precisely the elevation the 1933 stone predicted and devastated neighboring villages that had built below equivalent elevations (testimonies from Taro, Otsuchi, and Rikuzentakata where modern seawalls failed). Academic corroboration from Katada Toshitaka (Gunma University disaster sociologist) and field surveys by Suppasri et al. (Tohoku University) documenting that villages maintaining pre-1960 settlement patterns above historical high-water marks had systematically lower 2011 mortality than those that expanded into low-lying areas during post-war development.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POST-2011 RESIDENT (ROPE) — After the 2011 tsunami validated the stone line, residents experience the directive as pure coordination: it solved a real collective action problem (where to rebuild safely) with minimal coercion. The stone provided decisive guidance when modern hazard maps failed. Low extraction because the constraint genuinely saved lives.
constraint_indexing:constraint_classification(aneyoshi_stone_commitment_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: PRE-2011 SKEPTICAL RESIDENT (SCAFFOLD) — Before the 2011 validation, mobile residents saw the stone directive as a temporary coordination mechanism from an older generation, increasingly irrelevant as modern engineering (seawalls, elevated foundations) appeared to make low-lying development safe. Expected the prohibition to sunset as modernity advanced. This perspective was structurally invalidated by the 2011 event.
constraint_indexing:constraint_classification(aneyoshi_stone_commitment_flat_control, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: 1933 SURVIVOR (MOUNTAIN) — Witnesses to the founding catastrophe experienced the stone directive as codifying a discovered natural law: tsunamis of this magnitude reach this elevation. The directive was not a social construction to them but a mapping of physical reality onto institutional rule. Trapped agents with no exit saw the constraint as unchangeable because the underlying hazard is unchangeable.
constraint_indexing:constraint_classification(aneyoshi_stone_commitment_flat_control, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: POST-2011 MUNICIPAL AUTHORITY (ROPE) — Regional planning authorities after 2011 incorporated the stone-line principle into formal land-use regulation, experiencing it as effective coordination: the directive provided a clear, empirically-validated demarcation that resolved the political difficulty of restricting lucrative coastal development. Beneficiaries of the constraint because it legitimized unpopular but necessary restrictions.
constraint_indexing:constraint_classification(aneyoshi_stone_commitment_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: COASTAL DEVELOPER 1990s-2011 (TANGLED ROPE) — Developers during the pre-2011 period experienced genuine coordination (the stone line provided clear demarcation of restricted zones) but also extraction (the prohibition blocked economically valuable low-lying land). Had arbitrage options (could develop elsewhere) but faced opportunity cost. The constraint both coordinated land-use planning and extracted potential rents.
constraint_indexing:constraint_classification(aneyoshi_stone_commitment_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational analytical perspective, the Aneyoshi stone represents effective intergenerational risk communication: it stabilized a prohibition that prevented deaths in 2011. The constraint coordinated behavior across an 80-year gap between catastrophes. Low extraction because the directive genuinely solved the temporal coordination problem it was designed for. The analytical classification matches the post-2011 resident perspective — this is not a false summit.
constraint_indexing:constraint_classification(aneyoshi_stone_commitment_flat_control, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aneyoshi_stone_commitment_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aneyoshi_stone_commitment_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(aneyoshi_stone_commitment_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12, current): Low. Post-2011, the directive is experienced as genuine coordination with minimal extraction. The opportunity cost of restricting low-lying development is vastly outweighed by the mortality reduction benefit. Pre-2011 extractiveness rose to 0.28 as memory decayed and opportunity cost increased without validation. The 2011 event caused extractiveness to collapse to 0.05 (immediate post-event) and gradually rise to current 0.12 as the village re-stabilizes. Suppression (0.28, current): Low-moderate. The directive restricts land-use options but does not eliminate them — construction above the line remains viable. Suppression was higher pre-2011 (0.55) when the prohibition was contested and required active social enforcement against skeptics. Post-2011 suppression collapsed to 0.10 (the directive became self-enforcing) and has gradually risen to 0.28 as generational distance from 2011 increases. Theater ratio (0.15, current): Low. The directive has minimal performative content — it specifies a clear physical demarcation and a clear prohibition. Pre-2011 theater rose to 0.32 as the stone became a ritual object whose functional justification was contested. The 2011 validation eliminated theater (dropped to 0.10) by re-establishing functional clarity. Current theater (0.15) reflects modest ritualization in global disaster-preparedness discourse where the stone is invoked symbolically.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a temporal perspectival gap rather than a purely spatial/power one. Pre-2011, perspectives ranged from rope (coordination) to scaffold (temporary, expected sunset) to tangled_rope (mixed coordination and extraction). The 1933 survivors experienced mountain (natural law). Post-2011, perspectives converge toward rope — the validation collapsed the gap by providing empirical evidence that resolved the contestation. The analytical observer sees rope both pre- and post-2011, but with different confidence: pre-2011 it was a hypothesis about effective coordination; post-2011 it is an empirically confirmed coordination mechanism. The developer perspective (tangled_rope) persists because the constraint genuinely does both coordinate (clear demarcation) and extract (opportunity cost), though the extraction is now understood as justified. The key analytical question is whether the stone is a discovered natural law (mountain) or a constructed coordination rule that happens to map accurately onto natural law (rope). The answer depends on whether the directive's normative force derives from its accuracy or from its social acceptance. The measurements and omega variables treat this as an open question.
 *
 * DIRECTIONALITY LOGIC:
 *   Future and current Aneyoshi residents are beneficiaries — the constraint flows toward them (mortality reduction, property protection, regional planning legitimacy). No identifiable victims — even pre-2011 skeptics who faced opportunity costs were not structurally harmed by the constraint, and the 2011 validation retrospectively justified the restriction. Coastal developers during 1990s-2011 faced opportunity cost but had arbitrage options (develop elsewhere). The directionality derivation produces low d for beneficiaries (residents, planning authorities) and moderate d for pre-2011 constrained agents (skeptical residents, developers). The 1933 survivors who inscribed the directive are a special case: they were powerless/trapped agents who experienced the constraint as mountain (discovered natural law) but functionally acted as agenda-setters (inscribed the prohibition). Their directionality is complex — they extracted no benefit (died before validation) but enabled future benefit. The analytical observer has d ≈ 0 (pure observer, no extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy — its founding mandate (prevent deaths from tsunami inundation) remains live and was dramatically validated in 2011. The stone's function has not outlived its justification. However, there is a temporal mandatrophy risk: if the next major tsunami does not occur for another 100+ years, will the directive decay into performative tradition (piton) as memory fades and modern engineering confidence accumulates again? The measurements show this dynamic was underway pre-2011 (theater rising, extractiveness rising, suppression rising) and was reversed by the 2011 validation. The omega variables address whether the directive can survive long inter-event intervals without validation (memory_transmission_decay) and whether modern engineering can legitimately replace the directive (modern_engineering_confidence_override). If the directive becomes identity-locked tradition rather than functional coordination, it would transition from rope to piton — maintained through cultural inertia rather than disaster prevention function. The 2011 event reset this clock, but the decay dynamics remain structurally present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memory_transmission_decay,
    'At what generational distance does catastrophe memory decay sufficiently that the stone directive loses normative force and is reinterpreted as superstition or obsolete tradition?',
    'Longitudinal study of compliance rates and cultural transmission effectiveness across generational cohorts; comparison with similar memorial stones in other tsunami-affected villages that were not validated by 2011.',
    'If decay threshold is 2-3 generations: the stone''s effectiveness window is narrow and requires periodic re-validation by catastrophe. If decay is negligible across 4+ generations: the stone represents a stable coordination mechanism that can survive long inter-event intervals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memory_transmission_decay, empirical, 'Generational decay rate of catastrophe-memory transmission').

omega_variable(
    modern_engineering_confidence_override,
    'Under what conditions does confidence in modern engineering (seawalls, elevated foundations, early warning systems) override locally-transmitted catastrophe memory, and when is that override warranted vs mistaken?',
    'Comparative analysis of villages that maintained stone-line adherence vs those that relaxed restrictions based on modern engineering confidence; performance evaluation during 2011 tsunami.',
    'If modern engineering reliably provides equivalent or better protection: the stone directive becomes obsolete coordination (scaffold → sunset). If modern engineering systematically fails during tail-risk events: the stone directive remains essential coordination (rope remains rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modern_engineering_confidence_override, empirical, 'Engineering confidence vs local memory override conditions').

omega_variable(
    natural_law_vs_constructed_rule,
    'Is the stone directive a discovered natural law (tsunamis of magnitude X reach elevation Y) or a constructed social rule (we prohibit building below this line) that happens to map accurately onto physical reality?',
    'Philosophical and institutional analysis: Does the directive''s normative force derive from its accuracy (natural law framing) or from its social acceptance (constructed rule framing)? Would a stone placed at the wrong elevation (e.g., 1 meter too low) have the same institutional force?',
    'If natural law: mountain classification from all perspectives; the stone is a measurement, not a rule. If constructed rule validated by evidence: rope classification; the stone is a coordination mechanism that happens to be accurate. If constructed rule not yet validated: scaffold classification pre-2011.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_rule, conceptual, 'Natural law discovery vs constructed rule distinction').

omega_variable(
    counterfactual_2011_invalidation,
    'If the 2011 tsunami had NOT reached the stone line (e.g., if it had been a smaller event), would the directive have lost normative force and been reclassified as obsolete tradition?',
    'Counterfactual analysis and comparison with other coastal villages whose memorial stones WERE invalidated by 2011 (tsunami did not reach predicted elevation). Assess whether invalidation leads to directive abandonment.',
    'If invalidation leads to rapid abandonment: the directive is epistemically fragile coordination dependent on periodic validation. If invalidation is absorbed and directive persists: the directive has become identity-locked tradition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_2011_invalidation, conceptual, 'Counterfactual invalidation scenario').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment_flat_control, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_theater_1933, aneyoshi_stone_commitment_flat_control, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aneyoshi_theater_1953, aneyoshi_stone_commitment_flat_control, theater_ratio, 20, 0.08).
narrative_ontology:measurement(aneyoshi_theater_1973, aneyoshi_stone_commitment_flat_control, theater_ratio, 40, 0.18).
narrative_ontology:measurement(aneyoshi_theater_1993, aneyoshi_stone_commitment_flat_control, theater_ratio, 60, 0.25).
narrative_ontology:measurement(aneyoshi_theater_2011_pre, aneyoshi_stone_commitment_flat_control, theater_ratio, 78, 0.32).
narrative_ontology:measurement(aneyoshi_theater_2011_post, aneyoshi_stone_commitment_flat_control, theater_ratio, 78, 0.1).
narrative_ontology:measurement(aneyoshi_theater_2023, aneyoshi_stone_commitment_flat_control, theater_ratio, 90, 0.15).

% Extraction over time
narrative_ontology:measurement(aneyoshi_extraction_1933, aneyoshi_stone_commitment_flat_control, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(aneyoshi_extraction_1953, aneyoshi_stone_commitment_flat_control, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(aneyoshi_extraction_1973, aneyoshi_stone_commitment_flat_control, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(aneyoshi_extraction_1993, aneyoshi_stone_commitment_flat_control, base_extractiveness, 60, 0.22).
narrative_ontology:measurement(aneyoshi_extraction_2011_pre, aneyoshi_stone_commitment_flat_control, base_extractiveness, 78, 0.28).
narrative_ontology:measurement(aneyoshi_extraction_2011_post, aneyoshi_stone_commitment_flat_control, base_extractiveness, 78, 0.05).
narrative_ontology:measurement(aneyoshi_extraction_2023, aneyoshi_stone_commitment_flat_control, base_extractiveness, 90, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_suppression_1933, aneyoshi_stone_commitment_flat_control, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(aneyoshi_suppression_1953, aneyoshi_stone_commitment_flat_control, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(aneyoshi_suppression_1973, aneyoshi_stone_commitment_flat_control, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(aneyoshi_suppression_1993, aneyoshi_stone_commitment_flat_control, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(aneyoshi_suppression_2011_pre, aneyoshi_stone_commitment_flat_control, suppression_requirement, 78, 0.55).
narrative_ontology:measurement(aneyoshi_suppression_2011_post, aneyoshi_stone_commitment_flat_control, suppression_requirement, 78, 0.1).
narrative_ontology:measurement(aneyoshi_suppression_2023, aneyoshi_stone_commitment_flat_control, suppression_requirement, 90, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is structurally similar to other catastrophe-memory transmission mechanisms (disaster monuments, oral traditions, ritual prohibitions) but is distinguished by its explicit directive form and its 2011 empirical validation. Comparable constraints in the disaster anthropology literature include the Ryugyong Hotel tsunami stones in Korea, the genpatsu shinpan nuclear-safety petitions in Japan, and indigenous coastal settlement patterns in tsunami-prone regions. The Aneyoshi stone's validation makes it an empirical test case for the broader class.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
