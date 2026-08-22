% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Warning Stone — Live Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster anthropology / commitment systems / temporal institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the behavioral_competence_reading of the
 *   aneyoshi_land_use_prohibition kernel. On the Sanriku coast of Iwate
 *   Prefecture, stone markers erected after the 1896 Meiji Sanriku tsunami
 *   and renewed after the 1933 Showa Sanriku tsunami record the water's
 *   maximum reach and forbid building homes below the inscribed line. This
 *   reading holds that the prohibition was a live land-use rule,
 *   operationally enforced by hamlet practice across the 78 years from the
 *   1933 renewal to the 2011 Tohoku tsunami, and that the hamlet's 2011
 *   outcome — houses above the line, water reaching roughly the old markings
 *   — was the rule working as designed. The epsilon referent is the standing
 *   arrangement itself (the stone-enforced siting prohibition), assessed by
 *   this reading's own lights; the sibling commemorative_husk_reading
 *   assesses the same referent as decayed memorial and is authored as a
 *   separate constraint story, linked through network.affects_constraints.
 *   KEY AGENTS (by structural relationship): - aneyoshi_hamlet_residents:
 *   collective agenda-setter and beneficiary (organized/constrained) —
 *   maintains the stones, teaches the prohibition, sites all construction
 *   above the inscribed line; bears small forgone shoreline value; collected
 *   survival of homes and lives in 2011 - aneyoshi_postmemory_generation:
 *   residents raised after living memory of the founding tsunamis faded
 *   (moderate/identity_locked) — comply by taught obligation rather than
 *   witnessed catastrophe - iwate_coastal_planning_authorities: formal
 *   land-use apparatus outside the customary regime (institutional/arbitrage)
 *   — never codified or adjudicated the stone line -
 *   sanriku_disaster_researchers: analytical observers
 *   (institutional/analytical) — validated the markers against 2011
 *   inundation data; the seat from which the sibling memorial reading arises
 *   most naturally Assumptions stated: the interval is anchored at the 1933
 *   renewal (post-Showa-Sanriku reinscription) and closes at the 2011 Tohoku
 *   tsunami, giving the 78-year span the reading claims; the hamlet is
 *   treated as a single customary polity of a few dozen households;
 *   compliance is taken as continuous per post-2011 accounts, with the open
 *   question routed to the kernel_reading_divergence omega rather than
 *   assumed away.
 *
 * KEY AGENTS:
 *   - aneyoshi_hamlet_residents: collective agenda-setter and beneficiary (organized/constrained) — maintains the stones, teaches the prohibition, sites all construction above the inscribed line
 *   - aneyoshi_postmemory_generation: postmemory compliers (moderate/identity_locked) — keep the rule as inherited identity rather than witnessed necessity
 *   - iwate_coastal_planning_authorities: excluded formal land-use apparatus (institutional/arbitrage) — the permitting world the customary rule bypassed entirely
 *   - sanriku_disaster_researchers: analytical observers (institutional/analytical) — external validators of the markers' accuracy; the vantage from which the sibling husk reading is most natural
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.07).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.14).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Warning Stone — Live Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster anthropology / commitment systems / temporal institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '8cacf369-0674-42a4-9113-d9f767469103').
narrative_ontology:cs_kernel_codification('8cacf369-0674-42a4-9113-d9f767469103', fixed_text).
narrative_ontology:cs_authority_grounding('8cacf369-0674-42a4-9113-d9f767469103', lineage).
narrative_ontology:cs_interpretation_layer_present('8cacf369-0674-42a4-9113-d9f767469103').
narrative_ontology:cs_reading_relation('8cacf369-0674-42a4-9113-d9f767469103', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('8cacf369-0674-42a4-9113-d9f767469103', foundational, inscription_constitutes_binding_land_use_command).
narrative_ontology:cs_axiom_status(inscription_constitutes_binding_land_use_command, holdable).
narrative_ontology:cs_axiom_grounding('8cacf369-0674-42a4-9113-d9f767469103', inscription_constitutes_binding_land_use_command, conventional).
narrative_ontology:cs_axiom('8cacf369-0674-42a4-9113-d9f767469103', secondary, hazard_knowledge_transcends_individual_memory).
narrative_ontology:cs_axiom_status(hazard_knowledge_transcends_individual_memory, holdable).
narrative_ontology:cs_axiom_grounding('8cacf369-0674-42a4-9113-d9f767469103', hazard_knowledge_transcends_individual_memory, instrumental).
narrative_ontology:cs_reference_frame('8cacf369-0674-42a4-9113-d9f767469103', operative_hazard_boundary_rule).
narrative_ontology:cs_drift_state('8cacf369-0674-42a4-9113-d9f767469103', post_2011_validation_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('8cacf369-0674-42a4-9113-d9f767469103', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_hamlet_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_postmemory_generation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_postmemory_generation).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, intergenerational_hazard_memory_transmission).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, historical_inundation_marker_accuracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A few dozen households in a small cove hamlet on the Sanriku coast. They maintain the stone markers, teach children that homes go above the inscribed line, and decide where each new house stands. Building above the line costs them shoreline plots they could otherwise use; staying above it put their houses on high ground when the 2011 tsunami came. Leaving the hamlet would mean leaving ancestral graves, fishing grounds, and family land.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_hamlet_residents, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_hamlet_residents, agenda_setter).

% Residents born after the last hamlet-wide experience of the founding tsunamis, who inherited the siting rule as instruction rather than memory. They keep the rule because it is part of what being of this hamlet means — taught at home, reinforced at annual observances at the stones. Their alternative to compliance is not a different plot but a different self: building below the line would mean breaking with elders, neighbors, and the dead named on the stone.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_postmemory_generation, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_postmemory_generation, payer).

% Prefectural and municipal offices that approve land use along the Sanriku coast. The hamlet's siting rule never entered their permit framework; they regulated the surrounding coast through zoning and postwar reconstruction programs without reference to the stones. Had the inscribed line been on their desks, the development pressures visible elsewhere on the coast suggest they would have weighed it differently; in this hamlet they were simply never part of the conversation.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, iwate_coastal_planning_authorities, excluded,
    institutional, generational, arbitrage, regional).

% Geologists, engineers, and disaster anthropologists who surveyed the coast after 2011, compared the 2011 inundation line with the stones' inscriptions, and published on the markers' accuracy. They take no part in the hamlet's decisions; their assessments shape how the wider world reads the stones — as warnings, as heritage, or as both.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, sanriku_disaster_researchers, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational siting problem: where to place homes and communal buildings on a coast struck by large tsunamis at intervals longer than any individual's memory. The inscribed line converts one generation's observed maximum inundation into a durable siting boundary that later generations can follow without re-witnessing the hazard.
% TRANSFER_FUNCTION: Moves almost nothing material. Each generation transfers hazard knowledge and a siting obligation to the next; current builders give up shoreline plots (small forgone land value and convenience) in exchange for placement above the recorded reach of the water. No goods, money, or labor flow from any party to any other.
% ABSENT_VOICES: Formal land-use authorities (prefectural and municipal planning offices) never sat inside the arrangement — the rule ran entirely through hamlet custom, so postwar development priorities were never adjudicated against the stone line in any forum. Shoreline-proximity interests (fishermen wanting structures near their boats) are the internal voice that would object; within Aneyoshi none pressed the point hard enough to break compliance. Neighboring Sanriku villages that discarded or ignored their own markers lie outside this story's parties entirely.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, siting decisions would lose their only transmitted boundary: as living memory of 1933 faded, new construction would creep downslope toward the water, and the 2011 tsunami would have reached occupied houses instead of empty ground. The hamlet's survival pattern, inheritance practices, and the physical location of every home depend on the line holding.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami devastated the coast, survivors needed a way to keep future settlement out of the inundation zone despite a recurrence interval longer than any resident's remaining memory. The stones recorded the water's maximum reach and forbade building below it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: post-2011 geological and engineering surveys of the Sanriku coast independently confirmed that the 2011 inundation reached approximately the level the stones record, and the disaster-science literature treats the markers as accurate hazard records. Prefectural hazard mapping and archival documentation of the 1896 and 1933 death tolls attest the founding catastrophe. What only the hamlet itself attests is that compliance was continuous — the physics is corroborated externally; the behavioral record is not.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.07, 'stealth/ox-alpha', 'none', direct).

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
 *   Scores are authored descriptively and independently of the claim. Extractiveness 0.07: the arrangement's only cost is forgone shoreline plots, borne and benefited by the same population — no transfer between parties exists to extract through. Suppression 0.14: enforcement is assent-based (custom, home teaching, annual observance); there is no coercive apparatus, and per the framework suppression is a raw unscaled structural property. Theater_ratio 0.20: ceremonial activity around the stones grew as living memory faded (memorial services doubling as transmission events), but the function stayed operative — the 2011 siting decision is the proof — so the rise is ritual accretion around a live function, not Goodhart substitution; it remains well below the 0.5 drift threshold. Accessibility_collapse 0.60: the physics collapses the below-line alternative in the long run (the sea always eventually collects), but multi-decade silence lets rare-event discounting persist, so collapse is incomplete — unlike a strict natural law at 0.85+. Resistance 0.08: no documented push to build below the line within the hamlet. Claimed type rope is asserted from structure: a genuine collective-action problem (siting against a hazard whose recurrence exceeds memory), participants as net beneficiaries, minimal coercive overhead, no suppressed alternative and no victim. The kernel context's 'no beneficiary structure' is honored as no asymmetric rent-collector: the declared beneficiaries are the participant body itself, which is what makes this a rope rather than anything with a payer seat. Measurements run on one shared six-point grid (1933-2011) for both tracked metrics; suppression_requirement is deliberately not serialized because the enforcement picture is static-by-design (assent-based throughout) and is carried by the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. Residents with living memory of the founding tsunamis (early interval) experienced the rule as obvious self-protection — near-zero felt burden. The postmemory generation experiences the same rule as inherited identity-obligation: identical behavior, different epistemic footing, and an exit option that is not a different plot but a different self. External researchers see the stone primarily as data and heritage; from that vantage the ceremonial surface is more visible than the behavioral core — which is precisely the vantage from which the sibling commemorative_husk_reading arises. The excluded planning authorities never experienced the rule at all; they experienced its absence from their desks. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared (aneyoshi_hamlet_residents, aneyoshi_postmemory_generation), driving derived directionality toward the beneficiary end for both; no victims are declared because none exist — the arrangement extracts from no one. Gain_flow is authored as 'diffuse' affirmatively: every named seat was checked, and none captures another's contribution — the only costs are opportunity costs borne and benefits received by the same population. Scope is local: in a hamlet of a few dozen households, compliance is verifiable by eye, keeping verification cheap and effective extraction near base epsilon. The identity lock on the postmemory seat stabilizes participation without raising extraction, because the lock binds beneficiaries here, not targets — identity fusion amplifies effective extraction only for agents the arrangement takes from.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: the Sanriku hazard persists, and extreme-event recurrence spans centuries — longer than the transmission mechanism has yet been tested beyond its first 78-year silence. The mandate has not outlived its function, so no mandatrophy resolution is declared. The classification guards against two mislabels. First, mistaking the gently rising theater_ratio for piton drift: the theater is ceremonial accretion around a demonstrably operative function (verified at the interval endpoint by the 2011 siting outcome), not proxy replacement. Second, mistaking low epsilon for absence of constraint: the rule bound behavior for 78 years — it is a real constraint, merely a cheap one, and cheapness here is the signature of a rope doing its job, not of vacuity. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: no mismatch, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence_behavioral_vs_memorial,
    'This story instantiates the behavioral_competence_reading of the aneyoshi_land_use_prohibition kernel: the stone operated as a live land-use rule, enforced through hamlet practice across 78 years. The sibling commemorative_husk_reading holds that the stone is a historical memorial whose prohibition decayed to symbol without behavioral force. Which reading describes the arrangement''s actual operation?',
    'Construction records, oral histories, and permit archives for the hamlet across 1933-2011: did any dwelling go up below the inscribed line, and was the line cited in siting decisions? Post-2011 interviews with residents on why homes stand where they do.',
    'If the husk reading is right, this story''s low theater_ratio and rope-type claim misdescribe the arrangement, which recomputes as inertial theatrical maintenance; if the behavioral reading is right, the sibling''s memorial framing describes only the ceremony layered on the rule, not the rule itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_behavioral_vs_memorial, empirical, 'Which reading of the stone kernel matches the arrangement''s actual operation.').

omega_variable(
    avoided_loss_attribution,
    'How much of the hamlet''s 2011 survival is attributable to the prohibition''s siting effect, versus overdetermination by topography (a cove mouth and steep slopes that limited inundation regardless of where homes stood)?',
    'Hydrodynamic modeling of the cove under counterfactual siting scenarios, compared against the documented 2011 inundation line and the inscribed marker elevations.',
    'Strong attribution sustains the coordination function''s legitimacy and the compliance it commands; weak attribution means survival was geographic luck, the rule''s behavioral grip weakens going forward, and the arrangement drifts toward the sibling''s husk condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(avoided_loss_attribution, empirical, 'Whether the prohibition, rather than geography alone, produced the 2011 outcome.').

omega_variable(
    transmission_durability_next_recurrence,
    'Does the enforcement mechanism generalize past the conditions that sustained it for 78 years — a stable hamlet population, ceremonial reinforcement, and elders with living memory — given that the next major tsunami may arrive after living memory of 2011 has faded, exactly as 2011 arrived after living memory of 1933 had faded?',
    'Longitudinal observation of the transmission practice (school curricula, annual observances, stone maintenance, siting deliberations) and demographic stability of the hamlet across the coming decades.',
    'If transmission decays, the arrangement drifts toward the sibling''s husk condition and eventual reclassification; if it holds through another silence, the low-extraction profile persists across a second untested gap and the reading''s claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_durability_next_recurrence, empirical, 'Whether the behavioral force survives a second memory-hazard gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1933, 0.06).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(aney_tr_t1968, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1968, 0.13).
narrative_ontology:measurement(aney_tr_t1985, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1985, 0.16).
narrative_ontology:measurement(aney_tr_t1998, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.2).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(aney_be_t1968, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1968, 0.06).
narrative_ontology:measurement(aney_be_t1985, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1985, 0.06).
narrative_ontology:measurement(aney_be_t1998, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1998, 0.07).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.07).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_land_use_prohibition__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Aneyoshi stone' covers two structurally distinct claims that must not share one story. This story (behavioral_competence_reading) authors the claim that the prohibition carried operational behavioral force across 1933-2011; the sibling (commemorative_husk_reading) authors the claim that the prohibition decayed to symbol without behavioral force. Both readings share one referent — the standing arrangement of the stone-enforced siting rule — and differ in reading-indexed assessment of it (OQ-26), so each file carries its own epsilon, its own metrics, and its own claimed type. This reading is the upstream member (higher empirical confidence after the 2011 validation) and structurally pressures the sibling's plausibility conditions without logically eliminating it; the edge is declared here and mirrored in the sibling's network block.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
