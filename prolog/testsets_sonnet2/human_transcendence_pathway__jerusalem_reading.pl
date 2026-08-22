% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Reading: Rebuilding Authentic Community Through Patient Participatory Labor
 *   domain: Catholic Social Doctrine / Technology Ethics / Political Theology
 *
 * SUMMARY:
 *   This story instantiates the Jerusalem reading of the
 *   human-transcendence-pathway kernel: authentic community is rebuilt not
 *   through coercive unification (the Babel reading) nor through
 *   technological transcendence of limits (the technocratic/incarnational
 *   contest), but through slow, voluntary, participatory labor that
 *   deliberately preserves plurality of trade, lineage, and custom while
 *   integrating those differences into a shared common life under a horizon
 *   of divine blessing rather than self-sufficient human mastery. The
 *   rebuilding is inefficient by design — efficiency is sacrificed to
 *   solidarity — and the resulting social body is a genuine coordination
 *   achievement, not an extraction structure. No party is structurally
 *   positioned as a victim; the cost borne by laborers is the ordinary cost
 *   of voluntary common work, not extraction by an agenda-setting authority
 *   who profits asymmetrically.
 *
 * KEY AGENTS:
 *   - returning_exiles: Primary beneficiaries (powerless/constrained) — gain inclusion and standing through participation
 *   - rebuilt_community_as_whole: Emergent beneficiary — the trust and infrastructure produced by joint labor
 *   - local_leaders_and_elders: Agenda-setters who coordinate by persuasion, not coercion
 *   - skilled_and_unskilled_laborers: Bear the real but voluntary cost of foregone efficiency
 *   - surrounding_rival_communities: Excluded voices favoring faster, more centralized alternatives
 *   - theological_and_ethical_observers: Analytical seat assessing whether plurality is genuinely integrated or merely papered over
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.22).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.15).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Reading: Rebuilding Authentic Community Through Patient Participatory Labor").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "Catholic Social Doctrine / Technology Ethics / Political Theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '65c62ca4-363e-4126-b22c-d9b9c18b6db7').
narrative_ontology:cs_kernel_codification('65c62ca4-363e-4126-b22c-d9b9c18b6db7', distributed).
narrative_ontology:cs_authority_grounding('65c62ca4-363e-4126-b22c-d9b9c18b6db7', practice).
narrative_ontology:cs_interpretation_layer_present('65c62ca4-363e-4126-b22c-d9b9c18b6db7').
narrative_ontology:cs_reading_relation('65c62ca4-363e-4126-b22c-d9b9c18b6db7', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_reading_relation('65c62ca4-363e-4126-b22c-d9b9c18b6db7', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('65c62ca4-363e-4126-b22c-d9b9c18b6db7', foundational, plurality_as_resource_not_threat).
narrative_ontology:cs_axiom_status(plurality_as_resource_not_threat, holdable).
narrative_ontology:cs_axiom_grounding('65c62ca4-363e-4126-b22c-d9b9c18b6db7', plurality_as_resource_not_threat, deontological).
narrative_ontology:cs_axiom('65c62ca4-363e-4126-b22c-d9b9c18b6db7', foundational, transcendence_received_through_formation_not_imposed_by_unification).
narrative_ontology:cs_axiom_status(transcendence_received_through_formation_not_imposed_by_unification, holdable).
narrative_ontology:cs_axiom_grounding('65c62ca4-363e-4126-b22c-d9b9c18b6db7', transcendence_received_through_formation_not_imposed_by_unification, theological).
narrative_ontology:cs_axiom('65c62ca4-363e-4126-b22c-d9b9c18b6db7', secondary, efficiency_subordinate_to_solidarity).
narrative_ontology:cs_axiom_status(efficiency_subordinate_to_solidarity, holdable).
narrative_ontology:cs_axiom_grounding('65c62ca4-363e-4126-b22c-d9b9c18b6db7', efficiency_subordinate_to_solidarity, instrumental).
narrative_ontology:cs_reference_frame('65c62ca4-363e-4126-b22c-d9b9c18b6db7', covenantal_plural_communion).
narrative_ontology:cs_drift_state('65c62ca4-363e-4126-b22c-d9b9c18b6db7', contemporary_pluralist_societies, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65c62ca4-363e-4126-b22c-d9b9c18b6db7', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, rebuilt_community_as_whole).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, skilled_and_unskilled_laborers).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, skilled_and_unskilled_laborers).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, plurality_integrated_into_communion).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, shared_responsibility_over_centralized_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have returned from displacement to a fragmented, under-resourced homeland and must rebuild both physical infrastructure and communal identity from a position of material weakness. They gain standing and inclusion in the rebuilt community precisely through their participation in the slow, unglamorous labor of reconstruction, not through imposed uniformity.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles, beneficiary,
    powerless, generational, constrained, regional).

% The emergent social body formed by the joint labor of diverse households, trades, and factions who retain their distinct identities while contributing to a shared project. Benefits from durable social trust built through voluntary cooperation rather than coercive consolidation.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, rebuilt_community_as_whole, beneficiary,
    moderate, generational, constrained, regional).

% Coordinate the rebuilding effort by persuasion, shared deliberation, and appeal to covenantal obligation rather than command. They set the agenda for common work but hold no coercive enforcement apparatus; their authority depends on continued voluntary buy-in from the participating households.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, local_leaders_and_elders, agenda_setter,
    moderate, generational, constrained, regional).

% Contribute physical labor and time to a slow, inefficient rebuilding process that could be shortened by imposing uniform methods or excluding weaker contributors. They bear the real cost of foregone efficiency and personal time, but retain their trades, dialects, and local customs, and gain a durable stake in the outcome.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, skilled_and_unskilled_laborers, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, skilled_and_unskilled_laborers, beneficiary).

% Neighboring groups skeptical of or hostile to the rebuilding project, sometimes proposing faster or more centralized alternatives. Their objections are not incorporated into the deliberative process, though they remain free to pursue their own paths.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, surrounding_rival_communities, excluded,
    moderate, biographical, mobile, regional).

% Scholars and commentators who assess whether the rebuilding pattern actually integrates plurality into genuine communion or merely papers over unresolved tensions with pious language. They have no stake in the outcome but evaluate the pattern against its own claimed standard.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, theological_and_ethical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__jerusalem_reading, diffuse).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__jerusalem_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of rebuilding shared infrastructure, worship, and social trust after displacement, when no single household or faction has the resources or legitimacy to do it alone and imposed uniformity would fracture rather than unify the diverse returning population.
% TRANSFER_FUNCTION: Moves labor-time, skill, and material resources from individual households and trades into common structures (walls, temple, civic order), in exchange for durable inclusion, standing, and eventual mutual benefit within the rebuilt community — not into the pocket of a controlling authority.
% ABSENT_VOICES: Surrounding rival communities who favor faster, more centralized, or more assimilationist approaches are not part of the internal deliberation; their critique that the slow participatory method is inefficient or fragile is heard from outside, not incorporated as a governing consideration.
% DISAPPEARANCE_RATIONALE: If this pattern of patient participatory rebuilding vanished, the community would either fracture into disconnected factions pursuing separate, uncoordinated projects, or fall under a more coercive centralizing authority (the Babel-type alternative) to force efficiency at the cost of the plurality now preserved. The specific social trust generated by voluntary shared labor would not simply persist under a different mechanism.
% FOUNDING_PROBLEM: A displaced and fragmented people, returning from exile with no unified infrastructure, weakened institutions, and internally diverse identities (trades, lineages, degrees of prior assimilation), needed to rebuild a functioning common life without either abandoning their plurality or fracturing into permanent rival factions.
% FOUNDING_PROBLEM_CORROBORATION: Theological and ethical observers external to the rebuilding project attest that the founding problem (reconstituting genuine communion amid real diversity, without coercion) recurs across many post-displacement and post-crisis communities and is not fully resolved by any single historical instance; some observers argue the pattern is idealized in retrospect and that actual historical rebuilding efforts involved more coercion and exclusion than the reading credits, which is why the status is marked contested rather than simply live.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.22-0.28) because the referent — the standing rebuilding arrangement as this reading sees it — is a persuasion-and-formation-based coordination structure, not a coercive one; the modest decline over the interval reflects growing trust reducing residual friction rather than an extraction mechanism intensifying. Suppression is low (0.15) because no coercive enforcement apparatus exists; the agenda-setters hold authority contingent on continued voluntary buy-in. Theater ratio is modest (0.18-0.22) reflecting that some formation activity is genuinely performative (shared rituals, public deliberation) but this performance is functional to building trust, not a substitute for real coordination. Accessibility collapse is moderate (0.25): alternative paths (faster centralized rebuilding, or fragmenting into separate factions) remain visible and are not suppressed, only foregone by choice. Resistance is moderate (0.35) reflecting genuine tension between the slower participatory method and factions who would prefer efficiency.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setters' seat, this is straightforwardly cooperative: elders persuade, households volunteer, the arrangement reproduces itself through consent. From the laboring participants' seat, there is real cost — time, foregone efficiency, exposure to risk during vulnerable reconstruction — but this cost is not extraction because it is not captured asymmetrically by the agenda-setters; it is reinvested into the shared structure all parties will use. The engine should register these seats as convergent rather than divergent precisely because no party is positioned to extract rents from another's labor.
 *
 * DIRECTIONALITY LOGIC:
 *   Returning exiles and the rebuilt community as a whole are declared beneficiaries because the entire structural point of the arrangement is their inclusion and durable standing — this pulls their directionality toward the beneficiary end. Laborers carry a secondary payer role reflecting the real time and effort cost, but this cost is symmetric with eventual benefit (constrained exit, not trapped), keeping their derived directionality closer to symmetric than to full-target. No group is declared a victim because no party structurally profits from another's cost; the sacrifice of efficiency for solidarity is borne collectively, not asymmetrically extracted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rebuilding communion amid real plurality without coercion) is marked contested rather than simply live or dead, because while post-displacement community-formation is a recurring human problem that this pattern still addresses, some external observers argue the historical instances idealized in this reading involved more coercion and exclusion than the reading credits. This keeps the story honest: the Jerusalem reading is not claimed as a fully achieved or self-evidently resolved arrangement, but as a genuine coordination pattern whose success is partial and whose retrospective idealization is itself worth scrutiny — which is exactly what distinguishes a Rope with real friction from a mandate that has quietly become self-congratulatory theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jerusalem_kernel_reading_disambiguation,
    'This constraint is one reading (jerusalem_reading) of the shared kernel human_transcendence_pathway. The kernel also has two sibling readings not represented in this file: babel_reading (unified technological/linguistic self-sufficiency without transcendent reference) and technocratic_vs_incarnational_reading (transcendence via technological optimization vs. transcendence as gift received in vulnerability). Where is the disagreement located structurally?',
    'Compare the beneficiary/victim structure and epsilon across the three sibling constraint files: this reading declares no structural victims and low-moderate epsilon (persuasion/formation); the babel_reading would be expected to show different beneficiary concentration (centralized authority) and potentially higher suppression from imposed uniformity; the technocratic reading contests where transcendence itself is located (technological mastery vs. grace received in limitation). The disagreement is located in what counts as legitimate coordination: consent-based plural integration (this reading) vs. imposed unification (Babel) vs. mastery-over-limits (technocratic).',
    'If resolved toward the technocratic or babel readings being structurally dominant in a given historical instantiation, the jerusalem_reading''s low epsilon and absent-victims declaration would not transfer — those readings are separate constraints with their own files and their own epsilon, not alternative measurements of this same constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jerusalem_kernel_reading_disambiguation, conceptual, 'This story is one reading of a contested kernel; the sibling readings are separate constraint files, not alternative observables of this one.').

omega_variable(
    idealization_vs_historical_coercion,
    'Does the Jerusalem reading''s declaration of ''no structural victims'' hold under closer historical scrutiny, or does it retroject an idealized communal harmony onto a rebuilding process that in fact involved exclusion of some groups (e.g., those deemed insufficiently returned-from-exile, or intermarried households) from full participation?',
    'Historical-critical and textual analysis of primary rebuilding narratives, cross-checked against archaeological and social-historical reconstruction of post-displacement community boundaries, to assess whether exclusionary boundary-drawing accompanied the participatory rebuilding.',
    'If exclusionary boundary-drawing is substantiated as structural rather than incidental, this reading would need to declare a victim group (excluded sub-populations) and its extractiveness and suppression scores would rise, potentially shifting classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(idealization_vs_historical_coercion, empirical, 'Whether the reading''s benign beneficiary structure survives historical scrutiny or idealizes a more exclusionary reality.').

omega_variable(
    efficiency_sacrifice_measurement,
    'How much real cost (in delayed security, delayed material welfare, exposure to external threat during the slow rebuilding phase) does the plurality-preserving approach impose relative to a faster, more centralized alternative, and is that cost distributed evenly or does it fall disproportionately on the most vulnerable participants?',
    'Comparative case analysis against faster, more centralized rebuilding efforts in analogous post-crisis contexts, examining welfare and security outcomes during the transition period.',
    'If the efficiency cost falls disproportionately on the most vulnerable (rather than being genuinely shared), the ''no structural victims'' claim would need revision and extractiveness would need to rise for that sub-population specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_sacrifice_measurement, empirical, 'Whether the sacrificed efficiency is genuinely shared or quietly concentrated on the most vulnerable participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(huma_tr_t8, human_transcendence_pathway__jerusalem_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(huma_tr_t16, human_transcendence_pathway__jerusalem_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(huma_tr_t24, human_transcendence_pathway__jerusalem_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(huma_tr_t32, human_transcendence_pathway__jerusalem_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(huma_be_t8, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(huma_be_t16, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(huma_be_t24, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 24, 0.22).
narrative_ontology:measurement(huma_be_t32, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 32, 0.22).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(human_transcendence_pathway__jerusalem_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__jerusalem_reading, 0.1).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the human_transcendence_pathway kernel. babel_reading instantiates unified technological/linguistic self-sufficiency without transcendent reference (expected higher suppression, centralized beneficiary concentration). technocratic_vs_incarnational_reading contests whether transcendence is achieved through technological limit-elimination or received as grace in vulnerability. Each reading is authored as its own constraint with its own epsilon, beneficiary/victim structure, and classification, per the ε-invariance principle — they are not three measurements of one constraint but three structurally distinct claims sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
