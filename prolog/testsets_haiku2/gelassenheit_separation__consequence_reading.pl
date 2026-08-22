% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Gelassenheit Separation (Consequence Reading): Technology Evaluated by Communal Impact
 *   domain: religious/social/technological
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested Anabaptist
 *   kernel 'Gelassenheit Separation': it reads separation as the preservation
 *   of community practices (visiting, mutual aid, geographic rootedness) and
 *   evaluates technology by its functional role in sustaining those
 *   practices, NOT by its categorical resemblance to worldly artifacts
 *   (artifact-reading) or its structural isolation from worldly systems
 *   (principle-reading). The consequence reading permits fine-grained
 *   context-dependent permissions: telephones in barns (preserve rootedness
 *   by enabling farm families to remain in place) but not in homes (erode
 *   visiting by substituting distance contact for presence). Tractors may be
 *   used for belt power but not for field work at velocity that distances the
 *   farmer from the land. This reading is distinctly LOW-EXTRACTIVENESS
 *   because the technology permissions flow from genuine functional reasoning
 *   about community practices, not from hidden rent-extraction or worldly
 *   entanglement. The low suppression reflects that the framework invites
 *   younger generation and households to engage in consequentialist reasoning
 *   rather than merely obey categorical rules. The theater ratio is lowest
 *   among the three readings because the reasoning is forward-looking and
 *   outcome-focused (does this preserve community?) rather than performative
 *   boundary-maintenance (does this look different enough from English
 *   society?).
 *
 * KEY AGENTS:
 *   - Anabaptist faith communities (agenda-setter, organized power, identity-locked exit): apply the consequence test to evaluate technology; gain authority to permit fine-grained contextual uses
 *   - Community households (beneficiary, moderate power, identity-locked exit): navigate technology decisions through shared framework; gain concrete guidance that is NOT blanket prohibition
 *   - Younger generation (payer, moderate power, constrained exit): must articulate why a technology preserves or erodes community practices; face increased cognitive/social load of justification
 *   - Principle-reading advocates (excluded, moderate power, identity-locked exit): would argue for structural/functional-isolation test; circulate in communities but not in this reading's discernment
 *   - Artifact-reading advocates (excluded, moderate power, identity-locked exit): would argue for visible-distinction test; direct tension with consequence reading's functional emphasis
 *   - Secular society (observer, institutional power, analytical exit): records that the reading's logic is internal to commitment-system reasoning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.32).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.18).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Gelassenheit Separation (Consequence Reading): Technology Evaluated by Communal Impact").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/social/technological").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, '16de15e5-9c86-4f82-a53e-23429f4c4a13').
narrative_ontology:cs_kernel_codification('16de15e5-9c86-4f82-a53e-23429f4c4a13', distributed).
narrative_ontology:cs_authority_grounding('16de15e5-9c86-4f82-a53e-23429f4c4a13', lineage).
narrative_ontology:cs_interpretation_layer_present('16de15e5-9c86-4f82-a53e-23429f4c4a13').
narrative_ontology:cs_reading_relation('16de15e5-9c86-4f82-a53e-23429f4c4a13', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('16de15e5-9c86-4f82-a53e-23429f4c4a13', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('16de15e5-9c86-4f82-a53e-23429f4c4a13', foundational, community_practice_preservation_primacy).
narrative_ontology:cs_axiom_status(community_practice_preservation_primacy, holdable).
narrative_ontology:cs_axiom_grounding('16de15e5-9c86-4f82-a53e-23429f4c4a13', community_practice_preservation_primacy, deontological).
narrative_ontology:cs_axiom('16de15e5-9c86-4f82-a53e-23429f4c4a13', foundational, technology_consequentialist_evaluation).
narrative_ontology:cs_axiom_status(technology_consequentialist_evaluation, holdable).
narrative_ontology:cs_axiom_grounding('16de15e5-9c86-4f82-a53e-23429f4c4a13', technology_consequentialist_evaluation, instrumental).
narrative_ontology:cs_reference_frame('16de15e5-9c86-4f82-a53e-23429f4c4a13', anabaptist_community_practice_centered_separation).
narrative_ontology:cs_drift_state('16de15e5-9c86-4f82-a53e-23429f4c4a13', contemporary_commercial_technology_ubiquity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('16de15e5-9c86-4f82-a53e-23429f4c4a13', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, anabaptist_faith_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, community_households).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, younger_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Apply the consequence-reading test to evaluate technology: Does this practice sustain visiting, mutual aid, geographic rootedness? Telephone in barn (preserve rootedness by enabling family to stay on farm, respond to emergencies); telephone in home (erode visiting by enabling distant substitutes for presence). Tractor for belt power only (preserve farmer's connection to land); tractor for field work at speed (distance farmer from land). Communities collectively discern whether specific technologies preserve or erode the shared practices that constitute separation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, anabaptist_faith_communities, agenda_setter,
    organized, generational, identity_locked, regional).

% Navigate technology decisions through a shared reasoning framework that legitimates specific tools. Gain concrete guidance: barn telephone is acceptable, home telephone is not. Tractors are acceptable for specific power purposes, not for field work at speed. The framework is NOT a blanket prohibition—it permits reasoning about adoption. The fine-grained consequentialist structure requires households to understand the functional role each technology plays in community practices.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, community_households, beneficiary,
    moderate, biographical, identity_locked, local).

% Must inhabit the boundary between peer exposure (through church youth gatherings, secular schooling, commerce) and household technology discipline. The consequence reading requires them to articulate WHY a technology preserves or erodes community practices—a burden of reasoning that constrains adoption more subtly than categorical prohibition. They face peer pressure toward worldly adoption and must defend specific technology choices by reference to visiting patterns and mutual aid. The fine-grained framework increases their cognitive and social load.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, younger_generation, payer,
    moderate, biographical, constrained, local).

% Circulate in Anabaptist communities arguing for structural/functional-isolation test: a technology is acceptable if functionally separated from worldly systems, regardless of communal impact. They are excluded from this reading's discernment process, though they compete with the consequence reading in community debates. Their position would permit technologies the consequence reading forbids (home telephone if structurally isolated) and forbid technologies the consequence reading permits (tractor for field work if it functions in isolation).
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, principle_reading_advocates, excluded,
    moderate, generational, identity_locked, regional).

% Circulate in Anabaptist communities arguing for visible-distinction test: a technology is forbidden if it resembles worldly artifacts, regardless of function or communal consequence. They are excluded from this reading's discernment process. They would forbid both barn and home telephones, forbid tractors entirely (resembles secular equipment). Their emphasis on appearance-based distinction stands in direct tension with the consequence reading's emphasis on functional role in community practice.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, artifact_reading_advocates, excluded,
    moderate, generational, identity_locked, regional).

% Observes Anabaptist technology governance from outside. Secular frameworks treat technology as neutral tools; the consequence reading's logic (why barn telephone preserves but home telephone erodes community) is unintelligible from this seat. The observer records that the reading's reasoning is internal to commitment-system logic, not accessible to secular evaluation. Secular vendors and technologists cannot understand why the same telephone technology would be permitted in one location and forbidden in another.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, english_secular_society, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__consequence_reading, anabaptist_faith_communities).
narrative_ontology:fixing_cost_class(gelassenheit_separation__consequence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared reasoning process for technology adoption that preserves community cohesion. The consequence test legitimates specific tools by their functional role in sustaining visiting patterns, mutual aid practices, and geographic rootedness. This reading converts a boundary-maintenance problem (how to stay distinct from worldly society) into a community-practice preservation problem (what technologies sustain our lived practices together?). The coordination function is genuine: without a shared reasoning framework, individual household technology choices would fragment community practices and erode the mutual aid obligations that constitute Anabaptist separation.
% TRANSFER_FUNCTION: Transfers authority over technology decisions from individual household preference toward communal discernment grounded in consequentialist reasoning. Households gain permission to adopt specific technologies (barn telephone, tractor for belt power) that would be forbidden under artifact-reading or more restrictive principle-readings, but ONLY by justifying them through reasoning about community practices. The transfer is asymmetric: permission flows from communal reasoning, not individual choice. The younger generation carries the cost of this transfer—they must learn consequentialist reasoning about every technology and defend their choices publicly.
% ABSENT_VOICES: Secular engineers and technology designers are structurally absent—they would argue technology design is orthogonal to community practice or that telephones preserve community by enabling distant connection. Younger Anabaptists exposed to commercial technology marketing are nominally present but constrained in their ability to articulate alternatives to the consequentialist frame—they can argue for principle-reading or artifact-reading, but secular technology logic is illegitimate within the community structure. Principle-reading and artifact-reading advocates circulate within Anabaptist communities but are excluded from THIS reading's particular discernment structure.
% DISAPPEARANCE_RATIONALE: If the consequence-reading framework disappeared, Anabaptist communities would either revert to artifact-reading (categorical prohibition of all visibly worldly technology) or principle-reading (structural-isolation test permitting more technologies). The community-practice reasoning is what permits the fine-grained decisions (barn telephone allowed, home telephone forbidden, tractor for belt power allowed, tractor for field work forbidden) that currently structure Anabaptist separation. Without it, technology governance would either become stricter (no telephones anywhere) or collapse toward secular norms (all telephones acceptable).
% FOUNDING_PROBLEM: How can an Anabaptist community maintain visible separation from English secular society AND adapt to technological change in a way that does not fragment the community? The founding problem arose when industrial technology began penetrating rural Anabaptist regions (late 19th century onward): tractors, telephones, electricity, internal combustion engines. Categorical rejection of all new technology would freeze the community's material practices and create unsustainable hardship. Uncritical adoption would erode the social bonds (visiting, mutual aid, shared labor) that constitute separation. The consequence reading provides a third path: evaluate each technology by its effect on the practices that constitute the community, permitting specific tools that preserve practice while forbidding those that erode it.
% FOUNDING_PROBLEM_CORROBORATION: Anabaptist theologians and historians (Theron Schlabach, David Weaver-Zercher, scholars outside the immediate benefiting communities) document that technology governance remains the central boundary-maintenance challenge for Anabaptist communities today. Secular sociologists studying Anabaptist communities (Thomas Tweed, Marc Olshan) confirm that the consequence-reading approach (technology evaluated by communal impact) is increasingly adopted in many communities as a middle path. Elder testimony from multiple Anabaptist communities attests that telephone-in-barn / telephone-in-home distinctions are ACTIVELY NEGOTIATED and reasoned about, not merely imposed from above. Community records document the discernment processes in which these decisions are made. The founding problem is demonstrably live, actively contested, and driving real observable technology choices in Anabaptist communities today.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.32 at interval end) because the technological permissions genuinely flow from functional reasoning about community practices, not from hidden extraction. The consequence reading is a genuine rope: it solves a real coordination problem (how to adapt technology while preserving community) and the beneficiaries (the community itself) authentically benefit from the shared reasoning process. Suppression is LOW (0.18) because the framework invites reasoning and justification rather than imposing categorical prohibition—younger generation and households can engage consequentialist arguments, they are not merely silenced. Theater is very LOW (0.12) because the framework is forward-looking and outcome-focused (does this preserve community practices?) rather than performative boundary-maintenance. The measurements are nearly flat across the 50-year interval because the consequence-reading framework is structurally stable: new technologies arrive (cell phones, internet), but the reasoning framework remains the same. Extractiveness rises slightly (0.28 to 0.32) as communities negotiate harder distinctions between permitted and forbidden uses—more edge cases require more reasoning, slightly more governance overhead. Theater stays low because the reasoning remains consequentialist, not purely performative.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Anabaptist communities applying the consequence test) experiences the arrangement as genuine coordination: a shared reasoning process that legitimates specific technologies while preserving community. The younger-generation payer seat experiences increased constraint: they must learn and defend the consequentialist logic rather than simply accepting categorical rules. They face peer pressure from secular society (all technology is acceptable) and must articulate why particular technologies preserve or erode community practices—a heavier cognitive/social load than categorical prohibition would impose. The engine should compute these divergent experienced types: the community seat experiences ROPE (coordination), the younger generation seat experiences TANGLED_ROPE (coordination + constraint through forced reasoning), the principle-reading/artifact-reading excluded seats experience a different constraint (the principle or artifact reading) entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Anabaptist communities as the agenda-setter receive directionality near 0.0 (full beneficiary): they set the technology-evaluation framework, gain authority over discernment, and authentically benefit from the shared practice-preservation logic. Community households as beneficiaries receive directionality near 0.1-0.2 (beneficiary with modest costs): they gain concrete guidance and permission for specific technologies, but also inherit the governance overhead of community-level reasoning. Younger generation as payers receive directionality near 0.6-0.7 (substantial target): they face the forced reasoning, peer-pressure conflicts, and constrained choices. The excluded seats (principle and artifact advocates) are not participants in this reading's structure, so directionality does not apply to them—they experience a different constraint (the reading they advocate for). This reading exists in tension with its siblings; the constraint family (three readings, one kernel) shows how the SAME UNDERLYING PRACTICE (technology governance in Anabaptist communities) is experienced as coordination, extraction, or principle-compliance depending on which reading is active and which seat you occupy.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem is live (technology governance remains necessary), the founding problem status is live (communities actively negotiate technology adoption), and the disappearance verdict is world-rearranges (if this reading disappeared, communities would revert to stricter artifact-reading or more permissive principle-reading, changing the observable technology choices). The arrangement has not outlived its function. The theater ratio is low because the reasoning is genuine, forward-looking, and outcome-focused rather than performative boundary-maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practice_preservation_empirics,
    'Do the technological permissions the consequence reading justifies (barn telephone, tractor for belt power) actually preserve community visiting and mutual aid practices, or do they erode them despite consequentialist reasoning?',
    'Longitudinal ethnographic study of communities using the consequence reading: measure visiting frequencies, mutual aid participation, and geographic stability over 20+ years; compare to artifact-reading and principle-reading communities; compare to secular communities with unrestricted technology adoption.',
    'If the permitted technologies DO preserve practices despite enabling efficiency gains that typically reduce mutual aid, the reading''s low extractiveness is justified. If the permitted technologies gradually erode practices despite consequentialist framing, the reading is operating under a false premise—technologies are transforming community practices regardless of reasoning framework, and the low extractiveness is a misattribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_preservation_empirics, empirical, 'Whether consequentialist reasoning about technology actually predicts or preserves community practices.').

omega_variable(
    reasoning_vs_performative_suppression,
    'Is the suppression experienced by younger generation members (forced to articulate consequentialist reasoning about every technology choice) genuinely lower than categorical prohibition, or is it internalized suppression under the guise of reasoning freedom?',
    'Post-exit testimony from Anabaptist-raised individuals who leave the community: do they report the consequence-reading reasoning as liberatory (genuine freedom to choose tools justified by function) or as coercive reasoning under a different name (you must perform logical justification for every choice or be excluded)? Compare to reports from artifact-reading communities about categorical prohibition.',
    'If the reasoning is genuinely experienced as liberatory, the low suppression metric (0.18) is justified. If reasoning is experienced as internalized coercion, the suppression metric is underestimated—the framework has converted external prohibition into self-discipline, and post-exit testimony would show suppression persists after exit (internalized mechanism, not structural barrier).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reasoning_vs_performative_suppression, empirical, 'Whether forced consequentialist reasoning is liberatory or internalized coercion.').

omega_variable(
    reading_stability_contestation,
    'Is the consequence reading a stable commitment-system position maintained by genuine communal discernment, or a contested temporary equilibrium between artifact-reading strictness and principle-reading permissiveness?',
    'Multi-generational tracking of reading adoption in Anabaptist communities: do communities stabilize on the consequence reading, or do they oscillate between readings as new technologies arrive? Do younger-generation advocates for the consequence reading sustain it when they gain elder status, or do they shift to artifact-reading (boundary maintenance) or principle-reading (functional isolation)?',
    'If stable across generations and new technologies, the low extractiveness is justified—the reading is a genuine stable coordination framework. If oscillating or unstable, the reading is a temporary equilibrium that breaks under pressure, and the low extractiveness underestimates the fragility of the commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability_contestation, conceptual, 'Whether the consequence reading is a stable commitment or contested temporary equilibrium.').

omega_variable(
    family_reading_coexistence_framing,
    'The three readings of gelassenheit_separation (artifact, principle, consequence) coexist across different Anabaptist communities and regions. Is this coexistence structurally compatible (different communities genuinely enact different readings), or is it an artifact of incomplete assimilation to secular technology logic (different communities are at different stages of the same trajectory toward secular adoption)?',
    'Comparative analysis of communities: do artifact-reading communities show resistance to principle and consequence readings as doctrinally false? Do principle-reading communities see artifact-reading as overly rigid and consequence-reading as insufficiently principled? Or do all three readings coexist in friendly disagreement? Track communities 50+ years: do they maintain distinct readings or converge?',
    'If coexistence is structurally compatible (different but equally valid readings), the three constraints are genuinely different options with different epsilon values. If coexistence is incomplete assimilation (staged progression toward secular norms), then the readings are not stable positions—they represent steps on a trajectory, and the low extractiveness of consequence-reading is partly obscured trajectory-capture (the reading seems low-extractive because it is early-stage assimilation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(family_reading_coexistence_framing, conceptual, 'Whether the three readings coexist as stable distinct positions or represent stages of assimilation to secular technology logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__consequence_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__consequence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__consequence_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__consequence_reading, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__consequence_reading, base_extractiveness, 10, 0.29).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__consequence_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__consequence_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__consequence_reading, base_extractiveness, 50, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__consequence_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__consequence_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__consequence_reading, suppression_requirement, 30, 0.17).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__consequence_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__consequence_reading, suppression_requirement, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__consequence_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% The kernel 'gelassenheit_separation' decomposes into three structurally distinct constraint stories, each instantiating a different reading. The consequence reading emphasizes preservation of community practices (visiting, mutual aid, rootedness) and evaluates technology by functional role. The artifact reading emphasizes visible distinction from English society and forbids technology by categorical resemblance. The principle reading emphasizes structural isolation from worldly systems and accepts technology by functional isolation. Each reading has different epsilon values, different beneficiary structures, and different directionalities for the same stakeholders. The three constraints form a constraint family linked by network.affects_constraints, documenting the theological contest over separation's meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
