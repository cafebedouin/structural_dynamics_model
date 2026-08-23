% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Index (Objective Index Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists
 *   since 1947, is widely treated as an objective index of existential risk —
 *   a 'speedometer' for civilization's proximity to catastrophe. This reading
 *   (objective_index_reading) takes that self-presentation at face value: the
 *   Clock setting tracks measurable existential risk levels through expert
 *   synthesis of empirical indicators. The constraint is the epistemic
 *   arrangement that designates a closed expert committee's annual judgment
 *   as the authoritative metric of global existential risk, suppressing the
 *   normative choices (weighting, prioritization, value trade-offs) inherent
 *   in any such synthesis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.22).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.45).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, mountain).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Index (Objective Index Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:emerges_naturally(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '81fa09ae-5ede-40f0-a995-a00e124d7024').
narrative_ontology:cs_kernel_codification('81fa09ae-5ede-40f0-a995-a00e124d7024', implicit).
narrative_ontology:cs_authority_grounding('81fa09ae-5ede-40f0-a995-a00e124d7024', expertise).
narrative_ontology:cs_interpretation_layer_present('81fa09ae-5ede-40f0-a995-a00e124d7024').
narrative_ontology:cs_reading_relation('81fa09ae-5ede-40f0-a995-a00e124d7024', doomsday_clock_metric__performative_tool_reading, forecloses).
narrative_ontology:cs_reading_relation('81fa09ae-5ede-40f0-a995-a00e124d7024', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('81fa09ae-5ede-40f0-a995-a00e124d7024', foundational, existential_risk_is_commensurably_measurable).
narrative_ontology:cs_axiom_status(existential_risk_is_commensurably_measurable, holdable).
narrative_ontology:cs_axiom_grounding('81fa09ae-5ede-40f0-a995-a00e124d7024', existential_risk_is_commensurably_measurable, empirically_contingent).
narrative_ontology:cs_axiom('81fa09ae-5ede-40f0-a995-a00e124d7024', foundational, expert_synthesis_grounds_legitimate_authority).
narrative_ontology:cs_axiom_status(expert_synthesis_grounds_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('81fa09ae-5ede-40f0-a995-a00e124d7024', expert_synthesis_grounds_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('81fa09ae-5ede-40f0-a995-a00e124d7024', manhattan_project_scientist_communication).
narrative_ontology:cs_drift_state('81fa09ae-5ede-40f0-a995-a00e124d7024', contemporary_multi_risk_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('81fa09ae-5ede-40f0-a995-a00e124d7024', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, bulletin_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_synthesis_committee).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, institutional_science_authority).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_publics).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, policy_actors_outside_expert_consensus).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, civil_society_interpreters).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, existential_risk_is_measurable).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, expert_synthesis_yields_authoritative_judgment).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, scientific_authority_grounds_legitimate_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Bulletin of the Atomic Scientists' Science and Security Board sets the Clock's time through a deliberative process among recognized experts. They frame the setting as a synthesis of empirical indicators (nuclear arsenals, carbon emissions, biosafety incidents, disruptive technology milestones). Their authority derives from scientific credentials and institutional continuity since 1947. They benefit from the Clock's status as the authoritative metric of existential risk, which reinforces the epistemic privilege of expert synthesis in global governance.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_scientists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, bulletin_scientists, beneficiary).

% The rotating committee of scientists and security experts who participate in the Clock-setting process. Their participation confers professional recognition and positions them as legitimate interpreters of existential risk. They benefit from the institutionalized monopoly on translating complex risk landscapes into a single authoritative signal, which shapes funding priorities, policy agendas, and public discourse.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, expert_synthesis_committee, beneficiary,
    organized, biographical, mobile, global).

% The broader complex of scientific academies, intergovernmental panels (IPCC, IAEA), and expert advisory bodies whose governance model the Clock exemplifies. The Clock's presentation as an objective index validates the claim that existential risk assessment is properly a technocratic-expert function rather than a democratic-deliberative one. This reinforces their structural role in global risk governance.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, institutional_science_authority, beneficiary,
    institutional, civilizational, analytical, global).

% Citizens and civil society actors who bear the consequences of existential risk policy but are structurally excluded from the Clock-setting process. The Clock's single-number output (e.g., '90 seconds to midnight') enters public discourse as a fact-like claim, narrowing the space for contesting the normative judgments embedded in risk prioritization (e.g., weighting nuclear vs. climate vs. AI risk). Their exit is constrained: they can reject the Clock's authority but lack alternative authoritative metrics, and policy makers treat the Clock as a settled input.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_publics, payer,
    organized, generational, constrained, global).

% Government officials, legislators, and diplomatic actors whose policy preferences diverge from the expert consensus embodied in the Clock setting. They must either adopt the Clock's framing (ceding agenda-setting to expert synthesis) or expend significant political capital to contest it. The Clock's epistemic authority makes dissent appear 'anti-science' rather than a legitimate normative disagreement about risk tolerance, value trade-offs, or distributive justice.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_actors_outside_expert_consensus, payer,
    powerful, biographical, constrained, national).

% NGOs, journalists, advocacy groups, and independent researchers who translate existential risk for publics. They are pressured to treat the Clock setting as a fixed reference point rather than a contestable interpretation. Those who attempt alternative risk framings (e.g., justice-centered, indigenous knowledge, degrowth perspectives) face marginalization because the Clock occupies the 'authoritative metric' niche. Their exit is constrained by the Clock's media dominance and institutional uptake.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, civil_society_interpreters, payer,
    moderate, biographical, constrained, global).

% Academics and analysts who develop alternative existential risk frameworks (e.g., participatory risk assessment, pluralistic indicator dashboards, justice-weighted metrics). They are excluded from the Clock-setting process and their frameworks receive far less policy uptake. They would object to the Clock's claim to objective-index status but lack the institutional platform to challenge it effectively.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, independent_risk_scholars, excluded,
    moderate, biographical, mobile, global).

% Philosophers of science, STS scholars, and epistemologists who study how the Clock functions as a boundary object between expert knowledge and democratic governance. They analyze the Clock's claim to objectivity as a performed epistemic virtue that stabilizes expert authority. They neither collect nor pay the constraint's extraction; they describe its structural operation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, meta_science_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, recognizable, annually updated signal that compresses multidimensional existential risk data into a format usable by media, policymakers, and publics — solving the coordination problem of 'what is the current state of existential risk?' without requiring each actor to synthesize the evidence themselves.
% TRANSFER_FUNCTION: Moves epistemic authority and agenda-setting power from democratic-deliberative processes (where publics and their representatives would contest risk priorities, value trade-offs, and distributive consequences) to a closed expert synthesis process. The Clock setting becomes a de facto input to policy that bypasses standard democratic contestation channels.
% ABSENT_VOICES: Communities most exposed to existential risks (nuclear frontline communities, climate-vulnerable populations, Global South nations) are absent from the Clock-setting room. Their risk perceptions, value priorities, and knowledge traditions are not represented in the expert synthesis. Future generations — the ultimate stakeholders of existential risk — are structurally excluded by definition. Indigenous knowledge holders and alternative epistemologies of planetary stewardship are not consulted.
% DISAPPEARANCE_RATIONALE: If the Clock vanished overnight, the authoritative single-metric reference point for existential risk would disappear. Media would lose their primary 'state of the world' hook. Policymakers would lose a legitimizing citation for expert-driven agendas. Alternative risk frameworks (pluralistic dashboards, justice-weighted metrics, participatory assessments) would compete for the vacant niche. The expert monopoly on existential risk interpretation would fracture, opening space for democratic contestation of risk priorities — but also creating fragmentation and potential polarization.
% FOUNDING_PROBLEM: After the atomic bombings of Hiroshima and Nagasaki, Manhattan Project scientists confronted the unprecedented problem: how to communicate the magnitude of nuclear danger to a public and political class that lacked the technical vocabulary to grasp it. The Clock was built to translate 'we have created a technology that can end civilization' into a visceral, immediately intelligible image — the minutes-to-midnight metaphor.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin's founding scientists (Eugene Rabinowitch, Hyman Goldsmith, Martyl Langsdorf) attested the founding problem in contemporary accounts — the Clock was explicitly a communication tool for a specific historical moment (nuclear monopoly, pre-arms control). Independent historians of science (e.g., Alex Wellerstein, Sarah Kruse) corroborate that the founding problem was nuclear-specific communication, not a general existential risk index. The current Board attests the problem is still live (existential risk has multiplied), but this is self-asserted by the benefiting party. No external corroboration establishes that the original communication problem maps onto the current multi-risk synthesis mandate.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, ExtMetricName, E),
    domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(doomsday_clock_metric__objective_index_reading),
    narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.22) is low in absolute terms — the Clock does not extract material resources. But it extracts epistemic authority: it concentrates the legitimate interpretation of existential risk in a non-accountable expert body. Suppression (0.45) is moderate — the Clock does not legally forbid alternative metrics, but its institutional uptake and media dominance create a soft monopoly that makes alternatives structurally marginal. Theater ratio (0.18) reflects that the expert deliberation is genuine but increasingly performs objectivity while embedding normative judgments (e.g., treating nuclear and climate risk as commensurable on a single scale). Accessibility collapse (0.55) is moderate: the Clock's simplicity makes alternatives harder to communicate, but pluralistic dashboards exist. Resistance (0.35) is present but diffuse — critics challenge specific settings but rarely the Clock's authority as such.
 *
 * PERSPECTIVAL GAP:
 *   From the expert seat (agenda_setter/beneficiary), the Clock is a mountain — a genuine synthesis of empirical indicators that would exist in some form regardless of who administers it, because existential risk is objectively real and measurably trackable. From the payer seats (democratic publics, policy outsiders), the Clock operates as a snare — the normative framing is suppressed, the expert monopoly is enforced through soft power, and exit is constrained by the lack of alternative authoritative metrics. The engine computes this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin scientists and expert committee are structural beneficiaries (d ~ 0.15): they collect epistemic authority, media access, and policy influence from the Clock's authoritative status. Their exit is arbitrage/mobile — they could leave the institution but the institution's brand follows them. Democratic publics, policy outsiders, and civil society interpreters are payers (d ~ 0.75-0.85): they bear the cost of narrowed deliberative space and have constrained exit (no alternative authoritative metric, policy uptake locks in the Clock). Institutional science authority is a beneficiary at civilizational horizon (d ~ 0.1): the Clock validates the technocratic governance model. Independent risk scholars are excluded (trapped by structural marginalization). Meta-science observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (nuclear danger communication) is historically specific; the current mandate (multi-risk synthesis index) is a scope expansion without re-authorization. The Clock's authority now rests on the claim that expert synthesis can produce an objective index across incommensurable risk domains — a claim contested by the performative_tool_reading and hybrid_legitimacy_reading. Mandatrophy is unresolved: the arrangement persists because it solves a coordination problem for media and policymakers (a single number), not because its epistemic claim is validated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectivity_vs_normative_embedding,
    'Does the expert synthesis process actually produce an objective index, or does it necessarily embed normative judgments (risk weighting, value trade-offs, commensurability assumptions) that the ''objective index'' framing suppresses?',
    'Formal analysis of the Clock-setting methodology: whether the aggregation of heterogeneous indicators (nuclear warheads, CO2 ppm, AI compute milestones, biosafety incidents) into a single ordinal scale involves incommensurable value choices that no empirical evidence can resolve. Compare with pluralistic indicator approaches that make normative weights explicit.',
    'If the synthesis necessarily embeds normative judgments, the objective_index_reading''s claimed_type (mountain) is a false summit — the constraint is a constructed epistemic arrangement benefiting expert authority (tangled_rope or snare). If the synthesis can be purely empirical, the mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectivity_vs_normative_embedding, conceptual, 'Whether the Clock''s single-number output can be a genuine natural-law-like index or is inherently a normative construction.').

omega_variable(
    democratic_legitimacy_deficit,
    'Can an expert committee''s annual judgment legitimately function as the authoritative metric for existential risk governance in democratic societies, or does this constitute an unacknowledged transfer of sovereign risk-acceptance decisions to a non-accountable body?',
    'Democratic theory analysis: whether the Clock''s de facto policy uptake (cited in UN speeches, national security strategies, climate negotiations) constitutes a structural bypass of democratic deliberation on risk tolerance and intergenerational justice. Empirical study of policy citation patterns and whether legislatures debate Clock settings or treat them as settled inputs.',
    'If the Clock functions as a democratic legitimacy bypass, the extraction is political (epistemic capture of sovereign risk decisions) not just epistemic. This would elevate the constraint from mountain/rope toward tangled_rope/snare depending on enforcement dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_legitimacy_deficit, conceptual, 'Whether the Clock''s epistemic authority transfers into unauthorized political authority over existential risk priorities.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative risk framings structural (institutional uptake, media dominance, policy lock-in) or internalized (civil society actors self-censor because the Clock frames the legitimate debate space)?',
    'Post-exit trajectory study: track whether actors who reject the Clock''s authority (develop alternative metrics, refuse to cite it) face material exclusion (funding denial, policy irrelevance) or merely discursive marginalization. Survey civil society risk communicators on perceived pressure to reference the Clock.',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after exit. This would increase effective extraction for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative existential risk framings.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the objective_index_reading''s core premise (existential risk is measurably trackable by expert synthesis into a single objective index) logically foreclose the performative_tool_reading''s core premise (settings are strategically chosen for policy impact), or do they coexist as descriptions of different layers of the same practice?',
    'Internal documentary analysis: if Bulletin deliberation records show strategic consideration of policy impact driving settings, the objective reading is falsified as a complete description (though the synthesis may still be empirically grounded). If records show pure empirical deliberation, the performative reading is a hermeneutic overlay.',
    'If forecloses: the readings cannot both be true in any single framework — the kernel admits no stable hybrid. If coexists_with: the kernel sustains an irreducible interpretive contest (hybrid_legitimacy_reading''s claim).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between the objective_index_reading and performative_tool_reading of the doomsday_clock_metric kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__objective_index_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(doom_tr_t1953, doomsday_clock_metric__objective_index_reading, theater_ratio, 1953, 0.05).
narrative_ontology:measurement(doom_tr_t1963, doomsday_clock_metric__objective_index_reading, theater_ratio, 1963, 0.05).
narrative_ontology:measurement(doom_tr_t1984, doomsday_clock_metric__objective_index_reading, theater_ratio, 1984, 0.08).
narrative_ontology:measurement(doom_tr_t1991, doomsday_clock_metric__objective_index_reading, theater_ratio, 1991, 0.07).
narrative_ontology:measurement(doom_tr_t2007, doomsday_clock_metric__objective_index_reading, theater_ratio, 2007, 0.12).
narrative_ontology:measurement(doom_tr_t2015, doomsday_clock_metric__objective_index_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(doom_tr_t2020, doomsday_clock_metric__objective_index_reading, theater_ratio, 2020, 0.17).
narrative_ontology:measurement(doom_tr_t2023, doomsday_clock_metric__objective_index_reading, theater_ratio, 2023, 0.18).
narrative_ontology:measurement(doom_tr_t2025, doomsday_clock_metric__objective_index_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1947, 0.08).
narrative_ontology:measurement(doom_be_t1953, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1953, 0.1).
narrative_ontology:measurement(doom_be_t1963, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1963, 0.09).
narrative_ontology:measurement(doom_be_t1984, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1984, 0.12).
narrative_ontology:measurement(doom_be_t1991, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1991, 0.1).
narrative_ontology:measurement(doom_be_t2007, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2007, 0.18).
narrative_ontology:measurement(doom_be_t2015, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2015, 0.2).
narrative_ontology:measurement(doom_be_t2020, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2020, 0.22).
narrative_ontology:measurement(doom_be_t2023, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2023, 0.22).
narrative_ontology:measurement(doom_be_t2025, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1947, 0.15).
narrative_ontology:measurement(doom_su_t1953, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1953, 0.18).
narrative_ontology:measurement(doom_su_t1963, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1963, 0.15).
narrative_ontology:measurement(doom_su_t1984, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1984, 0.25).
narrative_ontology:measurement(doom_su_t1991, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1991, 0.22).
narrative_ontology:measurement(doom_su_t2007, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2007, 0.35).
narrative_ontology:measurement(doom_su_t2015, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(doom_su_t2020, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2020, 0.43).
narrative_ontology:measurement(doom_su_t2023, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2023, 0.45).
narrative_ontology:measurement(doom_su_t2025, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.02).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, global_existential_risk_governance_architecture).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, ipcc_assessment_cycle).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, nuclear_arms_control_regime).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel decomposes into three constraint stories: objective_index_reading (this file, claimed mountain, low extractiveness), performative_tool_reading (claimed snare, high extractiveness, strategic communication), hybrid_legitimacy_reading (claimed tangled_rope, moderate extractiveness, irreducible entanglement). Their ε values differ because they measure different structural arrangements: the expert synthesis process itself (this reading), the strategic communication function (performative), the entangled legitimation structure (hybrid). Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, institutional, 0.1).
constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, organized, 0.8).
constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, powerful, 0.75).
constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, moderate, 0.85).
constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
