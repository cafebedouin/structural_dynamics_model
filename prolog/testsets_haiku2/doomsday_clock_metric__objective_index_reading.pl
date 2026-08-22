% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Risk Index (Expert Authority Reading)
 *   domain: science_communication/risk_governance/epistemology
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists
 *   since 1947, displays an annual judgment about humanity's proximity to
 *   civilization-ending catastrophe. The constraint story examined here is
 *   the OBJECTIVE INDEX READING: the reading that the Clock's minute position
 *   is a direct measurement of existential risk levels, synthesized from
 *   empirical indicators across nuclear weapons, climate, biotechnology, and
 *   other domains. Under this reading, the Clock is a scientific instrument
 *   whose authority derives from the expert community's technical assessment
 *   of measurable threats. This reading suppresses the normative and
 *   communicative dimensions of the Clock's social function — what it does
 *   politically and emotionally — in favor of treating it as pure information
 *   transmission. The reading is characterized by high suppression of
 *   contestation and high extraction from democratic accountability: expert
 *   authority is elevated above challenge; alternative risk framings are
 *   excluded; public deliberation is channeled through expert interpretation.
 *
 * KEY AGENTS:
 *   - Bulletin of the Atomic Scientists: institutional agenda-setter, controls annual Clock setting and official interpretation; beneficiary of the Clock's authority
 *   - Expert epistemic community (physicists, climate scientists, biosecurity specialists): institutional beneficiary; authority vindicated and concentrated through the Clock
 *   - Democratic publics and policy communities: structurally payer; anxious about existential risk but without voice or access to expert reasoning
 *   - Dissident scientists and alternative risk ontologies: excluded victim; voice suppressed by expert consensus structure; identity-locked as scientists (challenging the Clock means challenging science authority itself)
 *   - Media and public intellectuals: observational seat; translate expert synthesis into public narrative, amplifying Clock authority
 *   - Alternative frameworks (Indigenous, postcolonial, value-pluralist): structurally excluded; cannot compete without adopting Clock's implicit empiricist epistemology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.68).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.79).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Risk Index (Expert Authority Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/risk_governance/epistemology").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, 'dad531c0-1fc2-444c-bae5-8038407ad91a').
narrative_ontology:cs_kernel_codification('dad531c0-1fc2-444c-bae5-8038407ad91a', formalized).
narrative_ontology:cs_authority_grounding('dad531c0-1fc2-444c-bae5-8038407ad91a', extraction).
narrative_ontology:cs_interpretation_layer_present('dad531c0-1fc2-444c-bae5-8038407ad91a').
narrative_ontology:cs_reading_relation('dad531c0-1fc2-444c-bae5-8038407ad91a', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_reading_relation('dad531c0-1fc2-444c-bae5-8038407ad91a', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('dad531c0-1fc2-444c-bae5-8038407ad91a', foundational, existential_risk_measurable_scalar_index).
narrative_ontology:cs_axiom_status(existential_risk_measurable_scalar_index, holdable).
narrative_ontology:cs_axiom_grounding('dad531c0-1fc2-444c-bae5-8038407ad91a', existential_risk_measurable_scalar_index, empirically_contingent).
narrative_ontology:cs_axiom('dad531c0-1fc2-444c-bae5-8038407ad91a', secondary, expert_authority_transparency_sufficient).
narrative_ontology:cs_axiom_status(expert_authority_transparency_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('dad531c0-1fc2-444c-bae5-8038407ad91a', expert_authority_transparency_sufficient, instrumental).
narrative_ontology:cs_reference_frame('dad531c0-1fc2-444c-bae5-8038407ad91a', expert_consensus_risk_index).
narrative_ontology:cs_drift_state('dad531c0-1fc2-444c-bae5-8038407ad91a', ai_integration_era_2015_2025, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dad531c0-1fc2-444c-bae5-8038407ad91a', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, bulletin_of_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_authority_in_risk_assessment).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_publics).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, policy_actors_without_access_to_reasoning).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, dissident_scientific_voices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the Clock's minute position annually via a convened panel of expert judges. Controls the interpretation framework: publishes the reasoning in narrow technical language accessible primarily to credentialed specialists. Frames the Clock as a direct index of objective existential risk, translating threat assessment into a single scalar metric. Benefits from the Clock's authority as a science communication tool and its legitimacy as a neutral risk gauge.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, arbitrage, global).

% The broader epistemic authority structure that produces the Clock: nuclear physicists, climate scientists, biosecurity specialists, and geopolitical analysts whose consensus judgments are synthesized into the clock setting. Their expert standing is vindicated by the Clock's public presence and media elevation. Their control over the interpretation of existential risk is crystallized in the artifact.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, expert_authority_in_risk_assessment, beneficiary,
    institutional, generational, mobile, global).

% Receive the Clock's annual setting as a normative anchor for their own sense of civilizational threat. They cannot access the expert reasoning in its technical form and must either accept the clock position as read or engage through mass media interpretations. The suppression mechanism operates here: they bear the existential anxiety the Clock generates, have no direct voice in its setting, and cannot demand transparency or accountability in the synthesis process without challenging scientific authority itself.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_publics, payer,
    powerless, biographical, constrained, global).

% National government officials, military strategists, and policy makers who use the Clock as a legitimizing signal for funding and priority-setting (e.g., nuclear preparedness budgets) but who do not participate in the Clock's reasoning and cannot audit or challenge the expert synthesis without appearing to dismiss scientific authority. They pay the cost of treating the Clock position as dispositive rather than advisory.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_actors_without_access_to_reasoning, payer,
    moderate, biographical, constrained, national).

% Scientists who disagree with the Clock panel's risk assessment (e.g., those who think biotech threats are overweighted or that certain risks are overrated relative to their empirical basis) but who find their dissent systematically excluded from public framing because the Clock operates as a consensus instrument. They bear the cost of expert monopoly on existential risk discourse: challenging the Clock means challenging the very authority structure that permits them to speak as scientists.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, dissident_scientific_voices, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, dissident_scientific_voices, excluded).

% Translate the Clock's expert setting into narrative for mass audiences. They operate at the boundary between expert synthesis and public meaning-making, amplifying the Clock's authority while simultaneously smoothing its technical reasoning into emotional and moral language. Their role is observational from the constraint's internal structure but consequential for its social function.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, media_and_public_intellectuals, observer,
    powerful, biographical, mobile, global).

% Non-expert, value-pluralist, or indigenous framings of existential and civilizational risk (e.g., Indigenous sovereignty frameworks, postcolonial risk ontologies, feminist care ethics approaches) that would name different threats or prioritize differently. They are structurally excluded from the Clock's expert panel and cannot compete as legitimate risk assessments without first adopting the Clock's implicit empiricist epistemology and the authority structure it presupposes.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, alternative_risk_frameworks, excluded,
    moderate, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, bulletin_of_atomic_scientists).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates fragmented expert judgments about multiple complex, global existential threats (nuclear war, climate, biotechnology, artificial intelligence, ecological collapse) into a single visible metric that permits societies to calibrate collective attention and resource allocation to civilizational risk.
% TRANSFER_FUNCTION: Transfers interpretive authority over existential risk assessment from democratic deliberation and contested values to an expert panel, whose synthesis is presented as objective fact rather than constructed judgment. Authority flows from the public upward to the panel; accountability flows downward from the panel to the public as a delivered position, not open to challenge.
% ABSENT_VOICES: Democratic publics have no formal voice in the Clock's setting; dissident scientists and alternative ontologies of risk are excluded by the consensus-panel structure; policy communities that must act on the Clock's signals do not participate in generating them; non-expert publics' fear and stake in existential outcomes are absent from the reasoning that produces the metric; Indigenous, postcolonial, and value-pluralist risk frameworks are categorically excluded as non-technical.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock's authority vanished overnight, existential risk discourse would fragment into competing expert and non-expert frameworks; policy-makers would lose a centralized legitimizing signal for funding; the public would experience existential anxiety without the anchoring artifact; expert authority in risk assessment would no longer have the singular public megaphone the Clock provides. Multiple governance structures and attention-allocation systems would emerge to fill the gap, likely incorporating democratic and pluralist voices currently suppressed.
% FOUNDING_PROBLEM: In the nuclear age and its successor crises (climate, biotech, AI), humanity faces multiple global catastrophic risks whose assessment requires synthesis of knowledge from dozens of disciplines and whose governance requires coordination on civilizational priority-setting. No democratic institution solved this problem; no market signal tracks existential risk. The Clock was created to provide a centralized authoritative index so that technical assessment could inform (and stabilize) public and policy attention.
% FOUNDING_PROBLEM_CORROBORATION: The Clock panel and institutional authority structures assert the problem remains live: new categories of existential risk (AI, engineered pandemics) continuously emerge and require expert synthesis. Critics including social scientists, democratic theorists, and dissident scientists assert the founding problem has been solved by the Clock's creation itself—the artifact now generates a dependency on expert authority that exceeds the problem it was meant to address. Independent corroboration: academic critiques of expert governance in science communication (Jasanoff, Wynne, Lehr, Sismondo); policy analysis showing Clock-signal uptake in funding allocation without corresponding transparency in methodology; sociological studies of public existential anxiety tied to Clock movements; historians documenting that the Clock was originally a communicative tool, not an empirical instrument.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction measures 0.68 at interval end, rising from 0.42 at start. The constraint transfers interpretive authority from public to expert, with no reverse accountability. Suppression measures 0.79 and is high throughout because the constraint's persistence depends on excluding challenge to expert authority — dissident scientists cannot speak publicly without undermining the scientific authority they rely on for standing; democratic publics cannot audit reasoning they cannot access without appearing to dismiss science. Theater ratio rises from 0.18 to 0.41 over the interval: the Clock's media presence and emotional weight (as a symbol of existential threat) grows while the underlying empirical synthesis remains opaque to non-experts. Accessibility collapse is moderate (0.62) because alternatives to expert-authority risk framing do exist (value-pluralist, feminist, postcolonial approaches) but are systematically excluded by the Clock's consensus structure. Resistance is moderate (0.58) because some scientific dissent exists (e.g., critiques of biotech risk overweighting) but is muted by the cost of publicly challenging expert consensus. The measurement series shows monotonic increase in extractiveness and theater ratio, with suppression rising sharply through year 35 (corresponding to the AI-risk integration era, 2015-2018) and then plateauing—reflecting the constraint's structural stabilization once AI was incorporated into the Clock's scope.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Bulletin) experiences this arrangement as genuine coordination — bringing fragmented expert judgment into a public-facing signal that guides collective attention. From the institutional expert seat, the Clock is a success: authority is concentrated, message is clear, policy impact is documented. From the payer seats (democratic publics and policy communities), the same arrangement operates as enforced extraction: they receive a dictated risk assessment without recourse, must treat it as authoritative despite not understanding its reasoning, and experience suppression of their own stake-based risk frameworks. From the dissident-scientist seat, the constraint is particularly extractive because it enforces their silence — the cost of scientific standing is accepting the Clock's consensus, making dissent professionally costly (identity-locked exit). The engine computes each seat's type from the structural data; the measurement of per-seat divergence is the exercise.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin and expert authority sit at low directionality (beneficiaries, d ≈ 0.1-0.2): they collect authority and legitimacy from the Clock's operation, have mobile exit (can revise the Clock, disband, or reframe), and operate at institutional power where alternatives are available (other epistemic institutions could compete for authority). Democratic publics sit at high directionality (targets, d ≈ 0.8-0.9): they are structurally powerless, identity-locked as citizens whose existential concern is weaponized into anxiety, and face collapsed alternatives (the Clock's authority is now the default frame for thinking about existential risk). Dissident scientists sit at the highest directionality (d ≈ 0.85-0.95): they are identity-locked as scientists (challenging the Clock means professional exile), face constrained exit (can leave science or accept silence), and operate at moderate power where the larger institutional authority can isolate them. Policy communities sit mid-high (d ≈ 0.65-0.75): they have institutional power to resist the Clock's signal, but their legitimacy depends on aligning with expert authority, making resistance costly. No directionality overrides are required; the structural data produces the right gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic tangled-rope form: genuine coordination (aggregating expert judgment on complex, global, multi-disciplinary risks) coupled with asymmetric extraction (transfer of interpretive authority from democratic deliberation to expert panel, with high suppression preventing challenge). The founding problem—how to coordinate technical assessment of existential risk without institutional capacity in democratic governance—remains contested and arguably partially solved (the Clock itself generates the expertise infrastructure it purports to serve). The measurement series show theater ratio rising faster than extractiveness plateaus (years 50-75), suggesting the constraint's function is increasingly communicative and symbolic rather than empirically-grounded technical synthesis. This is the Goodhart signal: the Clock optimizes for media attention and public alarm rather than for accuracy of risk assessment. The suppression mechanism is the operative structural enforcement: dissident voices are suppressed not by explicit exclusion (the panel is ostensibly open) but by the consensus structure's cost to dissent. The constraint qualifies as tangled rope: it coordinates and it extracts through the same mechanism (expert authority), and its persistence requires active enforcement (renewal of the panel, media amplification, policy uptake).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_adequacy_of_synthesis,
    'Does the Clock panel''s annual synthesis of risk indicators across nuclear, climate, biotech, and AI domains rest on defensible empirical aggregation, or does the reduction to a single scalar reflect unavoidable value choices that should be made democratically rather than by expert fiat?',
    'Systematic comparison of the Clock''s methodological transparency across time periods; audit of how new threat categories (AI, engineered pandemics) were incorporated and weighted relative to nuclear risks; solicitation of dissenting expert assessments and analysis of what criteria exclude them.',
    'If aggregation is empirically defensible, the objective-index reading holds; the high suppression reflects the legitimate cost of technical expertise. If aggregation involves irreducible value choices, the constraint reclassifies toward snare: the normative content is suppressed, falsely naturalized as objective fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_adequacy_of_synthesis, empirical, 'Whether the Clock''s risk synthesis is empirically defensible or value-laden aggregation presented as objectivity.').

omega_variable(
    identity_lock_mechanism_in_dissident_scientists,
    'Is the suppression of dissident expert voices (e.g., scientists who think biotech risks are overweighted or AI risk inflated) a structural consequence of the Clock''s authority, or do dissident scientists have available exit without career cost?',
    'Empirical study: track career trajectories of scientists who publicly dissent from Clock positions; compare publication, funding, and institutional acceptance before and after dissent; interview dissident scientists about exit costs they perceive.',
    'If exit is genuinely costly and identity-locked (scientific standing requires accepting consensus), suppression is structurally high and enforced through professional mechanisms. The constraint approaches pure snare at the dissident-scientist seat. If dissent is costly but professionally survivable, the suppression is moderate and the constraint remains tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_dissident_scientists, empirical, 'Whether dissident scientists face identity-locked suppression or merely social friction from consensus disagreement.').

omega_variable(
    democratic_accountability_vs_expertise,
    'Can a publicly-legitimized existential-risk index (the Clock) remain under expert authority without sacrificing democratic accountability for high-stakes civilizational decisions, or is the expert-authority reading necessarily foreclosing to democratic deliberation?',
    'Institutional innovation test: can a reformed Clock operate under multi-stakeholder governance (expert + policy + public representatives) while retaining technical credibility? Case study: comparison of governance structures that have integrated expert and democratic inputs in other high-stakes domains (pandemic response, climate policy).',
    'If accountability is structurally separable from expertise, reform is possible and suppression could be engineered down without sacrificing coordination function—tangled rope could rebalance toward rope. If they are inextricably coupled (expertise requires authority, authority requires insulation from contestation), the reading foreclosures the hybrid-legitimacy and performative-tool siblings—the objective index reading is the only internally coherent position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_accountability_vs_expertise, conceptual, 'Whether expert authority and democratic accountability are compatible in existential-risk governance or structurally opposed.').

omega_variable(
    reading_incoherence_under_scrutiny,
    'Does the objective-index reading cohere under scrutiny, or does close examination of the Clock''s actual functioning (media amplification, policy uptake patterns, theater ratio over time) reveal that the reading''s core axiom (empirical objectivity) is falsified by the constraint''s own observed behavior?',
    'Time-series analysis of Clock announcements: compare the technical reasoning (published by the panel) to media coverage and policy response; measure alignment between expert judgment changes and empirical threat developments; assess whether Clock movements lag or lead threats.',
    'If coherence holds—expert reasoning aligns with empirical data and policy responses are proportionate—the objective-index reading stands. If coherence breaks—Clock movements show strategic timing, media amplification distorts the signal, policy responses are disproportionate—the reading self-refutes: the Clock is performative-tool masquerading as objective index, and the objective-index reading foreclosures to the performative-tool reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incoherence_under_scrutiny, empirical, 'Whether the objective-index reading''s self-presentation as empirical fact survives scrutiny of its actual social functioning.').

omega_variable(
    committer_alternative_reading_suppression,
    'Is the suppression of alternative risk readings (Indigenous, postcolonial, feminist, value-pluralist frameworks) a side effect of the Clock''s technical focus, or is it structural to the objective-index reading''s claim to epistemic authority?',
    'Comparative analysis: document how alternative frameworks would assess existential risk (timeline, threat categories, priorities); measure whether incorporation of alternative frameworks would require revising the Clock''s core metrics or would instead reveal hidden value choices the objective-index reading naturalizes.',
    'If alternative frameworks are simply non-technical and thus legitimately excluded, suppression is a feature of expert specialization, not extraction. If alternative frameworks expose the normative content the objective-index reading suppresses, they are structurally excluded victims—the constraint is snare-like at the alternative-voice seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_alternative_reading_suppression, conceptual, 'Whether exclusion of alternative risk ontologies is technically justified or politically chosen suppression of competing authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(doom_tr_t10, doomsday_clock_metric__objective_index_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(doom_tr_t20, doomsday_clock_metric__objective_index_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(doom_tr_t35, doomsday_clock_metric__objective_index_reading, theater_ratio, 35, 0.35).
narrative_ontology:measurement(doom_tr_t50, doomsday_clock_metric__objective_index_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(doom_tr_t65, doomsday_clock_metric__objective_index_reading, theater_ratio, 65, 0.4).
narrative_ontology:measurement(doom_tr_t75, doomsday_clock_metric__objective_index_reading, theater_ratio, 75, 0.41).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(doom_be_t10, doomsday_clock_metric__objective_index_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(doom_be_t20, doomsday_clock_metric__objective_index_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(doom_be_t35, doomsday_clock_metric__objective_index_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement(doom_be_t50, doomsday_clock_metric__objective_index_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(doom_be_t65, doomsday_clock_metric__objective_index_reading, base_extractiveness, 65, 0.68).
narrative_ontology:measurement(doom_be_t75, doomsday_clock_metric__objective_index_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(doom_su_t10, doomsday_clock_metric__objective_index_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(doom_su_t20, doomsday_clock_metric__objective_index_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(doom_su_t35, doomsday_clock_metric__objective_index_reading, suppression_requirement, 35, 0.74).
narrative_ontology:measurement(doom_su_t50, doomsday_clock_metric__objective_index_reading, suppression_requirement, 50, 0.77).
narrative_ontology:measurement(doom_su_t65, doomsday_clock_metric__objective_index_reading, suppression_requirement, 65, 0.78).
narrative_ontology:measurement(doom_su_t75, doomsday_clock_metric__objective_index_reading, suppression_requirement, 75, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.16).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% The Doomsday Clock kernel decomposes into three constraint stories, each instantiating a different reading of the Clock's social and epistemic function. The OBJECTIVE INDEX READING (this constraint) treats the Clock as a scientific instrument outputting empirical risk assessment; it is characterized by high suppression of normative framing and high extraction from democratic accountability. The PERFORMATIVE TOOL READING treats the Clock as strategically designed communication; it would show lower suppression (the strategy is acknowledged) but higher theater ratio and different victim structure (public attention is engineered). The HYBRID LEGITIMACY READING treats the Clock as necessarily entangling fact and value; it would show lower suppression (hybridity acknowledged) but would reframe extraction as inescapable structural tension rather than asymmetric capture. Each reading has its own ε, beneficiary/victim structure, and type classification. They are not perspectives on the same thing; they are structurally distinct constraints instantiated by different interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, powerless, 0.85).
constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
