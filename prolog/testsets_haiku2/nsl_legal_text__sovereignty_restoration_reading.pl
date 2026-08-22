% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: NSL as Legitimate Sovereign Security Restoration (2020 Reading)
 *   domain: constitutional_law/political_sociology
 *
 * SUMMARY:
 *   The National Security Law (NSL) enacted in 2020 is framed by the Central
 *   People's Government as a legitimate instrument restoring constitutional
 *   order and sovereignty after the 2019-2020 unrest destabilized the Hong
 *   Kong territory. Under this reading, the law coordinates unified security
 *   authority (CPG beneficiary) while targeting identifiable security threats
 *   (pro-independence activists and opposition figures as victims) who are
 *   perceived as destabilizing legitimate constitutional order. The reading
 *   asserts that unrest constituted a genuine sovereignty crisis requiring
 *   permanent legal restoration, not a political protest. This constraint
 *   story instantiates that framing entirely — it is the NSL as the restoring
 *   reading commits to it. Sibling readings (democratic_enclosure and
 *   jurisdictional_capture) instantiate different ε values, different
 *   beneficiary/victim structures, and different claims about the law's
 *   functional purpose. All three readings share the same legal text (the
 *   kernel); they differ in what the law is about and who it legitimately
 *   targets.
 *
 * KEY AGENTS:
 *   - Central People's Government (CPG): agenda-setter, institutional power — authors and enforces the law; frames unrest as sovereignty threat
 *   - Pro-independence activists: victims, moderate power, identity-locked exit — face prosecution for speech/assembly; exit means identity dissolution
 *   - Opposition political figures: victims, powerful, constrained exit — restricted in electoral activity and public speech; political standing forfeit if they relocate
 *   - Public order restoration constituency: beneficiary, organized power — experienced 2019-2020 unrest as violent disruption; perceive NSL as restoring safety
 *   - Common law judiciary: observer, institutional power — constrained by dependence on institutional legitimacy
 *   - International observers: observer, analytical power — assess whether restoration framing matches documented targeting patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.58).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.72).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "NSL as Legitimate Sovereign Security Restoration (2020 Reading)").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, '11dcd854-e033-43f3-84e2-d7c05509d7d2').
narrative_ontology:cs_kernel_codification('11dcd854-e033-43f3-84e2-d7c05509d7d2', formalized).
narrative_ontology:cs_authority_grounding('11dcd854-e033-43f3-84e2-d7c05509d7d2', extraction).
narrative_ontology:cs_interpretation_layer_present('11dcd854-e033-43f3-84e2-d7c05509d7d2').
narrative_ontology:cs_reading_relation('11dcd854-e033-43f3-84e2-d7c05509d7d2', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('11dcd854-e033-43f3-84e2-d7c05509d7d2', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('11dcd854-e033-43f3-84e2-d7c05509d7d2', foundational, unrest_was_genuine_sovereignty_threat).
narrative_ontology:cs_axiom_status(unrest_was_genuine_sovereignty_threat, holdable).
narrative_ontology:cs_axiom_grounding('11dcd854-e033-43f3-84e2-d7c05509d7d2', unrest_was_genuine_sovereignty_threat, empirically_contingent).
narrative_ontology:cs_axiom('11dcd854-e033-43f3-84e2-d7c05509d7d2', foundational, security_targeting_is_proportional_response).
narrative_ontology:cs_axiom_status(security_targeting_is_proportional_response, holdable).
narrative_ontology:cs_axiom_grounding('11dcd854-e033-43f3-84e2-d7c05509d7d2', security_targeting_is_proportional_response, instrumental).
narrative_ontology:cs_reference_frame('11dcd854-e033-43f3-84e2-d7c05509d7d2', constitutional_sovereignty_restoration_framework).
narrative_ontology:cs_drift_state('11dcd854-e033-43f3-84e2-d7c05509d7d2', post_2024_sustained_order, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('11dcd854-e033-43f3-84e2-d7c05509d7d2', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_people_government_cpg).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, public_order_restoration_constituency).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_independence_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, opposition_political_figures).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, civil_liberties_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, public_order_restoration_constituency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors, enacts, and enforces the NSL as a constitutional restoration instrument. Frames the law as restoring sovereignty threatened by foreign-backed destabilization; argues restoration of order is prerequisite to constitutional normalcy. Controls the framing apparatus and adjudicates security threat determinations. Collects the legitimacy benefit of restored central authority over contested territory.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_people_government_cpg, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Face criminal prosecution, arrest, and imprisonment under NSL statutes for speech, assembly, and political organizing that was protected under pre-2020 legal frameworks. Exit options are foreclosed by identity commitment to the independence movement; relocation means surrendering political identity and organizational participation. Bear the direct enforcement cost of the constraint.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_independence_activists, payer,
    moderate, generational, identity_locked, regional).

% Operate under NSL restrictions on public statements, electoral activity, and party organization. High-profile figures face prosecution risk proportional to visibility. Exit is constrained: relocating means forfeiting political standing in the home territory; staying means navigating a narrowed legal space. The law targets their structural power to mobilize opposition.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, opposition_political_figures, payer,
    powerful, biographical, constrained, regional).

% Document and contest NSL prosecutions; file legal challenges; report to international bodies. Bear enforcement costs through prosecution of officers, asset freezes, and operational constraints. Observer role because they are positioned to analyze the law's effects across seats; payer because their advocacy work itself becomes a security-compliance risk.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, civil_liberties_organizations, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, civil_liberties_organizations, observer).

% Citizens and businesses that experienced the 2019-2020 unrest as disruptive, violent, or destabilizing. Perceive NSL as restoring public order, security of person, and normal commercial life. Also bear diffuse costs through surveillance expansion and chilling effects on legitimate speech. Their preference for order is real; the law redistributes risk from them to the activist and opposition seats.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, public_order_restoration_constituency, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, public_order_restoration_constituency, payer).

% Historically administered common-law protections; now applies NSL statutes authored and interpreted through CPG authority. Observer role: positioned to assess whether the law's operation tracks the stated restoration function or exceeds it. Constrained because institutional survival depends on legitimacy within the political order.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, common_law_judiciary, observer,
    institutional, generational, constrained, regional).

% Treaty bodies, human rights commissions, and independent legal scholars assess NSL compliance with international covenant obligations and norms. Analytical seat: no operational participation in the constraint, but positioned to evaluate whether the law's stated function (security restoration) matches documented patterns (political targeting, speech restriction, opposition foreclosure).
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, central_people_government_cpg).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores a monopoly of legitimate political order and security authority after the 2019-2020 unrest fragmented institutional control. Coordinates unified enforcement authority across security agencies, courts, and administrative bodies under a single legal framework that establishes CPG sovereignty over the contested territory.
% TRANSFER_FUNCTION: Transfers political organizing capacity, speech scope, and assembly freedom from opposition and activist seats to the CPG's exclusive control of legitimate political discourse. Moves prosecution risk from the public order constituency (who experienced unrest costs) to the pro-independence and opposition seats (who are classified as security threats).
% ABSENT_VOICES: Pro-independence constituencies are present but voiceless in the framing apparatus: the NSL's architecture allows CPG-controlled courts to adjudicate the very threat-determination the law is said to address, precluding independent evaluation. International observer voices lack enforcement power within the jurisdiction, limiting their ability to challenge the restoration framing.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, opposition political activity would resume, independence organizations would reactivate, and the 2019-2020 protest coalition would have a legal channel to organize again. The CPG would lose its primary instrument for targeting political opposition under security framing; the institutional order would reorganize around common-law protections and pre-2020 constitutional norms. The public order constituency would face renewed protest activity but lose the security-order framework they perceive as protecting them.
% FOUNDING_PROBLEM: The 2019-2020 unrest destabilized public order, produced violent confrontations, and fragmented institutional authority. The founding problem, as framed by the restoring reading: a genuine security and sovereignty crisis required immediate legal restoration of unified state authority.
% FOUNDING_PROBLEM_CORROBORATION: The CPG and public order constituencies attest the founding problem is live and require continued security vigilance. International observers and opposition seats contest the magnitude: they assert the unrest was political protest with real violence, not a sovereignty-destroying security threat requiring permanent legal changes. Pro-independence and civil liberties sectors assert the founding problem has been addressed through court orders and deployment, making permanent legal changes unnecessary. No independent, neutral arbiter has adjudicated the founding problem's status outside the CPG-controlled institutional framework.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.58 (moderate) reflects the reading's core claim: the law targets a specific threat (pro-independence/opposition organizing) that genuinely destabilized order, NOT the general population. The CPG benefits from restored sovereign authority; the public order constituency benefits from renewed security. The activist/opposition seats bear concentrated targeting costs. Suppression at 0.72 is high because enforcement depends on foreclosing organizing alternatives (arrest, prosecution, asset freezes), not voluntary participation. Theater at 0.41 (moderate) reflects the dual function: genuine restoration of unified authority (real function) overlaid with a prosecutorial apparatus that may target legitimate political opposition beyond security threats (performative function). The measurement series shows rising extractiveness from t0 to t24 (as the law's actual operation accumulates prosecution cases and suppression-requirement intensifies), then a plateau from t24 onward as the law stabilizes into a steady-state enforcement regime. The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the CPG's seat, the law is a rational restoration instrument: unrest was genuine, response was proportional, and operation targets identifiable security threats within appropriate scope. From the pro-independence activist seat, the law is a permanent criminal apparatus targeting political activity that was protected expression before 2020. From the public order seat, the law is justified by their experience of violence and disruption. The engine computes these divergent seat-level types from the structural data: CPG (beneficiary, arbitrage exit) may compute as rope; activists (identity-locked, concentrated targeting) may compute as snare; public order constituency (beneficiary with diffuse costs) may sit between. The authored claim is tangled_rope because the restoration function is real (coordination) and the targeting is real (asymmetric extraction), but the measurement metrics lean extractive because the suppression requirement is high and the theater ratio shows growing prosecutorial activity beyond security restoration.
 *
 * DIRECTIONALITY LOGIC:
 *   The CPG is the structural beneficiary under this reading: it collects restored sovereign authority, frames the security threat, and controls adjudication. Pro-independence activists are the structural targets: they are identity-locked in the movement (exit = dissolution of political self), face concentrated prosecution, and cannot arbitrage to an alternative political space within the jurisdiction. Opposition figures are targets: their power is real, but constrained; they can exit by relocating but forfeit political standing. The public order restoration constituency sits nearer symmetric: they benefit from restored order (real benefit), but also bear diffuse costs through surveillance and speech chilling. The reading's architecture makes the CPG the sole arbiter of what constitutes a security threat — this asymmetry is baked into the directionality derivation: beneficiaries control the classification system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is the 2019-2020 unrest; the restoration reading claims it was a genuine sovereignty crisis. Mandatrophy would arise if the founding problem were dead (unrest is over, order is restored) but the law persists with rising extractiveness (prosecution accumulates beyond security necessity). The measurement series shows extractiveness rising through t24, then plateauing — consistent with a law that solves its founding problem (unrest quashed, order restored) but persists as an enforcement/targeting apparatus. The theater_ratio rising from 0.28 to 0.44 suggests growing performative maintenance: as the security threat objectively declines (unrest is successfully suppressed), enforcement energy diverts to opposition targeting to maintain the law's legitimacy. The mismatch consumer would flag: founding_problem_status=contested + disappearance_verdict=world_rearranges + theater_rising + extractiveness_plateauing = potential mandatrophy signature. However, the reading does not CLAIM mandatrophy resolved; it asserts ongoing security need. The mismatch is exactly the signal the framework is designed to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_threat_magnitude_ambiguity,
    'Was the 2019-2020 unrest a genuine sovereignty-threatening security crisis, or was it political protest with real violence that did not threaten state institutional capacity?',
    'Comparative analysis of institutional fragmentation, security apparatus incapacity, and violence intensity against cases of documented security crises (e.g., insurgencies, coordinated coup attempts). Independent assessment of whether the state''s normal enforcement apparatus (police, courts, riot response) was incapable of restoring order without permanent legal changes.',
    'If the unrest was a genuine sovereignty crisis, the extraction targeting is proportional threat-response, and the extraction score should be lower (closer to 0.45). If the unrest was protest, the same targeting becomes political persecution, and the extraction score should be higher (closer to 0.72). The classification shifts from tangled_rope (real coordination + asymmetric targeting) toward snare (targeting frames itself as response to manufactured threat).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_threat_magnitude_ambiguity, empirical, 'Whether the founding problem was an existential security threat or political protest with violence.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Can the sovereignty_restoration_reading coexist with the democratic_enclosure_reading as live positions in a single institutional framework, or does the restoration reading''s core premise (unrest was security threat, not democratic suppression) logically foreclose the enclosure reading?',
    'Examination of whether CPG institutional authorities could simultaneously hold the restoration framing AND acknowledge that the law functions to suppress democratic opposition — i.e., whether the law could be justified as both security response AND opposition suppression in the same framework. If the restoration reading requires denying the enclosure framing''s core claim (that opposition suppression is a primary function), then foreclosure exists.',
    'If the readings foreclose each other, the cs_structure.reading_relations should declare forecloses, and the engine''s constraint-family analysis would treat them as mutually exclusive. If they coexist (both are live positions held by different institutional authorities), the relation is coexists_with, and the constraint family represents genuine institutional disagreement rather than logical contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the sovereignty-restoration reading logically forecloses the democratic-enclosure reading or both remain live as coexistent positions.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of pro-independence activism structural (legal barriers, arrest, prosecution, asset freezes, surveillance) or partly internalized (activists have internalized the threat, self-censor even absent enforcement, carry the suppression with them after political defeat)?',
    'Longitudinal study of activist behavior and speech patterns post-arrest vs. activists who left the jurisdiction: if suppression persists in the diaspora community (self-censorship, identity dissolution), internalization is significant. If activism resumes after a generation in exile communities without the NSL apparatus, suppression is primarily structural.',
    'If suppression is substantially internalized, the effective suppression is higher than the structural measure (0.72) suggests — the constraint''s extractive force persists even if formal enforcement weakens. If suppression is purely structural, removing the legal apparatus would quickly restore activist organizing. The distinction informs whether the constraint is a transient legal instrument (structural suppression, removable) or a permanent cognitive restructuring (internalized, persistent).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural legal barriers or internalized political identity dissolution.').

omega_variable(
    cpg_authority_legitimacy_grounding,
    'Is the CPG''s authority to define security threats grounded in restored constitutional sovereignty (a defensible legal claim within the territory''s governance framework), or is it grounded in extraction of political opposition power (using security framing as cover for regime consolidation)?',
    'Analysis of the NSL''s authority chain: does it route through the territory''s pre-existing constitutional framework and courts, or does it bypass them and establish a parallel authority structure? Do CPG-controlled courts adjudicate all security determinations? Can common-law courts overturn security threat classifications? If authority bypasses the territory''s constitutional institutions, legitimacy is extractive rather than restorative.',
    'If authority is constitutionally grounded, the restoration reading is descriptively coherent, and the constraint may be rationally defeasible (if the founding problem ends, the law could legitimately sunset). If authority bypasses the territory''s framework, the restoration reading is using constitutional framing to cover institutional capture, shifting the constraint from tangled_rope (real coordination + asymmetric targeting) toward snare (targeting frames itself as response to security threat that is actually regime consolidation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cpg_authority_legitimacy_grounding, empirical, 'Whether the CPG''s authority to adjudicate security threats is grounded in constitutional restoration or institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 36, 0.44).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 48, 0.41).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 36, 0.61).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 48, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 36, 0.74).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 48, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__sovereignty_restoration_reading, 0.12).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% The National Security Law is a contested kernel instantiated by three structurally distinct readings. This constraint story (sovereignty_restoration_reading, ε=0.58) represents the CPG's framing: unrest was a genuine security/sovereignty crisis, targeting is proportional threat-response, and the law restores constitutional order. The democratic_enclosure_reading (ε≈0.78) reads the same text as a permanent closure mechanism for democratic space, with opposition and activism as primary targets. The jurisdictional_capture_reading (ε≈0.71) reads the text as vehicle for mainland legal system transplantation, eroding common-law autonomy. All three readings decompose from one kernel and share the same legal text; they differ in what the law is about and who it legitimately targets. The three constraints form a family linked by affects_constraints; no single reading is privileged; all three represent live institutional framings instantiated by different authority structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
