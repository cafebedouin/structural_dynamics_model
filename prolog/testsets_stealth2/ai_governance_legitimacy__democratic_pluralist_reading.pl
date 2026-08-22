% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__democratic_pluralist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: Democratic-Pluralist Reading of AI Governance Legitimacy
 *   domain: political theology / technology governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the ai_governance_legitimacy
 *   kernel: the democratic-pluralist reading, on which AI governance is
 *   legitimate only when authorized through inclusive public reason and
 *   consent of the governed, with no tradition — religious or technocratic —
 *   holding interpretive monopoly. The encyclical enters as one moral voice
 *   among many. The referent of epsilon is the standing
 *   participatory-governance arrangement AS IT ACTUALLY OPERATES
 *   (consultation portals, citizen assemblies, parliamentary AI committees,
 *   judicial review), assessed by this reading's own lights — not the fully
 *   inclusive ideal the reading endorses. On that referent the arrangement
 *   genuinely coordinates (it solves the pluralist authorization problem)
 *   while extracting real costs from those its deliberative machinery cannot
 *   reach: authoritarian subjects, the digitally excluded, the unborn. The
 *   claim/metric gap is deliberate: the reading CLAIMS scaffold (transitional
 *   participatory infrastructure with a completion condition) while the
 *   authored metrics describe moderate, slowly accumulating extraction — the
 *   engine measures that divergence; nothing here reconciles them.
 *
 * KEY AGENTS:
 *   - democratic_institutions: agenda-setter (institutional/constrained) — runs the deliberative machinery, collects the legitimacy rents of being the venue
 *   - civil_society_organizations: beneficiary (organized/mobile) — gains standing and resources from participatory channels
 *   - minority_rights_holders: beneficiary (moderate/constrained) — protected by process guarantees and judicial review
 *   - ai_developing_corporations: payer with secondary benefit (powerful/arbitrage) — bears compliance costs, buys social license, can relocate
 *   - technical_expert_advisors: payer with secondary benefit (institutional/constrained) — traded unilateral authority for mandated advisory roles
 *   - religious_tradition_representatives: beneficiary with secondary payment (organized/mobile) — guaranteed voice, surrendered monopoly
 *   - populations_under_authoritarian_regimes: payer (powerless/trapped) — bound by norms they cannot authorize or escape
 *   - digitally_excluded_communities: payer (powerless/trapped) — unreachable by the digital-first deliberative surface
 *   - future_generations: excluded (non-agent) — inherit commitments made without representation
 *   - deliberative_democracy_scholars: observer (analytical/analytical) — audits whether participation forms consent or manufactures it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.3).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "Democratic-Pluralist Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "political theology / technology governance").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, '73ff3777-eeac-4215-ac6f-9f1032b3ef13').
narrative_ontology:cs_kernel_codification('73ff3777-eeac-4215-ac6f-9f1032b3ef13', distributed).
narrative_ontology:cs_authority_grounding('73ff3777-eeac-4215-ac6f-9f1032b3ef13', practice).
narrative_ontology:cs_interpretation_layer_present('73ff3777-eeac-4215-ac6f-9f1032b3ef13').
narrative_ontology:cs_reading_relation('73ff3777-eeac-4215-ac6f-9f1032b3ef13', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('73ff3777-eeac-4215-ac6f-9f1032b3ef13', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('73ff3777-eeac-4215-ac6f-9f1032b3ef13', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('73ff3777-eeac-4215-ac6f-9f1032b3ef13', foundational, legitimacy_requires_inclusive_public_reason).
narrative_ontology:cs_axiom_status(legitimacy_requires_inclusive_public_reason, holdable).
narrative_ontology:cs_axiom_grounding('73ff3777-eeac-4215-ac6f-9f1032b3ef13', legitimacy_requires_inclusive_public_reason, deontological).
narrative_ontology:cs_axiom('73ff3777-eeac-4215-ac6f-9f1032b3ef13', foundational, no_interpretive_monopoly_over_ai_norms).
narrative_ontology:cs_axiom_status(no_interpretive_monopoly_over_ai_norms, holdable).
narrative_ontology:cs_axiom_grounding('73ff3777-eeac-4215-ac6f-9f1032b3ef13', no_interpretive_monopoly_over_ai_norms, conventional).
narrative_ontology:cs_reference_frame('73ff3777-eeac-4215-ac6f-9f1032b3ef13', inclusive_public_reason_pluralism).
narrative_ontology:cs_drift_state('73ff3777-eeac-4215-ac6f-9f1032b3ef13', contemporary_participatory_governance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('73ff3777-eeac-4215-ac6f-9f1032b3ef13', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, digitally_excluded_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, ai_developing_corporations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, technical_expert_advisors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, religious_tradition_representatives).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, ai_developing_corporations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, technical_expert_advisors).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, religious_tradition_representatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the parliaments, courts, electoral systems, and consultative machinery that convert public deliberation into binding AI rules. Sets the agenda for what counts as a legitimate AI governance question, administers the participation channels, and collects the authority that flows from being the recognized venue. Exit would mean constitutional rupture rather than relocation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Gain formal standing, consultative access, and funding streams from the deliberative architecture; their influence depends on the continued centrality of participatory channels. They can shift attention across issue domains if AI governance loses salience.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, global).

% Depend on inclusive-process guarantees, judicial review, and civil liberties protections to prevent majority-approved AI systems from overriding their claims. Their protection is internal to the framework: leaving the jurisdiction trades one rights regime for another of uncertain quality.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    moderate, generational, constrained, national).

% Bear compliance costs, mandatory impact assessments, disclosure obligations, and delayed deployments under democratically authorized rules. They also receive a social license and stable market access that unilateral self-governance cannot confer. Development and deployment can be shifted toward permissive jurisdictions, giving them the strongest exit position of any bound party.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, ai_developing_corporations, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, ai_developing_corporations, beneficiary).

% Standards bodies and scientific advisory panels surrender unilateral standard-setting authority: their recommendations bind only when converted through democratic mandate. In exchange they receive formal advisory roles, mandates, and resourcing inside the process they no longer control.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technical_expert_advisors, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, technical_expert_advisors, beneficiary).

% Hold a guaranteed seat in the deliberative order as contributors of moral argument — the encyclical is heard as one voice among many. The same arrangement strips interpretive monopoly: their pronouncements persuade or fail on public reasons rather than binding by authority. Withdrawal from secular deliberation is possible but would forfeit the standing the framework grants them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, religious_tradition_representatives, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, religious_tradition_representatives, payer).

% Live under AI-enabled surveillance, scoring, and information control deployed by governments that reject consent-based legitimacy outright. Democratically negotiated AI norms bind the systems they encounter without offering them any participation channel; their rulers never enter the framework on their behalf, and emigration is blocked or ruinous.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes, payer,
    powerless, biographical, trapped, regional).

% Lack the connectivity, devices, digital literacy, or language access that the deliberative process presupposes. Consultations run through online portals and digital-first engagement formats they cannot reach, while algorithmic credit, benefits, and policing decisions land on them directly. There is no offline equivalent channel to exit into.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, digitally_excluded_communities, payer,
    powerless, immediate, trapped, global).

% Will inherit AI infrastructures, locked-in standards, and trained-in value alignments authorized by processes they could not join. They would object to irreversible commitments made without their interests represented; they exist nowhere in the current conversation except as rhetorical invocations.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(ai_governance_legitimacy__democratic_pluralist_reading, future_generations).

% Study whether participatory AI governance processes form genuine consent or manufacture legitimation: tracing participation-to-outcome causal chains, auditing who is invited, and comparing announced inclusiveness with actual agenda control.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, deliberative_democracy_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__democratic_pluralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how societies holding irreconcilable comprehensive doctrines can authorize binding rules for a transformative technology without any single tradition — confessional or technocratic — monopolizing interpretation. Converts value pluralism from deadlock into a workable procedure: public reasons, transparent process, majority decision bounded by rights.
% TRANSFER_FUNCTION: Moves interpretive authority over AI's permissible uses from unelected holders (corporate boards, expert bodies, religious hierarchies) to accountable democratic venues; moves public resources toward inclusion infrastructure (consultation budgets, civic participation programs); moves compliance costs onto deployers and opportunity costs onto anyone preferring unilateral action.
% ABSENT_VOICES: Populations under authoritarian regimes, digitally excluded communities, and future generations would object if present: rules bind them that they had no hand in authorizing, and the deliberative channels that legitimate those rules are structurally unreachable from where they stand. Non-citizen residents affected by extraterritorially deployed AI systems sit in the same position. They are outside the venues — disenfranchised, unconnected, or unborn — and their absence is what keeps the measured extraction moderate rather than negligible.
% DISAPPEARANCE_RATIONALE: If the consent-based legitimacy requirement vanished overnight, AI governance would reorganize around whichever claimant moved fastest — corporate self-certification, technical standards bodies, or state directive — and the one channel through which dissenting value traditions can contest deployment decisions would close. Civil society consultative structures, minority-rights litigation strategies, and the encyclical's standing as a public-reason contributor would all lose their operative home within months.
% FOUNDING_PROBLEM: How can binding rules for a capability that reshapes speech, labor, credit, and coercion be legitimately authorized when the governed share no comprehensive doctrine — without reverting either to confessional authority or to unchecked expert administration? The democratic-pluralist answer was built as the general post-confessional legitimacy solution extended to a new technological domain.
% FOUNDING_PROBLEM_CORROBORATION: Every sibling reading attests the founding problem is live while disputing the answer: the magisterial reading concedes no secular consensus exists, the technocratic reading concedes that value trade-offs outrun technical method, and the market-libertarian reading concedes that AI externalities exceed what exit and exchange resolve. Outside the beneficiary set, industry actors' pursuit of 'social license,' UNESCO and OECD multistakeholder processes, and legislative hearings on AI legitimacy all corroborate the deficit. No corroborating source attests the problem is dead.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).
:- end_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.40 because the arrangement's costs concentrate on exactly the parties the consent mechanism cannot hear: the constraint taxes unilateral authority moderately everywhere, but its residual extraction lands disproportionately on the voiceless. Suppression is 0.30 — enforcement runs through elections, courts, and civil-liberties protections rather than prohibition; the sibling readings remain fully live positions, so alternatives are suppressed only in standing, not in availability. Theater_ratio 0.38 reflects the documented consultation-washing pattern: a growing share of participatory activity legitimates predetermined outcomes rather than forming consent, though electoral accountability keeps a real causal channel open. Accessibility_collapse is low (0.35) because understanding this constraint does not close alternatives — the magisterial, technocratic, and market-libertarian readings survive intact as rival constraints. Resistance is substantial (0.58): industry lobbies against binding oversight, authoritarian states reject the framework wholesale, and religious authorities resist demotion from authoritative interpreter to one-contributor-among-many. The temporal series share one grid (T=0,3,6,9,12,15,18) with all three metrics authored at every point; extraction and theater rise together as participatory ritual outpaces participatory power, while suppression_requirement plateaus after the enforcement build-out — the story traces enforcement-capacity maturation, hence the suppression series. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (democratic_institutions) the arrangement IS its own authority source — the constraint reads as the framework that makes its decisions count. From the beneficiary seats (civil society, minorities, religious contributors) it reads as empowerment: guaranteed standing, protected claims, a hearing. From the arbitrage-bearing payer seat (corporations) it reads as a manageable tax softened by exit. From the trapped payer seats (authoritarian populations, digitally excluded) the same structure operates as a gate that validates rules binding them without them — procedure experienced as exclusion. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for civil_society_organizations, minority_rights_holders, and (net of their secondary payment) religious_tradition_representatives. Victim declarations drive high directionality for populations_under_authoritarian_regimes and digitally_excluded_communities — amplified by trapped exit, which sits them nearest the full-target pole. Dual-positioned agents are encoded with secondary_role rather than overrides: corporations (payer/beneficiary, arbitrage exit) and technical experts (payer/beneficiary, constrained) derive intermediate d from the combination, and religious representatives (beneficiary/payer, mobile) derive near-symmetric d. No directionality_overrides are authored: the derivation chain captures every asymmetry from the declared roles and exit options, and an override keyed by power atom would collide across same-power seats (organized covers both civil-society beneficiaries and religious contributors; institutional covers both the agenda-setter and the expert payers).
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim is load-bearing here. The reading's mandate is transitional: build participatory infrastructure until standing democratic institutions absorb the AI-governance function, at which point ad hoc assemblies, provisional frameworks, and emergency consultative bodies dissolve (hence has_sunset_clause: true alongside active enforcement). Mandatrophy risk is real and tracked: if transitional bodies persist past completion, the arrangement degenerates into performed participation — the rising theater_ratio series is the early-warning signature, and the sunset_completion_risk omega records the open question. The R5 interview supports the live-mandate reading: founding problem live, disappearance verdict world_rearranges — the mismatch consumer sees no dead-mandate-plus-dependence flag. Classification prevents mislabeling in both directions: reading the arrangement as pure coordination ignores the trapped payers who fund its legitimacy with their exclusion; reading it as pure extraction ignores the genuine pluralist deadlock it resolves and the minority protections only this structure supplies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the democratic_pluralist_reading of the ai_governance_legitimacy kernel — how would classification shift if a sibling reading were instantiated instead?',
    'Comparative classification across the four sibling stories, using the foreclosure/coexistence edges declared in cs_structure.reading_relations; the engine computes per-reading types from each file''s structural data.',
    'Under the magisterial reading the victim set swaps to conscience-objectors and non-Catholic traditions and democratic_institutions drops to payer; under the technocratic reading expert bodies take the agenda-setter seat and consent-based protections become friction; epsilon re-bins in each case. This file''s numbers are valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position of this story within the four-reading kernel; sibling deltas are structural, not metric noise.').

omega_variable(
    deliberation_genuineness,
    'Is the measured deliberation genuine consent-formation, or legitimation theater that ratifies predetermined outcomes?',
    'Trace participation-to-outcome causal chains: do citizen-assembly recommendations and public consultations statistically influence enacted AI rules, controlling for agenda-setter preferences?',
    'If theater dominates, theater_ratio crosses 0.5 and the scaffold drifts toward piton-shaped performance; if genuine, the sunset path stays open and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberation_genuineness, empirical, 'Whether participatory activity forms consent or performs it.').

omega_variable(
    demos_boundary_ambiguity,
    'Who counts as ''the governed'' whose consent legitimates — territorial citizens, all subjected to AI systems, or humanity at large?',
    'Conceptual analysis joined to jurisdictional practice: compare franchise rules, extraterritorial-application doctrines, and affected-interests representation schemes across democracies.',
    'A broader demos recognizes more victims and pushes epsilon upward; a narrower demos shrinks the victim set and lowers it. The current 0.40 assumes the mixed practice actually observed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_boundary_ambiguity, conceptual, 'Boundary of the consenting demos is underdetermined by the reading itself.').

omega_variable(
    sunset_completion_risk,
    'Will the participatory infrastructure complete its transition into standing democratic AI institutions, or persist indefinitely as permanent scaffolding?',
    'Track whether transitional bodies — ad hoc assemblies, provisional frameworks, emergency consultative commissions — dissolve on schedule as standing institutions absorb their function.',
    'Failure to sunset converts the scaffold into an inertial fixture maintained by routine and performance; success keeps the classification transitional and eventually retires the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_completion_risk, empirical, 'Whether the declared sunset clause is honored or becomes decorative.').

omega_variable(
    authoritarian_population_directionality,
    'Are populations under authoritarian regimes victims of this constraint (bound by norms they cannot authorize) or indirect beneficiaries (shielded by democratic norm diffusion they did not author)?',
    'Compare rights and welfare outcomes for populations inside versus outside the reach of democratically negotiated AI norms; measure diffusion of accountability standards into non-democratic jurisdictions.',
    'If diffusion dominates, their directionality swings toward the beneficiary pole and epsilon drops materially; if exclusion dominates, they anchor the high-directionality pole and epsilon holds or rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authoritarian_population_directionality, empirical, 'Sign of the largest trapped-payer seat''s structural relationship is unresolved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_g_tr_t3, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement(ai_g_tr_t6, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(ai_g_tr_t9, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 9, 0.33).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(ai_g_tr_t18, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 18, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_g_be_t3, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 3, 0.31).
narrative_ontology:measurement(ai_g_be_t6, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement(ai_g_be_t9, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 9, 0.36).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(ai_g_be_t18, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 18, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ai_g_su_t3, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 3, 0.24).
narrative_ontology:measurement(ai_g_su_t6, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 6, 0.27).
narrative_ontology:measurement(ai_g_su_t9, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 9, 0.29).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(ai_g_su_t18, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 18, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, market_libertarian_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI governance legitimacy' decomposes into four structurally distinct constraints — one per reading of the shared kernel. Each member carries its own epsilon, beneficiary/victim structure, and claimed type; linking them via affects_constraints lets contamination and foreclosure propagate correctly. This reading influences the others' operating environment (every sibling must argue against a live democratic-authority competitor) while logically foreclosing two of them within any single framework (see cs_structure.reading_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
