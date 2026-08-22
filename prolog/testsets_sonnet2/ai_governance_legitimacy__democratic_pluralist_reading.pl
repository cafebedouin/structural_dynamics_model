% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Democratic Pluralist Reading of AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This story instantiates the democratic pluralist reading of the contested
 *   'AI governance legitimacy' kernel: the claim that legitimacy for AI
 *   governance flows from democratic deliberation and consent of the
 *   governed, with no tradition — including the Catholic Magisterium's
 *   encyclical tradition — holding interpretive monopoly. Under this reading
 *   the encyclical is welcomed as one voice among many but denied final
 *   authority; legitimacy is procedural (who deliberated, who consented)
 *   rather than substantive (which tradition's account of dignity is
 *   correct). The reading builds real participatory infrastructure —
 *   electoral accountability, judicial review, civil liberties protections —
 *   that functions as a scaffold: a transitional architecture whose declared
 *   purpose is enabling broader, more inclusive deliberation over time, not a
 *   permanent settlement of the underlying value questions. It is explicitly
 *   one of four sibling readings of the same kernel (magisterial
 *   subsidiarity, technocratic optimization, market libertarian); each is
 *   authored as its own constraint with its own epsilon, per the
 *   epsilon-invariance principle, and linked via network edges.
 *
 * KEY AGENTS:
 *   - civil_society_organizations: organized beneficiary and partial agenda-setter — gains standing through the deliberative channel
 *   - democratic_institutions: primary agenda-setter — administers the deliberative and enforcement machinery
 *   - minority_rights_holders: powerless beneficiary — protected only as strongly as judicial review backs them
 *   - excluded_deliberative_populations: payer — governed by outputs of a process they cannot enter
 *   - authoritarian_regime_subjects: payer — no access to the legitimating mechanism at all
 *   - encyclical_authors: excluded/observer — admitted to the conversation, denied interpretive priority
 *   - technology_firms: payer/beneficiary — compliance costs offset by legitimacy conferred
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.32).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "Democratic Pluralist Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, 'e12cb67a-7916-4a16-9103-a659de291a4f').
narrative_ontology:cs_kernel_codification('e12cb67a-7916-4a16-9103-a659de291a4f', distributed).
narrative_ontology:cs_authority_grounding('e12cb67a-7916-4a16-9103-a659de291a4f', distributed).
narrative_ontology:cs_reading_relation('e12cb67a-7916-4a16-9103-a659de291a4f', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e12cb67a-7916-4a16-9103-a659de291a4f', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('e12cb67a-7916-4a16-9103-a659de291a4f', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('e12cb67a-7916-4a16-9103-a659de291a4f', foundational, no_tradition_holds_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_tradition_holds_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('e12cb67a-7916-4a16-9103-a659de291a4f', no_tradition_holds_interpretive_monopoly, conventional).
narrative_ontology:cs_axiom('e12cb67a-7916-4a16-9103-a659de291a4f', foundational, legitimacy_derives_from_procedural_consent_not_doctrinal_content).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_procedural_consent_not_doctrinal_content, holdable).
narrative_ontology:cs_axiom_grounding('e12cb67a-7916-4a16-9103-a659de291a4f', legitimacy_derives_from_procedural_consent_not_doctrinal_content, conventional).
narrative_ontology:cs_reference_frame('e12cb67a-7916-4a16-9103-a659de291a4f', liberal_democratic_public_reason_tradition).
narrative_ontology:cs_drift_state('e12cb67a-7916-4a16-9103-a659de291a4f', contemporary_ai_governance_debates, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e12cb67a-7916-4a16-9103-a659de291a4f', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, excluded_deliberative_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regime_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, technology_firms).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, technology_firms).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, public_reason_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, consent_of_the_governed_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% NGOs, advocacy coalitions, and public-interest technologists gain standing to shape AI governance through hearings, litigation, and coalition politics. They both benefit from the deliberative infrastructure existing and help set its agenda by organizing constituencies and drafting proposed rules; their leverage depends entirely on the deliberative channel staying open and funded.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, agenda_setter).

% Legislatures, courts, and electoral bodies administer the deliberative process itself — drafting AI statutes, adjudicating disputes, certifying elections that produce mandates. They can revise the rules of deliberation but are bound by their own procedures and by electoral accountability; changing course requires winning elections or court cases, not unilateral fiat.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Groups whose interests are easily outvoted or under-resourced in majoritarian processes rely on judicial review and civil liberties protections layered onto the deliberative scaffold to keep AI governance from simply implementing majority preference against them. Their protection is only as strong as the enforcement machinery that backs it, and they cannot exit the jurisdiction whose deliberation governs them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    powerless, biographical, trapped, national).

% Non-citizens, disenfranchised residents, and communities without organized advocacy have no seat at the deliberative table even though AI systems governed by its outputs affect them directly — content moderation, benefits allocation, predictive policing. The arrangement's claim to legitimacy through 'consent of the governed' does not extend to them because they are not counted among 'the governed' in the relevant electorate.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, excluded_deliberative_populations, payer,
    powerless, biographical, trapped, national).

% Populations living under regimes that reject or hollow out democratic deliberation have no access to the mechanism this reading treats as the sole source of legitimate AI governance. Global AI systems built or exported under this framework's normative cover reach them without any deliberative process on their behalf, and the reading offers them no alternative legitimation path — only the hope of eventual democratization.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regime_subjects, payer,
    powerless, civilizational, trapped, global).

% The Magisterium and its interpretive apparatus offer a substantive account of dignity and the common good, but under this reading their voice is admitted only as one input among many competing traditions in public deliberation, stripped of any claim to interpretive authority. They can participate in the conversation but cannot adjudicate it.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, encyclical_authors, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, encyclical_authors, observer).

% AI developers and platform operators must comply with whatever rules emerge from democratic deliberation in each jurisdiction, which is costly and unpredictable compared to a single settled standard — but they also benefit from the legitimacy the process confers on rules they eventually comply with, and can shift operations toward jurisdictions with lighter deliberative output.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technology_firms, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, technology_firms, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__democratic_pluralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedural mechanism — elections, legislatures, courts, public comment — through which people with genuinely different values about AI can arrive at rules they are bound to accept as legitimate without any one tradition imposing its substantive answer on the rest.
% TRANSFER_FUNCTION: Moves interpretive authority away from any single tradition (religious or technocratic) and toward organized political constituencies capable of participating effectively in deliberative processes; moves practical governance capacity toward those with the resources to organize, litigate, and vote, and away from those without standing in the relevant polity.
% ABSENT_VOICES: Non-citizens and residents outside the enfranchised population, populations under authoritarian rule who have no functioning deliberative channel to enter, and the Magisterium itself, which is admitted to the conversation but denied any interpretive priority even regarding claims it originated.
% DISAPPEARANCE_RATIONALE: If democratic deliberation ceased to be treated as the source of AI governance legitimacy, the entire justificatory architecture for current AI regulation in democracies — legislative mandates, judicial review of algorithmic harms, public comment processes — would need to be replaced by some other legitimating story (technocratic, religious, or market-based), and civil society's standing to shape policy would collapse absent electoral or judicial leverage.
% FOUNDING_PROBLEM: Which tradition or authority gets to say what AI systems may and may not do, given that religious, technocratic, and market traditions each claim a monopoly on the correct answer and none can be neutrally adjudicated by the others.
% FOUNDING_PROBLEM_CORROBORATION: Comparative political theorists and constitutional scholars outside any advocacy coalition attest that the problem of adjudicating among rival legitimation claims for emerging technologies remains unresolved and structurally live; however, no corroboration exists from populations under authoritarian rule or from unenfranchised residents, since this reading offers them no standing from which to attest anything about the arrangement that governs them.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.4, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness sits at 0.40, moderate as specified by the expected structural delta: the deliberative scaffold genuinely redistributes interpretive power away from any single incumbent tradition, but it does so by transferring practical governance capacity toward whoever can organize effectively within the electorate, which is itself an asymmetric outcome for the unorganized and the disenfranchised. Suppression is comparatively low (0.32) because the mechanism relies on procedural inclusion and legal protections rather than coercive enforcement of a single substantive doctrine, though it rises modestly over the interval as judicial and regulatory machinery matures and hardens. Theater ratio is modest (0.28): public comment periods and participatory forums carry some genuinely performative risk (consultation theater) but the underlying electoral and judicial accountability functions remain substantively real over the tracked interval.
 *
 * PERSPECTIVAL GAP:
 *   Democratic institutions and civil society organizations, from their own seats, experience this arrangement as a rope-like coordination solution to an otherwise-intractable clash of rival authority claims. Excluded populations and authoritarian-regime subjects experience the same structure as an arrangement that extracts practical governance authority over their lives while offering them no path to consent — from their seat it reads closer to tangled coordination-with-extraction, since the 'consent of the governed' framing formally excludes them from 'the governed.' The engine should compute this divergence directly from the beneficiary/victim and exit-option declarations; the claimed_type of scaffold reflects the story's own declared transitional self-understanding, not a resolution of that divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil society organizations and democratic institutions sit near the beneficiary end: they gain standing, resources, and (for institutions) administrative power from the deliberative infrastructure. Minority rights holders are beneficiaries in principle but structurally powerless and trapped, so their benefit is contingent and thin. Excluded deliberative populations and authoritarian regime subjects are the clearest targets: high directionality toward extraction because the constraint's legitimating story (consent of the governed) is constructed in a way that structurally cannot include them, yet its downstream effects (AI systems built and regulated under its authority) reach them anyway. Technology firms occupy a mixed position — payers of compliance cost with mobile exit options, but partial beneficiaries of the legitimacy the process confers on rules once settled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adjudicating among rival legitimation claims for emerging technology absent a neutral arbiter — remains live by the corroboration of independent political theorists, which is why this reading is not classified as a piton. But the scaffold's declared sunset condition (eventual convergence on settled, broadly legitimate AI governance norms) has not been met, and the arrangement shows signs of persisting past any clear transition marker, which is the mandatrophy risk to monitor: a scaffold whose transition never completes functions increasingly as permanent infrastructure without ever being re-justified as such.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_reading_of_ai_governance_legitimacy,
    'Is legitimacy for AI governance properly located in democratic-procedural consent, in Magisterial doctrinal authority, in technocratic performance, or in market voluntarism — and can any of these four readings be adjudicated as correct independent of which tradition is doing the adjudicating?',
    'No purely empirical resolution mechanism exists; this is a live political-theological and political-philosophical dispute. Partial evidence could come from comparative outcomes (do jurisdictions governed under each reading produce measurably different dignity or welfare outcomes for excluded populations), but the underlying legitimacy claim is contested at the level of first principles, not just consequences.',
    'If the democratic-pluralist reading is adopted as authoritative, the Magisterium''s dignity claims are demoted to one input among many, civil society and democratic institutions gain governance standing, and populations outside the enfranchised polity remain structurally unaddressed. If a sibling reading is adopted instead, the beneficiary and victim sets shift substantially — e.g., under the magisterial reading, the Magisterium becomes agenda-setter rather than excluded voice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_of_ai_governance_legitimacy, preference, 'Committer-level ambiguity: which reading of the ai_governance_legitimacy kernel is authoritative.').

omega_variable(
    consent_of_the_governed_scope_ambiguity,
    'Does ''consent of the governed'' as the legitimating mechanism include only enfranchised citizens of the deliberating polity, or does it extend in principle to all persons materially affected by the AI systems that polity''s deliberation authorizes (including non-citizens, foreign populations, and future generations)?',
    'Examine whether deliberative bodies extend formal comment rights, standing, or representation to non-citizen affected parties in practice, versus treating consent as coextensive with electoral citizenship.',
    'A narrow scope confirms the reading''s structural exclusion of excluded_deliberative_populations and authoritarian_regime_subjects as a permanent feature rather than a contingent gap; a broad scope would require redesigning the deliberative infrastructure itself, substantially changing the beneficiary/victim structure and potentially reclassifying elements of the arrangement as rope rather than scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_of_the_governed_scope_ambiguity, conceptual, 'Whether ''the governed'' in this reading''s core claim is scoped to citizens or to all materially affected persons.').

omega_variable(
    scaffold_permanence_risk,
    'Is the deliberative infrastructure this reading builds genuinely transitional toward a more inclusive settlement, or is it becoming a permanent governance architecture that will never complete a declared sunset?',
    'Track whether concrete sunset milestones (e.g., extension of deliberative standing to currently excluded populations, international deliberative mechanisms for globally-deployed AI) are ever specified and met, versus the infrastructure persisting indefinitely without such milestones being named.',
    'If no sunset milestones are ever specified or approached, the scaffold classification becomes doubtful and the arrangement drifts toward a tangled_rope or piton reading over time as its provisional justification hardens into permanent status without re-examination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_permanence_risk, empirical, 'Whether the declared transitional scaffold is on a real path to sunset or is calcifying into permanent infrastructure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 24, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 12, 0.28).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 24, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings decomposing the natural-language kernel 'AI governance legitimacy' per the epsilon-invariance principle. Each sibling reading (magisterial_subsidiarity, technocratic_optimization, market_libertarian) authors its own epsilon, beneficiary/victim structure, and claimed_type from its own normative premises. This democratic_pluralist_reading is authored at moderate epsilon (0.40) reflecting genuine but incompletely inclusive procedural coordination; siblings are expected to diverge substantially (e.g., the technocratic reading is expected to show higher extraction concentrated on non-expert populations, the magisterial reading redistributes agenda-setting authority to the Magisterium). All four are linked bidirectionally via affects_constraints to preserve the constraint family structure for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__democratic_pluralist_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
