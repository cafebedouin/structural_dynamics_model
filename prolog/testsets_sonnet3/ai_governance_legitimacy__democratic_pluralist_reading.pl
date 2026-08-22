% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: AI Governance Legitimacy — Democratic Pluralist Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This story instantiates the democratic-pluralist reading of the
 *   ai_governance_legitimacy kernel: the claim that legitimate AI governance
 *   derives from inclusive public deliberation and electoral consent, with no
 *   single tradition — religious or technocratic — holding interpretive
 *   monopoly. This reading accepts the encyclical's underlying dignity claims
 *   as one contribution among many but explicitly denies the Magisterium (or
 *   any expert body, or market logic) a privileged interpretive seat. The
 *   constraint functions as a scaffold: it builds participatory
 *   infrastructure (public comment processes, judicial review, electoral
 *   accountability mechanisms) intended to be the transitional means by which
 *   contested AI governance norms get settled, not a permanent doctrinal
 *   endpoint. It is authored as a distinct constraint from the sibling
 *   readings (magisterial_subsidiarity, technocratic_optimization,
 *   market_libertarian) per the ε-invariance principle — each reading has its
 *   own beneficiary/victim structure and its own extraction profile, and none
 *   averages over the others.
 *
 * KEY AGENTS:
 *   - democratic_institutions: agenda_setter/beneficiary (institutional/constrained) — administers the deliberative apparatus
 *   - civil_society_organizations: beneficiary (organized/mobile) — gains standing within the process
 *   - minority_rights_holders: beneficiary (moderate/constrained) — protected conditionally via judicial review
 *   - populations_under_authoritarian_regimes: payer (powerless/trapped) — no functioning deliberative channel exists for them
 *   - deliberatively_excluded_communities: payer (powerless/constrained) — formally included, substantively unheard
 *   - magisterial_authority_claimants: excluded (organized/constrained) — denied privileged interpretive standing
 *   - technology_firms: payer/beneficiary (powerful/mobile) — compliance cost vs. lobbying access
 *   - analytical_observers: observer (analytical) — assesses whether deliberation is genuine or theatrical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.38).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "AI Governance Legitimacy — Democratic Pluralist Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, 'a8932592-3147-4031-9737-e9c376cb432b').
narrative_ontology:cs_kernel_codification('a8932592-3147-4031-9737-e9c376cb432b', distributed).
narrative_ontology:cs_authority_grounding('a8932592-3147-4031-9737-e9c376cb432b', distributed).
narrative_ontology:cs_reading_relation('a8932592-3147-4031-9737-e9c376cb432b', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('a8932592-3147-4031-9737-e9c376cb432b', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8932592-3147-4031-9737-e9c376cb432b', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('a8932592-3147-4031-9737-e9c376cb432b', foundational, no_single_tradition_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_single_tradition_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('a8932592-3147-4031-9737-e9c376cb432b', no_single_tradition_interpretive_monopoly, conventional).
narrative_ontology:cs_axiom('a8932592-3147-4031-9737-e9c376cb432b', foundational, consent_of_the_governed_grounds_legitimacy).
narrative_ontology:cs_axiom_status(consent_of_the_governed_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a8932592-3147-4031-9737-e9c376cb432b', consent_of_the_governed_grounds_legitimacy, deontological).
narrative_ontology:cs_reference_frame('a8932592-3147-4031-9737-e9c376cb432b', post_westphalian_procedural_democratic_legitimacy).
narrative_ontology:cs_drift_state('a8932592-3147-4031-9737-e9c376cb432b', contemporary_ai_governance_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a8932592-3147-4031-9737-e9c376cb432b', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, deliberatively_excluded_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, technology_firms).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, technology_firms).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, public_reason_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, pluralist_non_monopoly_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures, electoral commissions, and courts administer the deliberative process through which AI governance principles are debated, amended, and legitimated. They set the procedural rules (transparency requirements, public comment periods, judicial review standards) and derive legitimacy from being seen as accountable to the electorate rather than to any single tradition.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, beneficiary).

% NGOs, advocacy groups, and public-interest coalitions gain standing and voice within the deliberative apparatus — they can petition, litigate, testify, and mobilize public opinion. Their influence depends on the deliberative infrastructure remaining open; they benefit directly from the pluralist framing that denies any single authority a veto over the outcome.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, national).

% Groups whose interests would be discounted under a majoritarian or purely technocratic process gain protection through judicial review and constitutional rights guarantees layered onto the deliberative structure. Their benefit is conditional on courts and constitutional norms actually holding — where those erode, the protection is theoretical.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    moderate, biographical, constrained, national).

% Live under governments that face no electoral accountability and permit no genuine public deliberation on AI systems used for surveillance, scoring, or censorship. The democratic-pluralist framing offers them no functioning legitimacy mechanism at all — they are governed by AI systems shaped entirely outside any deliberative process, while the reading's global rhetoric implies universal applicability it cannot deliver where it matters most for them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes, payer,
    powerless, biographical, trapped, national).

% Within nominally democratic states, groups lacking resources, literacy, language access, or institutional connections to participate in public comment periods, expert hearings, or advocacy coalitions are formally included but substantively absent. Their interests are folded into aggregate outcomes without their direct voice, even as the process is described as inclusive.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, deliberatively_excluded_communities, payer,
    powerless, biographical, constrained, national).

% Church authorities and religious institutions asserting that AI governance principles derive from authoritative doctrinal interpretation are treated as one voice among many rather than as a privileged interpreter. They participate in public deliberation but their claim to unique interpretive authority over dignity and the common good is structurally denied standing within this reading.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_authority_claimants, excluded,
    organized, civilizational, constrained, global).

% Face compliance costs and regulatory uncertainty from evolving, contested deliberative outcomes rather than a single stable authority to satisfy. They benefit from being able to lobby within the pluralist process and from the absence of a fixed doctrinal ceiling on permissible technology, but bear the cost of shifting political majorities and litigation risk.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technology_firms, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, technology_firms, beneficiary).

% Political theorists and comparative governance scholars assess whether the deliberative infrastructure actually achieves inclusive public reason or merely launders elite consensus through participatory theater, comparing outcomes across jurisdictions with varying degrees of genuine democratic capacity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedural mechanism — public deliberation, electoral accountability, judicial review — through which people holding genuinely incompatible values about AI (religious, secular, market-oriented, technocratic) can arrive at binding governance principles without any faction needing to first defeat the others' worldview.
% TRANSFER_FUNCTION: Moves interpretive authority away from any single tradition (Magisterium, technical expert class, or market actors) and distributes it across democratic institutions and civil society; moves compliance burden onto technology firms and moves protective standing toward organized minority and civil-society interests capable of participating in the process.
% ABSENT_VOICES: Populations under authoritarian regimes have no functioning deliberative channel at all and are simply outside the reading's operative scope despite its universalist rhetoric. Within democracies, deliberatively excluded communities lacking resources or access are formally invited but substantively unheard. The Magisterium and other claimants to singular interpretive authority are present as participants but denied any privileged voice, which is exactly what this reading is built to ensure.
% DISAPPEARANCE_RATIONALE: If democratic deliberative legitimacy for AI governance vanished, the vacuum would be filled by whichever authority claim currently has the most concentrated power — likely technocratic expert bodies or dominant market actors — reallocating standing away from civil society and minority-rights advocates and eliminating the procedural check that judicial review and electoral accountability currently provide.
% FOUNDING_PROBLEM: Faced with rapid AI deployment and competing claims to moral authority over its governance (religious doctrine, technical expertise, market ideology), this reading was built to prevent any single tradition from monopolizing legitimacy and to ground governance instead in processes all affected parties could in principle contest and revise.
% FOUNDING_PROBLEM_CORROBORATION: Comparative governance scholars and international human rights bodies outside the civil-society beneficiary set corroborate that the underlying problem — contested authority claims over AI's normative constraints — remains unresolved and that deliberative capture, disinformation, and unequal participatory access are documented, ongoing failure modes rather than solved history.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness sits at 0.40, within the expected moderate band (0.35–0.45): this reading redistributes interpretive authority rather than concentrating rents, but it does impose real costs on those excluded from deliberation (authoritarian-regime populations, resource-poor communities) who receive none of the participatory benefits while nominally being governed under a 'consent of the governed' framework that does not reach them. Suppression (0.38) reflects that participation is formally open but access barriers (resources, language, institutional connections) function as soft suppression for the deliberatively excluded. Theater ratio (0.30) captures that some fraction of 'public deliberation' is performative consultation that ratifies decisions already made by better-resourced participants, rising modestly over the interval as deliberative processes institutionalize and risk ossifying into consultation theater. Accessibility collapse is moderate (0.35) — genuine alternative governance framings (doctrinal, technocratic, market) remain visible and contestable within the process, which is the reading's own claimed virtue. Resistance (0.50) is substantial: both excluded populations and rival authority claimants (Magisterium, technocratic bodies) actively contest the pluralist framing's adequacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic institutions and civil society organizations sit near the beneficiary end: they administer or gain voice within the process. Minority rights holders benefit conditionally — real protection when judicial review functions, illusory where it doesn't. Populations under authoritarian regimes and deliberatively excluded communities sit near the full-target end: they bear the costs of a legitimacy framework that assumes functioning deliberative access they do not have, while still being nominally subject to its governance claims. Technology firms are dual-positioned: payers of compliance cost, beneficiaries of lobbying access and the absence of a single fixed doctrinal ceiling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing any single tradition from monopolizing AI governance legitimacy — remains live wherever genuine pluralism of values exists and deliberative capacity is real. It risks mandatrophy specifically where deliberative infrastructure has ossified into consultation theater that ratifies elite consensus while claiming inclusive process, or where it is invoked to legitimate governance over populations (authoritarian-state subjects) who have no actual access to the deliberation being claimed on their behalf. Because it is authored as a scaffold with a sunset clause (the deliberative infrastructure is meant to yield settled, revisable norms, not stand as permanent doctrine), a rising theater_ratio trend is the leading indicator to watch for drift toward performative rather than functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberative_access_reality_gap,
    'Does the formally inclusive public deliberation this reading relies on actually reach the populations it claims to legitimate governance for, or does unequal access to participation (resources, language, institutional connection) reproduce the same exclusion the pluralist framing claims to correct?',
    'Empirical study of who actually participates in public comment periods, expert hearings, and advocacy coalitions shaping AI governance, compared against the demographic and political composition of populations nominally governed by the resulting norms.',
    'If access is systematically unequal, the reading''s legitimacy claim (consent of the governed via inclusive public reason) is substantially undermined and the constraint''s effective type drifts toward tangled_rope (real coordination function for the well-connected, extraction from the excluded) rather than a clean scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_access_reality_gap, empirical, 'Whether deliberative inclusion is substantive or nominal.').

omega_variable(
    authoritarian_scope_limit,
    'Is this reading a universal claim about AI governance legitimacy (applying everywhere, including under authoritarian regimes where it structurally cannot function) or an implicitly scoped claim limited to functioning democracies?',
    'Examine whether proponents of the reading explicitly restrict its applicability or continue to invoke it as grounds for critiquing AI governance in non-democratic contexts despite the absence of any deliberative mechanism there.',
    'If the reading is invoked universally without scope restriction, populations under authoritarian regimes are victims of a legitimacy framework applied to them in name only — raising effective extraction and suppression for that group beyond what is captured by treating the claim as merely aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authoritarian_scope_limit, conceptual, 'Whether the reading''s universalist rhetoric matches its actual operative scope.').

omega_variable(
    reading_kernel_framing_choice,
    'The ai_governance_legitimacy kernel could be framed as a contest between four INSTITUTIONAL AUTHORITY claims (as authored here) or as a contest between underlying VALUE COMMITMENTS (dignity-centered vs. efficiency-centered vs. liberty-centered) that cut across the institutional framing. The institutional framing was chosen because the source material explicitly poses the contest as ''no single tradition holds interpretive monopoly'' — a claim about who gets to interpret, not merely what values apply.',
    'Compare classification outcomes if the kernel were re-authored around value commitments rather than institutional authority — would the democratic_pluralist_reading''s beneficiary/victim structure change materially?',
    'Under the value-commitment framing, this reading might classify closer to rope (pure value-pluralism coordination) rather than scaffold, since ''balancing diverse values'' reads more like an ongoing coordination function than a transitional structure. The scaffold classification here rests on treating deliberative infrastructure as meant to produce settled, revisable norms rather than a permanent value-balancing steady state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_framing_choice, conceptual, 'Alternative framing of the kernel around values rather than institutional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__democratic_pluralist_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the ai_governance_legitimacy kernel, each authored as a separate constraint per the ε-invariance principle. democratic_pluralist_reading (this file, scaffold, ε≈0.40) denies any single tradition interpretive monopoly and grounds legitimacy in deliberative process; magisterial_subsidiarity_reading grounds legitimacy in Magisterial doctrinal authority; technocratic_optimization_reading grounds legitimacy in expert-optimized welfare maximization; market_libertarian_reading grounds legitimacy in voluntary exchange and property rights. All four readings share the same underlying kernel text (the encyclical's claims about AI and human dignity) but instantiate structurally distinct constraints with different beneficiaries, victims, and enforcement mechanisms. Network edges connect all sibling readings bidirectionally in spirit; this file lists the outbound edges to preserve constraint-family traceability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
