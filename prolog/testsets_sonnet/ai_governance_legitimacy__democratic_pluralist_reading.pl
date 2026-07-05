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
 *   domain: Theological Ethics / Technology Governance / Political Theology
 *
 * SUMMARY:
 *   This story instantiates one reading within the contested 'AI governance
 *   legitimacy' kernel: the democratic-pluralist reading, which locates
 *   legitimacy in inclusive public reason and consent of the governed rather
 *   than in any single tradition's authoritative interpretation. It accepts
 *   the substantive dignity claims found in encyclical-derived Catholic
 *   Social Doctrine but denies that the Magisterium (or any technocratic or
 *   market tradition) holds interpretive monopoly over how those claims
 *   should govern AI. The constraint is authored as a scaffold: it builds
 *   participatory infrastructure (elections, courts, consultation processes)
 *   explicitly meant to be a transitional, revisable mechanism for
 *   adjudicating value pluralism, not a permanent settlement. Sibling
 *   readings of the same kernel — magisterial_subsidiarity_reading,
 *   technocratic_optimization_reading, market_libertarian_reading — are NOT
 *   part of this constraint; each is authored as its own file with its own
 *   epsilon and stakeholder structure. Where this reading's beneficiaries
 *   (organized civil society, rights-bearing minorities within functioning
 *   democracies) and victims (the unorganized, the disenfranchised, and
 *   populations under authoritarian rule) diverge sharply from the sibling
 *   readings' beneficiary/victim structures, that divergence is the point of
 *   decomposition, not a defect to reconcile.
 *
 * KEY AGENTS:
 *   - democratic_institutions: agenda_setter (institutional/constrained) — designs and administers the deliberative machinery
 *   - civil_society_organizations: primary beneficiary (organized/mobile) — gains voice and standing under pluralist framing
 *   - minority_rights_holders: beneficiary (moderate/constrained) — protected by rights review built into the scaffold
 *   - populations_excluded_from_deliberation: primary payer (powerless/trapped) — bears costs of governance set without their input
 *   - populations_under_authoritarian_regimes: payer (powerless/trapped) — no domestic deliberative machinery exists to carry the legitimacy claim
 *   - religious_and_technocratic_traditions: excluded from final authority — demoted to 'one voice among many'
 *   - political_theology_scholars: analytical observer — documents the gap between claimed and actual inclusivity
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
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "Democratic-Pluralist Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "Theological Ethics / Technology Governance / Political Theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, 'ba6ddd9e-9c28-4c00-952f-bc3d04310778').
narrative_ontology:cs_kernel_codification('ba6ddd9e-9c28-4c00-952f-bc3d04310778', distributed).
narrative_ontology:cs_authority_grounding('ba6ddd9e-9c28-4c00-952f-bc3d04310778', distributed).
narrative_ontology:cs_reading_relation('ba6ddd9e-9c28-4c00-952f-bc3d04310778', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba6ddd9e-9c28-4c00-952f-bc3d04310778', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba6ddd9e-9c28-4c00-952f-bc3d04310778', ai_governance_legitimacy__market_libertarian_reading, influences).
narrative_ontology:cs_axiom('ba6ddd9e-9c28-4c00-952f-bc3d04310778', foundational, no_tradition_holds_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_tradition_holds_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('ba6ddd9e-9c28-4c00-952f-bc3d04310778', no_tradition_holds_interpretive_monopoly, conventional).
narrative_ontology:cs_axiom('ba6ddd9e-9c28-4c00-952f-bc3d04310778', foundational, legitimacy_derives_from_consent_of_the_governed).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_consent_of_the_governed, holdable).
narrative_ontology:cs_axiom_grounding('ba6ddd9e-9c28-4c00-952f-bc3d04310778', legitimacy_derives_from_consent_of_the_governed, deontological).
narrative_ontology:cs_reference_frame('ba6ddd9e-9c28-4c00-952f-bc3d04310778', pre_deliberative_pluralism_settlement).
narrative_ontology:cs_drift_state('ba6ddd9e-9c28-4c00-952f-bc3d04310778', contemporary_ai_governance_debates, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ba6ddd9e-9c28-4c00-952f-bc3d04310778', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, populations_excluded_from_deliberation).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, public_reason_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, consent_of_the_governed_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures, electoral commissions, and judicial bodies design and administer the deliberative processes (hearings, notice-and-comment, referenda, judicial review) through which AI governance principles are supposed to be legitimated. They set procedural rules, adjudicate disputes about who gets a seat at the table, and can revise the framework through ordinary political and legal channels. Their authority is contingent on continued electoral and judicial legitimacy, not on doctrinal warrant.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).

% NGOs, advocacy coalitions, professional associations, and public-interest litigators gain standing and voice under the pluralist framework that they would not have under a magisterial or technocratic reading. They participate in consultations, file amicus briefs, run public campaigns, and shape the substantive content of AI governance norms. Their exit option is comparatively strong: they can shift jurisdictions, coalitions, or advocacy targets.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, agenda_setter).

% Groups whose interests would be discounted under a purely majoritarian or purely technocratic process benefit from constitutional and judicial protections built into the deliberative scaffold (rights review, minority-veto points, anti-discrimination enforcement). They depend on courts and constitutional guarantees remaining robust; their exit from the polity itself is limited, so the quality of the deliberative process matters directly to their material situation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    moderate, biographical, constrained, national).

% Non-citizens, disenfranchised residents, populations without digital access or civic literacy, and communities structurally underrepresented in notice-and-comment or electoral processes bear the costs of AI systems whose governing principles were set without their input. The pluralist framework claims their legitimacy but the actual deliberative process routes around them; they cannot buy their way in and have no alternative forum with comparable authority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, populations_excluded_from_deliberation, payer,
    powerless, biographical, trapped, national).

% In states without functioning electoral accountability or judicial independence, the democratic-pluralist legitimacy claim has no local machinery to run on. AI systems governed 'in their name' by the framework's international extensions (soft-law instruments, multilateral guidelines invoking public reason) are administered by elites who face no domestic deliberative check. These populations cannot exit their political system and have no access to the deliberative infrastructure the reading claims as its legitimating mechanism.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes, payer,
    powerless, generational, trapped, national).

% The Magisterium and technical-expert bodies are treated as contributing 'one voice among many' rather than holding interpretive authority. They retain standing to speak in the public square but are structurally denied any veto or privileged claim over final governance outcomes; the pluralist reading's founding move is precisely to demote their traditions to inputs rather than arbiters.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, religious_and_technocratic_traditions, excluded,
    organized, civilizational, mobile, global).

% Study how the democratic-pluralist reading manages the tension between procedural inclusivity and substantive outcomes, and how it compares to sibling readings that ground legitimacy in doctrine, expertise, or market exchange. They document who is actually heard in 'inclusive' processes versus who is nominally included.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, political_theology_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__democratic_pluralist_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__democratic_pluralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedural mechanism — elections, courts, public consultation — through which people with irreducibly different values (religious, secular, technocratic, libertarian) can arrive at AI governance rules they did not individually author but can regard as legitimately theirs, without requiring prior agreement on ultimate values.
% TRANSFER_FUNCTION: Moves interpretive authority away from any single tradition (Magisterium, technical experts, market actors) and distributes it across democratic institutions and organized civil society; in practice this also moves effective influence toward whichever groups are already organized, literate in the process, and resourced enough to participate, and away from groups that are unorganized, non-citizen, or living under non-democratic regimes.
% ABSENT_VOICES: Populations excluded from deliberation by poverty, disenfranchisement, or lack of civic infrastructure, and entire populations under authoritarian governments, would object that the 'consent of the governed' the framework invokes was never actually solicited from them. They are structurally outside the rooms where the deliberation happens and have no comparable alternative forum. Religious and technocratic traditions would separately object that their interpretive claims have been demoted to mere inputs without argument on the merits.
% DISAPPEARANCE_RATIONALE: Democratic institutions, civil society organizations, and rights-review courts would say the deliberative scaffold's disappearance would rearrange the world severely: AI governance would default to whichever tradition (doctrinal, technocratic, or market) fills the vacuum, and minority protections built into the deliberative process would lose their institutional home. Excluded populations and authoritarian-state subjects would say comparatively little would change for them, since the deliberative infrastructure was never actually operative in their situation.
% FOUNDING_PROBLEM: No single tradition — religious, technocratic, or market — commands universal assent as the authority for governing a technology whose effects cross all of them; the founding problem is how to legitimate binding governance choices under genuine, persistent value pluralism without simply picking a winner among traditions.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists and comparative-law scholars outside the civil-society coalitions that benefit from this reading corroborate that value pluralism in technology governance is empirically real and unresolved (documented disagreement among religious, technocratic, and market constituencies on AI ethics standards). However, human-rights monitors and democracy-index researchers — also outside the benefiting coalition — corroborate that the deliberative machinery this reading relies on is absent or captured in the majority of jurisdictions where AI systems are actually deployed, meaning the solution's coverage is much narrower than its universalist framing suggests.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.40 at interval end) reflecting the expected structural delta: the framework is not a naked extraction mechanism, but its universalist 'consent of the governed' claim quietly transfers real interpretive authority toward whichever actors are already organized and civically resourced, at the expense of the unorganized and the non-citizen. Suppression is moderate-low (0.32) because the mechanism does not coerce belief, but active enforcement — electoral accountability, judicial review, civil liberties litigation — is real and required to keep the scaffold functioning; without it the framework collapses into whichever tradition fills the vacuum. Theater ratio rises modestly over the interval (0.15 to 0.28) as consultation processes proliferate faster than their capacity to actually incorporate excluded voices, a mild but real Goodhart drift where procedural compliance (holding hearings, publishing comment periods) substitutes for substantive inclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil society organizations and rights-holders within functioning democracies sit near the beneficiary end: they gain standing, voice, and protective machinery they would lack under a magisterial or purely technocratic settlement, and they retain meaningful exit (coalition-shifting, jurisdiction-shopping). Democratic institutions themselves are agenda-setters administering the scaffold rather than pure beneficiaries — they derive legitimacy from operating it well, and can be held accountable through the very mechanisms they administer. Populations excluded from deliberation and populations under authoritarian regimes sit near the full-target end: the constraint's legitimating claim is made in their name ('consent of the governed') while the actual deliberative machinery is structurally unavailable to them — trapped exit options and powerless structural position compound the disjunction between claim and access.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine, persistent value pluralism about how technology should be governed — remains live; no tradition has achieved actual universal assent. This blocks a premature mandatrophy verdict: the scaffold is not simply an obsolete mandate persisting on inertia. But the founding_problem_status is authored as 'live' rather than 'dead' cautiously, because the corroboration is split — political theorists confirm the pluralism problem is real, while human-rights monitors confirm the solution's actual reach (functioning deliberative machinery) covers a minority of the populations the framework claims to legitimate governance for. This is the scaffold/snare boundary condition the six-questions interview is built to surface: a genuinely live founding problem with a coverage gap wide enough that its 'consent of the governed' claim is, for a majority of affected populations, not currently backed by any operative mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_reason_inclusivity_gap,
    'Does the democratic-pluralist reading''s ''inclusive public reason'' actually incorporate the interests of the unorganized and disenfranchised, or does it systematically privilege whichever civil society actors are already resourced enough to participate in consultation processes?',
    'Comparative study of who actually appears in AI governance consultation records (submitter demographics, organizational resources) versus the populations formally claimed as represented by ''consent of the governed'' language.',
    'If participation is systematically skewed toward already-organized actors, the reading''s legitimacy claim is weaker than authored and the extractiveness/suppression metrics should be revised upward for excluded populations; if participation is genuinely broad-based, the scaffold classification and moderate epsilon are well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_reason_inclusivity_gap, empirical, 'Whether inclusive public reason is actually inclusive in practice or systematically favors organized incumbents.').

omega_variable(
    reading_selection_under_determination,
    'Is the democratic-pluralist framing the only defensible reading of the underlying encyclical-derived dignity claims, or does the choice to read them through a procedural-legitimacy lens (rather than a substantive-doctrinal or technocratic lens) itself reflect a prior commitment this story imports rather than derives?',
    'Compare this reading''s classification against the sibling readings (magisterial_subsidiarity_reading, technocratic_optimization_reading, market_libertarian_reading) once all four are authored: if the epsilon values and beneficiary/victim structures diverge sharply by reading choice alone, the kernel is confirmed genuinely contested rather than resolvable by better evidence.',
    'Confirms that this constraint''s classification (scaffold, moderate epsilon) is reading-specific rather than a settled fact about ''AI governance legitimacy'' as a natural-language label; supports the decomposition strategy already applied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether the procedural-legitimacy framing is itself a substantive prior rather than a neutral default reading of the kernel.').

omega_variable(
    authoritarian_context_applicability,
    'Can a legitimacy framework grounded in electoral accountability and judicial review meaningfully claim to legitimate AI governance for populations living under regimes where neither institution functions, or does the framework''s universalist language (''consent of the governed'') misrepresent its actual jurisdictional reach?',
    'Map the framework''s actual enforcement mechanisms (electoral accountability, judicial review, civil liberties protections) against the set of jurisdictions where AI systems are deployed and assess coverage.',
    'A large coverage gap would suggest the framework''s legitimacy claim functions as ideology for populations outside functioning democracies — supporting a harder read (closer to snare) for that subset of affected populations, even while the scaffold classification holds where the machinery is actually operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authoritarian_context_applicability, empirical, 'Whether the framework''s legitimacy claim overstates its actual jurisdictional applicability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 20, 0.26).
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
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__democratic_pluralist_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the ai_governance_legitimacy kernel, decomposed per the epsilon-invariance principle: measuring 'AI governance legitimacy' through a procedural-democratic lens yields a moderate, scaffold-shaped constraint (this file), while measuring it through magisterial-doctrinal, technocratic-expertise, or market-exchange lenses yields structurally distinct constraints with different beneficiary/victim sets and different epsilon values. Each reading is authored as its own file with its own claimed_type and metrics; none averages or arbitrates among the others. The four files together constitute the full kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
