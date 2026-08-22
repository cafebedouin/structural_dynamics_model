% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Dual-Sourced Sovereign Legitimacy: Inherited Ceremony, Delegated Power, Constitutional Mediation
 *   domain: political philosophy/constitutional theory
 *
 * SUMMARY:
 *   A constitutional order runs on two legitimacy sources at once: a
 *   hereditary officeholder supplies ceremonial headship, continuity, and
 *   non-partisan symbolism, while elected officials supply operative
 *   political power, with constitutional law and its courts policing the line
 *   between the two spheres. The settlement is presented as the stable middle
 *   between personal rule and pure popular sovereignty. Structurally it
 *   coordinates a real problem (how a polity keeps a unity symbol and a
 *   neutral headship while making power accountable) while transferring real
 *   resources (the crown grant, immunities) and foreclosing both pure-form
 *   alternatives through entrenchment and establishment consensus. KEY AGENTS
 *   (by structural relationship): hereditary_monarch — primary beneficiary
 *   (institutional/identity_locked), collects status, income, and immunity;
 *   elected_officials — secondary beneficiary and co-administrator
 *   (institutional/mobile), exercises delegated power and votes the grant;
 *   constitutional_judiciary — agenda-setter (institutional/constrained),
 *   adjudicates the boundary; absolutist_factions — payer
 *   (powerless/identity_locked), bears foreclosure of pure hereditary rule;
 *   republican_movements — payer (organized/trapped), bears foreclosure of
 *   abolition; taxpayers_funding_crown — diffuse contributor
 *   (organized/constrained); constitutional_scholars — analytical observer.
 *   FAMILY NOTE: 'legitimate authority' decomposes into three structurally
 *   distinct claims; this file authors only the hybrid reading, whose epsilon
 *   reflects compromise costs — the monarchical sibling concentrates
 *   extraction far more heavily on subjects, while the republican sibling has
 *   a different victim set entirely (no inherited officeholder to subsidize,
 *   but different exclusion costs around headship selection).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.38).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.35).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Dual-Sourced Sovereign Legitimacy: Inherited Ceremony, Delegated Power, Constitutional Mediation").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political philosophy/constitutional theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '85a6229d-f909-49af-b11f-9b218786b402').
narrative_ontology:cs_kernel_codification('85a6229d-f909-49af-b11f-9b218786b402', fixed_text).
narrative_ontology:cs_authority_grounding('85a6229d-f909-49af-b11f-9b218786b402', lineage).
narrative_ontology:cs_interpretation_layer_present('85a6229d-f909-49af-b11f-9b218786b402').
narrative_ontology:cs_reading_relation('85a6229d-f909-49af-b11f-9b218786b402', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('85a6229d-f909-49af-b11f-9b218786b402', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_axiom('85a6229d-f909-49af-b11f-9b218786b402', foundational, dual_source_legitimacy).
narrative_ontology:cs_axiom_status(dual_source_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('85a6229d-f909-49af-b11f-9b218786b402', dual_source_legitimacy, conventional).
narrative_ontology:cs_axiom('85a6229d-f909-49af-b11f-9b218786b402', foundational, inherited_ceremonial_office_legitimate).
narrative_ontology:cs_axiom_status(inherited_ceremonial_office_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('85a6229d-f909-49af-b11f-9b218786b402', inherited_ceremonial_office_legitimate, conventional).
narrative_ontology:cs_reference_frame('85a6229d-f909-49af-b11f-9b218786b402', dual_source_constitutional_settlement).
narrative_ontology:cs_drift_state('85a6229d-f909-49af-b11f-9b218786b402', contemporary_boundary_dispute_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('85a6229d-f909-49af-b11f-9b218786b402', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_factions).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, taxpayers_funding_crown).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, taxpayers_funding_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherits the head-of-state office by birth order and holds it for life. Receives an annual public grant, residences, security, and legal immunities; performs ceremonial duties — opening parliament, receiving ambassadors, conferring honors — and holds reserve powers that convention bars from political use. Cannot relinquish the office without breaking the dynastic continuity that gives the whole arrangement its meaning; abdication resolves a crisis only by substituting the heir.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, identity_locked, national).

% Win office through periodic elections and exercise the operative powers of government: legislation, budget, appointments, war. Their authority is renewed or withdrawn by voters, yet they share public legitimacy space with an unelected head of state whose signature and presence still validate their acts. They also administer the settlement itself — voting the crown grant, adjusting succession rules, and referring boundary questions to the courts.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter).

% Adjudicates where inherited authority ends and delegated authority begins: reviewing exercises of prerogative, policing the crown's neutrality, deciding whether acts done in the sovereign's name bind the government. Tenure protects independence; precedent binds them to prior boundary rulings even when the underlying settlement strains under a new dispute.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Hold that the head of state should govern personally as of inherited right, and read the delegation of political power to elected bodies as a usurpation awaiting reversal. Organized remnants publish, lobby, and occasionally contest elections; their program has no viable legislative path, and abandoning the claim would dissolve the dynastic loyalty that organizes them.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_factions, payer,
    powerless, generational, identity_locked, national).

% Campaign to replace the inherited office with an elected or appointed head of state, arguing that hereditary public office is indefensible in principle. They contest referenda, publish costings of the royal household, and introduce abolition bills that repeatedly fail against cross-party establishment consensus and steep amendment thresholds; the movement persists because its goal stays blocked, not because it has been answered.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_movements, payer,
    organized, biographical, trapped, national).

% Fund the head-of-state institution through general taxation — grants, security, upkeep of residences — and receive in return a non-partisan symbol of continuity that they did not individually choose and cannot individually decline to support. They can contest the size of the grant at elections, but not the existence of the office short of constitutional upheaval.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, taxpayers_funding_crown, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, taxpayers_funding_crown, payer).

% Analyze the settlement from outside its operation: tracing how boundary disputes were resolved, comparing hybrid systems across countries, and testing whether the founding justifications still describe current practice. They shape doctrine indirectly through citation and critique, collect nothing from the arrangement, and bear none of its costs.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the transition-and-continuity problem of post-personal-rule governance: preserves an inherited, non-partisan symbol of national unity while relocating operative political power to delegated, electorally accountable institutions, with constitutional law allocating which decisions each source may make so the two authorities do not contest the same act.
% TRANSFER_FUNCTION: Moves material support (annual grants, residences, immunities) from the general taxpayer to the hereditary officeholder; moves formal validation and reserve authority into the orbit of elected officials' disposal; moves boundary adjudication to the constitutional judiciary.
% ABSENT_VOICES: Pure-form advocates sit at the margins of the conversation: absolutists, whose restoration program has no legislative path, and republicans, whose abolition bills never reach a binding vote despite sustained polling support in several hybrid systems. Also absent are peoples who never consented to either source — indigenous nations and conquered populations whose subjection the crown's continuity sometimes still memorializes — who hold no seat in the settlement's amendment processes.
% DISAPPEARANCE_RATIONALE: If the dual-sourcing vanished overnight, every head-of-state function would need reassignment at once: either the monarch resumes personal government (a legitimacy crisis for the elected order) or a republic drafts itself under emergency conditions (a legitimacy crisis for the inherited order). The boundary-adjudication machinery would lose its object, the crown grant would terminate or convert, and every act formerly validated by the sovereign's signature would need a new validating source.
% FOUNDING_PROBLEM: Managing the passage from personal, hereditary rule to delegated democratic governance without civil war or legitimacy collapse — retaining a continuity symbol and a neutral headship while making power answerable to electorates.
% FOUNDING_PROBLEM_CORROBORATION: Defenders of the settlement (government sources, monarchist institutes) attest the founding problem is permanently live: every state needs a non-partisan headship, and the hybrid supplies it at known cost. Constitutional historians document the transition-era bargains (revolutionary settlements, restoration compromises, post-authoritarian impositions) as solutions to problems specific to their centuries, and republican campaign materials outside the benefiting parties argue the transition is complete and the arrangement now persists by elite bargain and inertia. Corroboration exists on both sides, which is why the status is contested rather than live or dead.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-to-moderate (0.38): the compulsory crown grant and the foreclosure of pure-form alternatives are real costs, but they are bounded, publicly accounted, and exchanged for a coordination good (neutral headship, continuity) that the settlement's own terms treat as the point. Suppression (0.35) is conventional-judicial rather than coercive: the arrangement is held up by amendment thresholds, establishment consensus, and precedent rather than by force, though the reserve powers remain a dormant coercive backstop. Theater (0.42) needs careful reading: ceremony is functional within this settlement — the dignified work IS the product — but the purely performative share grows as operative functions migrate to elected institutions, and the rising series tracks that migration, not decay. Accessibility collapse is low (0.35): elected-headship models are instantiated in neighboring states and personally comprehensible, so understanding the hybrid does not collapse its alternatives. Resistance (0.45) is persistent and organized on the republican side, marginal and nostalgic on the absolutist side. The measurement series run on one shared six-point grid: extractiveness dips through the accountability-reform era then rises mildly as boundary-dispute costs accumulate; suppression declines through consolidation and normalizes; theater climbs steadily with the ceremonialization of the crown's remaining activity.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same structure differently. From the monarch's position the settlement is protection: status, income, and immunity preserved precisely because the dangerous powers went unused and away. From the elected officials' position it is a convenient division of labor — a validation source they do not pay for politically and a scapegoat-free headship above the fray. From the judiciary's position it is a tractable, if recurring, adjudication docket. From the purist seats it is usurpation (absolutists) or an indefensible hereditary subsidy (republicans). The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch sits nearest the beneficiary pole: the grant flows in unconditionally, and identity-lock removes even the theoretical exit, deepening the subsidy. Elected officials derive low directionality as beneficiaries, but conditionally — their gain (office) is renewed or withdrawn by voters, unlike the monarch's. Taxpayers sit near symmetric: they pay the grant and receive the symbol, with no individual opt-out. Absolutist and republican movements sit near the target pole: both bear the arrangement's characteristic cost, which is foreclosure — their preferred constitutions are unreachable inside the framework, by entrenchment rather than by argument. The judiciary is the symmetric administrator: it neither captures nor pays, it allocates.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two symmetrical mislabels. Calling the arrangement pure extraction ignores its genuine coordination function — neutral headship and continuity are real products that a pure republic must reconstruct deliberately and a pure monarchy never supplies accountably. Calling it pure coordination ignores the compulsory transfer and the entrenchment costs borne by taxpayers and purists. The mandatrophy question is live: if the founding problem (managing the post-personal-rule transition) is dead and the settlement persists by inertia, the arrangement drifts toward theatrical maintenance — which is why the theater_ratio series is the early-warning instrument here, and why the founding-problem status is authored contested rather than resolved. No mandatrophy_resolved flag is declared: the mandate's status is precisely what the corpus is measuring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading of the sovereign_legitimacy kernel (constitutional_hybrid_reading); do corpus consumers treat ''sovereign legitimacy'' as a single constraint and average over the monarchical and republican readings?',
    'Compile the sibling readings as separate stories and compare epsilon, beneficiary/victim sets, and computed types across the three files; divergence in all three confirms the decomposition was necessary.',
    'Conflation would average incompatible structures: the hybrid''s low-to-moderate extraction would be inflated by the monarchical reading''s concentrated extraction and its victim set confused with the republican reading''s, producing a verdict true of no actual arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer structure: one of three readings of the sovereign_legitimacy kernel; siblings are separate constraints.').

omega_variable(
    boundary_dispute_structural_status,
    'Are recurring boundary disputes (reserve powers, dissolution and prorogation, royal assent, the crown''s neutrality) incidental friction that the mediation layer absorbs, or structural instability inherent to dual-sourcing?',
    'Comparative frequency-and-resolution analysis across all functioning hybrid systems: if dispute rates fall as precedent accumulates, friction is incidental; if rates hold or rise despite accumulated precedent, the ambiguity is structural.',
    'If structural, the arrangement carries a rising ambiguity cost that compounds effective extraction over time and pressures eventual convergence on one source; if incidental, the current epsilon is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_dispute_structural_status, empirical, 'Whether the hybrid''s boundary disputes are noise or signal of structural strain.').

omega_variable(
    crown_grant_cost_or_rent,
    'Is the sovereign grant a coordination cost — the price of purchasing a neutral, non-partisan headship — or hereditary rent exceeding the market cost of equivalent ceremonial functions?',
    'Compare total public cost of the hereditary office against the audited cost of equivalent ceremonial headship in elected-headship systems of comparable scale, controlling for security and heritage-maintenance overhead.',
    'If the grant approximates replacement cost, the material transfer belongs to the coordination floor and epsilon drops; if it substantially exceeds it, the excess is rent and epsilon rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_grant_cost_or_rent, empirical, 'Whether the crown grant prices a service or collects a premium.').

omega_variable(
    foreclosure_victimhood_status,
    'Are absolutist and republican movements victims bearing an imposed cost, or ordinary losers in constitutional politics that any stable order necessarily produces?',
    'Distinguish foreclosure-by-neutral-process from foreclosure-by-self-interested-entrenchment: trace who designed the amendment thresholds and establishment conventions that block pure-form change, and whether those barriers protect the sitting beneficiaries specifically.',
    'If the barriers were built by and for the arrangement''s beneficiaries, the purist seats count as victims and the hybrid''s asymmetric-extraction half stands; if they are neutral constitutional architecture, the arrangement moves toward the pure-coordination end and the victim declarations overstate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_victimhood_status, conceptual, 'Whether pure-form foreclosure constitutes structural victimhood or routine constitutional loss.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sove_tr_t6, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(sove_tr_t12, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(sove_tr_t18, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(sove_tr_t24, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(sove_be_t6, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 6, 0.41).
narrative_ontology:measurement(sove_be_t12, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(sove_be_t18, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement(sove_be_t24, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(sove_su_t6, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(sove_su_t12, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(sove_su_t18, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 18, 0.33).
narrative_ontology:measurement(sove_su_t24, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, republican_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'legitimate authority' decomposes into three structurally distinct claims — monarchical_reading (single inherited downward source), republican_reading (single delegated upward source), and this file, constitutional_hybrid_reading (two independent sources with constitutional mediation). Each member has its own epsilon, beneficiary/victim structure, and failure modes; they are linked because the hybrid historically intermediates between the other two — it inherits the monarchical settlement's continuity claims and concedes the republican reading's delegation principle, so each sibling exerts structural pressure on it (restoration pressure from one side, abolition pressure from the other). Upstream confidence sits with the older monarchical settlements; the hybrid's stability depends on keeping both siblings' core premises out of the same framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
