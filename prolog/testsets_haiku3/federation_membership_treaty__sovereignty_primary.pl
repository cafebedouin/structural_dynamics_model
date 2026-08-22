% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federalist Treaty: Sovereignty-Primary Reading of Free Movement Restrictions
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This is the sovereignty-primary reading of free movement in a federal
 *   treaty system. Under this reading, member states retain primary authority
 *   to condition labor-market access on nationals' employment credentials,
 *   welfare contributions, and security/health grounds. The constraint
 *   coordinates federation membership by letting states preserve distinct
 *   labor and welfare regimes; it also extracts by giving states the power to
 *   restrict worker mobility and by gatekeeping welfare eligibility. The
 *   reading sits in explicit contest with an integration-primary reading
 *   (mobility is constitutive of the single market) and a
 *   subsidiarity-balance reading (mobility is presumptively free but can be
 *   constrained for proportionate reasons). This story instantiates only the
 *   sovereignty reading—the others are separate constraint stories linked via
 *   network effects.
 *
 * KEY AGENTS:
 *   - member_state_governments: institutional agenda-setters; retain discretionary authority
 *   - domestic_labor_protectionists: organized beneficiaries; preserved from competition
 *   - welfare_system_preservers: organized beneficiaries; gatekeep benefits on contribution grounds
 *   - mobile_eu_workers: moderate-power payers; constrained access to labor markets
 *   - third_country_migrants: powerless and structurally excluded; no treaty rights
 *   - supra_national_commission: institutional observer; monitors but cannot override state discretion
 *   - integration_advocates: institutional excluded voices; their frame is systematically disadvantaged
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.68).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.71).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federalist Treaty: Sovereignty-Primary Reading of Free Movement Restrictions").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, 'beca71fa-9bab-42ec-aa74-5506e7386b22').
narrative_ontology:cs_kernel_codification('beca71fa-9bab-42ec-aa74-5506e7386b22', formalized).
narrative_ontology:cs_authority_grounding('beca71fa-9bab-42ec-aa74-5506e7386b22', lineage).
narrative_ontology:cs_interpretation_layer_present('beca71fa-9bab-42ec-aa74-5506e7386b22').
narrative_ontology:cs_reading_relation('beca71fa-9bab-42ec-aa74-5506e7386b22', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('beca71fa-9bab-42ec-aa74-5506e7386b22', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('beca71fa-9bab-42ec-aa74-5506e7386b22', foundational, member_state_authority_primacy).
narrative_ontology:cs_axiom_status(member_state_authority_primacy, holdable).
narrative_ontology:cs_axiom_grounding('beca71fa-9bab-42ec-aa74-5506e7386b22', member_state_authority_primacy, conventional).
narrative_ontology:cs_axiom('beca71fa-9bab-42ec-aa74-5506e7386b22', foundational, labor_market_distinctness_preservable).
narrative_ontology:cs_axiom_status(labor_market_distinctness_preservable, holdable).
narrative_ontology:cs_axiom_grounding('beca71fa-9bab-42ec-aa74-5506e7386b22', labor_market_distinctness_preservable, empirically_contingent).
narrative_ontology:cs_reference_frame('beca71fa-9bab-42ec-aa74-5506e7386b22', westphalian_federation_with_preserved_state_capacity).
narrative_ontology:cs_drift_state('beca71fa-9bab-42ec-aa74-5506e7386b22', contemporary_eu_with_migration_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('beca71fa-9bab-42ec-aa74-5506e7386b22', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, domestic_labor_protectionists).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, welfare_system_preservers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_eu_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, third_country_migrants_seeking_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_citizens_dependent_on_mobility).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, member_state_citizens_dependent_on_mobility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain and exercise the authority to condition free movement on nationals' welfare credentials, employment history, and health/security grounds. They frame this as protection of social cohesion and fiscal sustainability. They administer both the permission structure (who enters the labor market) and the withdrawal threats (revocation of status, deportation) that give the restriction teeth. This reading treats member states as the primary rights-holders in the treaty framework.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Protected from direct wage-level competition by workers willing to accept lower rates, through restrictions on the pool of workers entitled to compete for each position. They experience the constraint as legitimate boundary maintenance (foreigners must prove they belong). Their material interest is preserved by controlling labor supply elasticity; the constraint's legitimacy rests on the state's authority to prioritize its own citizens.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, domestic_labor_protectionists, beneficiary,
    organized, biographical, mobile, national).

% Include public administrators, trade unions, and welfare-program advocates who argue that unrestricted access to welfare entitlements for non-citizens would erode the fiscal base and political support for the welfare state. They use the constraint to maintain gatekeeping: contributions first, benefits later, with the state authorized to make that determination. Their interest is in preserving program parameters.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, welfare_system_preservers, beneficiary,
    organized, generational, mobile, national).

% Face discretionary barriers to labor market entry in other member states: conditional access based on job offer, residency requirements, language/credential screening, and welfare-eligibility waiting periods. Under the sovereignty-primary reading, they are explicitly subordinate to member state discretion. Their exit from the constraint is exit from the federation itself, and their alternatives (non-EU labor markets) often require comparable or higher barriers.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_eu_workers, payer,
    moderate, biographical, constrained, global).

% Are structurally excluded from the conversation about free movement within the federation; they negotiate with member states individually and hold no treaty rights. Under the sovereignty reading they are entirely at state discretion. Their only 'voice' is the asylum and humanitarian frame, which operates outside the labor-market freedom conversation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, third_country_migrants_seeking_access, payer,
    powerless, biographical, trapped, global).

% Monitors member state compliance with treaty free-movement provisions but has limited enforcement power under the sovereignty-primary reading. Initiates infringement proceedings when states act in bad faith, but the proceedings themselves unfold within a framework that presumes state authority unless the restriction is plainly irrational. Their role is to police the outer bounds of state discretion, not to centralize labor mobility rights.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, supra_national_commission, observer,
    institutional, generational, analytical, global).

% Include transnational NGOs, pro-integration political parties, and economic analysts who argue the sovereignty reading is a cover story for protectionism and that free movement should be constitutive of the single market. They are partly in the conversation (litigation, policy advocacy) but their frame is structurally disadvantaged under this reading because they must argue FOR overriding state discretion, not in service of coordination but in service of a different federation design.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, integration_advocates, excluded,
    institutional, generational, analytical, global).

% Citizens of member states who want to work or study abroad benefit from reciprocal free-movement agreements among member states, but bear the costs of their own state's restrictions on inbound workers (wage pressure, domestic labor-market effects). They occupy a dual position: their state's citizen privileges come with restrictions on others; they rely on other states' reciprocal openness when they travel for work.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_citizens_dependent_on_mobility, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, member_state_citizens_dependent_on_mobility, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the federation's foundational legitimacy problem by preserving member state authority over labor-market composition and welfare eligibility. Member states agree to open labor markets to other member states' workers ONLY subject to state-determined conditions (job offers, residency, credentials, means tests). This coordination prevents a race to the bottom on labor standards and welfare entitlements by keeping that regulation at the state level.
% TRANSFER_FUNCTION: Moves labor (specifically, the right to supply labor) from mobile workers to member state gatekeepers. Mobile workers transfer the authority to make their employment and welfare eligibility conditional on state discretion; in return, they receive conditional access to a larger labor market than their home state alone offers. States retain the residual power to revoke that access.
% ABSENT_VOICES: Third-country migrants have no seat at the table and no rights derived from the treaty; they are structured out of the conversation entirely. Pro-integration advocates are partly heard (in litigation and policy forums) but their frame — that free movement should be constitutive rather than conditional — is systematically disadvantaged by the sovereignty-primary reading itself. Supranational institutions have only monitoring capacity, not agenda-setting power.
% DISAPPEARANCE_RATIONALE: If member state authority over free-movement conditions disappeared overnight, labor would flow to opportunity without state gatekeeping; wage disparities would compress; welfare-eligibility criteria would face pressure to converge upward or face fiscal migration; the political coalition holding member states in federation would fracture because state-level legitimacy would hinge on openness rather than boundary control. The constraint is not incidental to the federation's design; it is a load-bearing pillar of state consent to federation membership.
% FOUNDING_PROBLEM: Post-war European federalism faced an irresolvable tension: create a common market to prevent war and enable prosperity, but allow member states to preserve distinct welfare regimes and labor-market protections without which domestic political coalitions would withdraw support from federation membership.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments continue to invoke labor protection and welfare sustainability as justifications for mobility restrictions. Independent policy analysis from outside the member states (OECD, World Bank) documents that member states use all available discretion to screen inbound workers and that welfare-eligibility conditions remain primary political instruments. Parliamentary testimony from welfare administrators and labor economists corroborates that unrestricted access to welfare would force either fiscal increases or benefit cuts, neither politically sustainable in high-taxation countries.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint gives member states power to restrict worker access without proportionality review; the restriction persists because it serves concentrated interests (labor protectionists, welfare administrators) at the cost of diffuse harm (constrained mobility for millions of workers). Suppression is comparable (0.71) because the constraint's enforcement rests on active state gatekeeping: work permits, credential screening, welfare-eligibility bureaucracy, deportation threats. Theater ratio (0.42) is moderate-high because the constraint wraps itself in narratives of social protection and fiscal sustainability, but increasingly the performative justifications (security screening, job-match verification) exceed the actual gatekeeping function. The measurement series show extraction rising from 0.52 to 0.68 over the interval (political ratchet: states have incrementally tightened restrictions in response to migration pressure and welfare concerns), theater rising from 0.28 to 0.42 (bureaucratic expansion of screening criteria), and suppression plateauing near 0.71 (enforcement infrastructure stabilized). All metrics share one time grid (t0,4,8,12,16,20,24,28,32); no metric is authored at a time point the others omit.
 *
 * PERSPECTIVAL GAP:
 *   The member-state-government seat and the mobile-worker seat should compute different types from the engine. From the state's position, the arrangement is genuine coordination: it solved the federation's legitimacy crisis by preserving state authority, and states experience the constraint as enabling rather than extractive (without the authority to restrict, they would exit federation entirely). From the worker's position, the same structure operates as enforced extraction: constrained access, discriminatory criteria, conditional welfare rights. The suppression experienced by a third-country migrant (structurally excluded, no appeal mechanism, trapped between states) is qualitatively different from the suppression experienced by a mobile EU worker (nominally free but in practice subject to state discretion). These divergences are computed from the stakeholder data (power atoms, exit options, role declarations) — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments are the structural beneficiaries: they collect the power to make mobility conditional, and they administer the rules. Domestic labor protectionists and welfare preservers benefit from restricted labor supply and gatekept welfare access (d near 0.0 for beneficiaries, or negative when subsidy flow is computed). Mobile EU workers are the primary targets (pay the transfer in constrained access, moderately powerful but trapped in federation-level negotiation, d near 0.7). Third-country migrants are the purest extraction targets (powerless, trapped, identity-locked by migration status, d near 1.0). Supra-national commission sits analytically neutral (observer seat, no extraction/benefit). Integration advocates are partially excluded (their frame is systematically blocked by the reading's structure, but they remain partly heard in litigation).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint satisfies the tangled-rope criterion: (1) genuine coordination function—solved the federation's legitimacy crisis by preserving state authority over labor and welfare (required for member states to consent to federation); (2) asymmetric extraction—the same machinery that coordinates (state discretion over membership conditions) also extracts (workers cannot appeal state gatekeeping); (3) active enforcement—member states continuously administer work permits, credential screening, welfare-eligibility verification, and deportation. The classification resists being degraded to 'rope' (pure coordination) because the extraction is not incidental—the entire rationale for state consent turns on the authority to restrict. It resists being degraded to 'snare' (pure extraction) because the coordination is not a cover story—states genuinely use the authority to preserve distinct welfare regimes and labor-market protections, not merely to collect rents. The founding problem (post-war federation needs to let states preserve distinct regimes) is live and corroborated by state behavior; it prevents the constraint from being reclassified as a dead-mandate piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression (0.71) primarily structural (external barriers: permits, credentials, waiting periods) or internalized (workers internalize the ''nationals first'' frame and believe they don''t belong)?',
    'Post-migration trajectory analysis: track workers who successfully migrated after state gatekeeping was lifted (either they achieved legal status or moved to a more-open jurisdiction). If suppression dissipates after structural barriers are removed, the suppression was primarily structural. If workers carry constraints with them (self-imposed limits on career advancement, welfare-seeking avoidance, identity-fusion with subordinate status), suppression has an internalized component.',
    'If internalized, the effective suppression is higher than the structural measure suggests—the constraint persists in workers'' choices even after barriers are formally removed. This would increase the classification''s extractiveness on the victim side. If purely structural, barrier removal would restore mobility; if partially internalized, barrier removal would restore some mobility but leave residual constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether suppression is structural (external) or internalized (workers'' own cognition).').

omega_variable(
    reading_incompatibility_integration_vs_sovereignty,
    'Can the sovereignty-primary reading and the integration-primary reading coexist in a single federation''s legal framework, or does adopting one logically foreclose the other?',
    'Historical analysis of case law: does the federation''s highest court ever hold that both free movement is constitutive AND states retain authority to restrict? Or do successive judgments show the readings replacing each other (court pivots from one to the other) rather than coexisting? If court doctrine stabilizes on an explicit both-and (subsidiarity reading), the two readings coexist; if doctrine oscillates or explicitly rejects one reading, they foreclose.',
    'If readings foreclose each other, the classification of this constraint depends on which reading is institutionally dominant at a given time—a shift in interpretation changes the constraint type. If readings coexist across different institutional venues (national courts holding sovereignty-primary, supranational court holding integration-primary), the constraint becomes a contested institutional site rather than a stable type. This affects whether mandatrophy is resolved or persistent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incompatibility_integration_vs_sovereignty, conceptual, 'Whether this reading logically forecloses integration-primary or whether both can coexist.').

omega_variable(
    founding_problem_residual_validity,
    'The founding problem is post-war federation: preserve state capacity to protect labor markets and welfare to maintain domestic political support for federation. Does this problem remain live, or have subsequent institutional developments (centralized welfare floors, labor standards harmonization) partly resolved it such that the constraint persists by inertia rather than function?',
    'Counterfactual scenario: if a supranational authority imposed uniform welfare floors and labor standards (removing states'' incentive to restrict access to protect distinct regimes), would member states still demand the gatekeeping authority? Or would they accept unrestricted mobility? Member-state rhetoric and reservation-statements on any attempted harmonization would reveal whether the founding problem is still generative of state behavior or has become vestigial.',
    'If the problem is residual and the constraint persists by inertia, the classification shifts toward piton (dead mandate). If the problem is live—states still explicitly justify restrictions on founding-problem grounds—the constraint remains tangled rope. This affects whether the constraint is targeted for reform (if piton) or accepted as a necessary federalism accommodation (if tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_residual_validity, empirical, 'Whether the founding problem remains live or has been partly resolved by subsequent institutional development.').

omega_variable(
    sovereignty_vs_integration_reading_coexistence,
    'This reading (sovereignty-primary) asserts that readings coexist across different parties; the sibling integration reading asserts that free movement is constitutive (readings implicitly foreclose under that frame). Which reading of the readings is correct?',
    'Mapping institutional behavior: document which courts, legislatures, and negotiating bodies adopt which reading and whether any single institutional venue holds both simultaneously. If a court explicitly states ''both readings are live and the resolution is subsidiarity-balance,'' that is evidence that readings coexist (not foreclose). If a court says ''free movement is constitutive, restrictions must be strictly justified,'' that reading forecloses the sovereignty reading from that court''s logic.',
    'If sovereignty and integration readings coexist, the federation is in stable contest and the constraint operates as tangled rope under both readings. If one reading forecloses the other institutionally, the constraint''s type oscillates with which reading is ascendant—a mandatrophy-adjacent instability. The subsidiarity-balance reading (if it is a genuine third reading and not just a rhetorical middle ground) could stabilize the contest by providing an institutional resolution frame that both readings can inhabit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_reading_coexistence, conceptual, 'Whether the sovereignty and integration readings logically coexist or foreclose each other; equivalently, what the correct cs_structure.reading_relations assignment is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fede_tr_t4, federation_membership_treaty__sovereignty_primary, theater_ratio, 4, 0.31).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__sovereignty_primary, theater_ratio, 8, 0.34).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__sovereignty_primary, theater_ratio, 12, 0.37).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__sovereignty_primary, theater_ratio, 16, 0.4).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.41).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__sovereignty_primary, theater_ratio, 24, 0.42).
narrative_ontology:measurement(fede_tr_t28, federation_membership_treaty__sovereignty_primary, theater_ratio, 28, 0.42).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__sovereignty_primary, theater_ratio, 32, 0.42).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(fede_be_t4, federation_membership_treaty__sovereignty_primary, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__sovereignty_primary, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__sovereignty_primary, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__sovereignty_primary, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__sovereignty_primary, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(fede_be_t28, federation_membership_treaty__sovereignty_primary, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__sovereignty_primary, base_extractiveness, 32, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(fede_su_t4, federation_membership_treaty__sovereignty_primary, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__sovereignty_primary, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__sovereignty_primary, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__sovereignty_primary, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__sovereignty_primary, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(fede_su_t28, federation_membership_treaty__sovereignty_primary, suppression_requirement, 28, 0.71).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__sovereignty_primary, suppression_requirement, 32, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__sovereignty_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, welfare_state_migration_pressure).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, labor_market_segmentation_by_citizenship).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading family under the federation_membership_treaty kernel. The sovereignty-primary reading (this story) holds that member states retain primary authority to condition labor mobility; the integration-primary reading holds that free movement is constitutive and restrictions require narrow justification; the subsidiarity-balance reading holds that legitimate national interests constrain but do not eliminate mobility. All three readings share the same kernel (the written treaty clause) and the same beneficiary/victim structure (states benefit, mobile workers pay), but differ in what that structure means. The three stories are linked via network.affects_constraints; a shift in which reading is institutionally dominant changes the effective type of the constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, powerless, 0.95).
constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
