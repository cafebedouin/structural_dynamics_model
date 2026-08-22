% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Welfare Closure Authority with Conditional Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint is the member_sovereignty_primary reading of the
 *   federation_membership_obligations kernel. It instantiates the claim that
 *   national welfare states retain closure authority over welfare access,
 *   with free movement conditional on labor market protection and system
 *   sustainability. Under this reading, mobile workers are excluded from full
 *   welfare beneficiary status, receiving-state labor forces are protected
 *   from wage competition, and member-state legislatures retain veto
 *   authority over welfare access conditions. Sibling readings include
 *   integration_primary (free movement as constitutive of citizenship) and
 *   selective_solidarity (tiered access by contribution history).
 *
 * KEY AGENTS:
 *   - Member state legislatures: Primary agenda-setter (institutional/constrained) â sets closure conditions and enforces contributory thresholds.
 *   - Receiving state workforce: Primary beneficiary (organized/constrained) â gains wage and welfare-system protection.
 *   - Mobile workers: Primary payer (powerless/constrained) â bears partial exclusion costs while exercising formal movement rights.
 *   - EU Commission: Analytical observer (institutional/analytical) â monitors compliance and challenges closure through infringement actions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.62).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.58).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Welfare Closure Authority with Conditional Free Movement").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '28927780-fec2-4899-a836-cb85b2bd92af').
narrative_ontology:cs_kernel_codification('28927780-fec2-4899-a836-cb85b2bd92af', formalized).
narrative_ontology:cs_authority_grounding('28927780-fec2-4899-a836-cb85b2bd92af', lineage).
narrative_ontology:cs_interpretation_layer_present('28927780-fec2-4899-a836-cb85b2bd92af').
narrative_ontology:cs_reading_relation('28927780-fec2-4899-a836-cb85b2bd92af', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('28927780-fec2-4899-a836-cb85b2bd92af', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('28927780-fec2-4899-a836-cb85b2bd92af', foundational, national_welfare_closure_authority).
narrative_ontology:cs_axiom_status(national_welfare_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('28927780-fec2-4899-a836-cb85b2bd92af', national_welfare_closure_authority, conventional).
narrative_ontology:cs_axiom('28927780-fec2-4899-a836-cb85b2bd92af', foundational, labor_market_protection_over_mobility).
narrative_ontology:cs_axiom_status(labor_market_protection_over_mobility, holdable).
narrative_ontology:cs_axiom_grounding('28927780-fec2-4899-a836-cb85b2bd92af', labor_market_protection_over_mobility, instrumental).
narrative_ontology:cs_reference_frame('28927780-fec2-4899-a836-cb85b2bd92af', national_welfare_sovereignty).
narrative_ontology:cs_drift_state('28927780-fec2-4899-a836-cb85b2bd92af', post_eastern_enlargement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28927780-fec2-4899-a836-cb85b2bd92af', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, receiving_state_workforce).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set residency, contributory, and labor-market conditions for welfare access; retain veto-like authority over EU-mandated openness; justify closure by referencing welfare system sustainability and the need to protect domestic labor markets from asymmetric competition.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced wage competition and from welfare systems whose contribution bases are shielded from immediate claims by new arrivals; protected by closure conditions that reserve full benefits for established contributors.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_workforce, beneficiary,
    organized, biographical, constrained, national).

% Exercise formal free movement rights but encounter contributory thresholds, waiting periods, and exclusions from non-contributory benefits; bear the cost of partial integration while participating in receiving-state labor markets.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, regional).

% Monitors compliance with free movement and non-discrimination directives; brings infringement proceedings against closure measures; analytically tracks the gap between formal mobility rights and conditioned welfare access.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, eu_commission, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the sustainability of national welfare systems by preventing rapid contribution-base erosion and protecting domestic labor markets from asymmetric wage competition in a federal context with uneven economic development.
% TRANSFER_FUNCTION: Moves closure authority and conditional welfare access from mobile workers to member state legislatures and receiving-state workforces, exchanging full mobility rights for labor-market stability and welfare-system protection.
% ABSENT_VOICES: Mobile workers without stable employment contracts and prospective entrants from lower-wage regions are underrepresented in national legislative processes that set closure conditions; the EU Commission's integration-primary framing is structurally sidelined in welfare-policy debates dominated by member-state sovereignty claims.
% DISAPPEARANCE_RATIONALE: If closure authority vanished overnight, receiving-state workforces would face immediate wage competition and welfare systems would face contribution strain; member-state legislatures would lose a primary policy lever for managing migration impacts; mobile workers would gain full access but the existing domestic equilibrium would rearrange around open welfare access.
% FOUNDING_PROBLEM: Uneven economic development across a federal union creates incentives for welfare migration and asymmetric labor competition, threatening the political sustainability of national welfare states and depressing wages in receiving regions.
% FOUNDING_PROBLEM_CORROBORATION: National finance ministries and established labor economists attest to welfare-sustainability risks from asymmetric mobility; EU Commission economic analyses and mobile-worker advocacy groups contest the severity, citing net fiscal contributions of migrants and arguing the problem is overstated to justify insider protection.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because mobile workers face systematic exclusion from non-contributory benefits and contributory waiting periods despite formal free movement. Suppression (0.58) is moderate-to-high because alternatives (unconditional welfare access) are legally barred by national legislation and judicially contested. Theater ratio (0.34) reflects periodic political performance around 'welfare tourism' that exceeds the actual fiscal incidence. Accessibility collapse (0.65) is significant: once the closure framework is accepted, unconditional mobility alternatives are difficult to reconstruct politically. Resistance (0.50) is moderate, coming from mobile workers, EU institutions, and pro-mobility advocacy. The temporal series show extraction rising through enlargement eras and peaking around peak politicization (time_point 24), then slightly moderating as legal challenges and compensatory mechanisms partially offset pure closure.
 *
 * PERSPECTIVAL GAP:
 *   The member-state legislature seat experiences the constraint as necessary coordination to preserve welfare-state legitimacy and labor-market stability; the mobile-worker seat experiences the same structure as enforced exclusion that converts formal rights into conditional privileges. The engine computes this divergence from the structural data rather than adjudicating it through the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state workforces are structural beneficiaries (low directionality), shielded from competition. Mobile workers are structural targets (high directionality), paying through exclusion and delayed access. Member-state legislatures sit near the agenda-setter pole with moderate directionality: they do not capture monetary rents but capture political authority and voter support from maintaining closure. The EU Commission sits as an analytical observer with negligible extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by preserving the genuine coordination function: without closure authority, asymmetric federalism would create welfare migration pressures that could destabilize national social insurance systems. However, the exclusion of mobile workers is not an unavoidable side effect; it is a structural feature that extracts from one group to protect another. Because both coordination and extraction are present and operate through the same mechanism, the constraint is tangled_rope rather than rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_sustainability_or_insider_rent,
    'Does closure authority primarily protect welfare system solvency, or does it transfer rents to established workers and domestic constituencies at the expense of mobile workers?',
    'Cross-national fiscal incidence studies measuring net transfer effects of mobile worker exclusion on domestic constituencies and social insurance funds.',
    'If net protection to domestic constituencies exceeds measurable solvency benefit, classification shifts toward snare; if solvency is genuine, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_sustainability_or_insider_rent, empirical, 'Whether closure tracks welfare sustainability or insider rent extraction.').

omega_variable(
    sovereignty_naturalness,
    'Is member-state welfare closure an inherent feature of federal architecture, or a constructed privilege maintained by political choice?',
    'Comparative federalism analysis: do stable federal unions with comparable welfare states universally exhibit closure, or do open-access federal systems sustain welfare through alternative mechanisms?',
    'If closure is not structurally necessary, the constraint''s coordination function is weaker than claimed and extraction dominates the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_naturalness, conceptual, 'Whether closure is structurally necessary or constructed privilege.').

omega_variable(
    legislative_veto_democratic_legitimacy,
    'Does the legislative veto over welfare access reflect democratic self-determination or sectional extraction by receiving states?',
    'Analysis of voter preferences versus mobile worker disenfranchisement; assessment of whether closure measures track broad majority preference or concentrated interest-group pressure.',
    'If closure tracks concentrated interests, directionality for member-state legislatures shifts toward extraction-agenda-setter; if broad democratic mandate, coordination legitimacy is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_veto_democratic_legitimacy, preference, 'Whether closure is democratically grounded or sectional extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_tr_t8, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 8, 0.2).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_tr_t16, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 16, 0.3).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_tr_t24, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 24, 0.38).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_tr_t32, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 32, 0.36).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_tr_t40, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 40, 0.34).

% Extraction over time
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_be_t8, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_be_t16, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_be_t24, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_be_t32, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_be_t40, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_su_t8, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_su_t16, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_su_t24, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_su_t32, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(federation_membership_obligations__member_sovereignty_primary_su_t40, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% The federation_membership_obligations kernel decomposes into three structurally distinct constraints: integration_primary (mobility as constitutive), member_sovereignty_primary (closure authority retained), and selective_solidarity (tiered contributory access). Each reading has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
