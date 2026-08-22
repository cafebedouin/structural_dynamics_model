% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: Free Movement as Constitutive of EU Citizenship (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story instantiates the integration-primary reading of the
 *   federation_membership_obligations kernel: free movement is treated as
 *   constitutive of EU citizenship itself, not merely a market-access
 *   provision conditional on member-state welfare sustainability. Under this
 *   reading, ECJ case law progressively expanded equal-treatment doctrine
 *   (Grzelczyk onward) so that mobile workers, once resident and economically
 *   active, enter the full welfare beneficiary set of the receiving state on
 *   terms equal to nationals. The extraction this story authors is real and
 *   rising: receiving-state welfare systems and local labor bear adjustment
 *   costs that were never legislated domestically but were instead produced
 *   through treaty interpretation and doctrinal accretion. This is NOT the
 *   same constraint as member_sovereignty_primary (which authors near-zero
 *   extraction from the receiving-state perspective because it treats closure
 *   authority as retained) or selective_solidarity (which authors extraction
 *   only for the non-contributory subset of mobile workers). Each reading has
 *   its own stable epsilon; they are linked as siblings, not blended.
 *
 * KEY AGENTS:
 *   - mobile_eu_workers: primary beneficiary (moderate/mobile) — gains equal-treatment welfare access
 *   - single_market_employers: secondary beneficiary (powerful/arbitrage) — draws on enlarged labor pool without bearing fiscal costs
 *   - european_court_of_justice: agenda_setter (institutional/analytical) — expands doctrine through case law, the primary engine of this reading's structural delta
 *   - receiving_state_local_labor: primary target (powerless/trapped) — bears wage and job-competition costs with no comparable exit
 *   - receiving_state_welfare_systems: institutional target (institutional/constrained) — must extend eligibility beyond domestically legislated boundaries
 *   - sending_state_labor_markets: excluded — bears population and human-capital loss, absent from this reading's accounting entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.58).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.62).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "Free Movement as Constitutive of EU Citizenship (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '6c91d132-c093-4b7b-b951-e9d1f6d9d83e').
narrative_ontology:cs_kernel_codification('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', fixed_text).
narrative_ontology:cs_authority_grounding('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', lineage).
narrative_ontology:cs_interpretation_layer_present('6c91d132-c093-4b7b-b951-e9d1f6d9d83e').
narrative_ontology:cs_reading_relation('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', federation_membership_obligations__member_sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', foundational, free_movement_constitutive_of_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', free_movement_constitutive_of_citizenship, conventional).
narrative_ontology:cs_axiom('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', secondary, welfare_boundary_subordinate_to_market_unity).
narrative_ontology:cs_axiom_status(welfare_boundary_subordinate_to_market_unity, holdable).
narrative_ontology:cs_axiom_grounding('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', welfare_boundary_subordinate_to_market_unity, instrumental).
narrative_ontology:cs_reference_frame('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', treaty_of_rome_worker_mobility_baseline).
narrative_ontology:cs_drift_state('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', post_maastricht_citizenship_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6c91d132-c093-4b7b-b951-e9d1f6d9d83e', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, single_market_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, eu_commission).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, european_court_of_justice).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_state_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, net_contributor_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, eu_citizenship_as_constitutive_status).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, single_market_indivisibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move across member states to work, drawing on the integration-primary reading's guarantee of equal treatment and welfare access once resident and economically active. Their exit options within the EU are genuinely enhanced by this reading; the constraint is what makes their mobility more than nominal.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on a continent-wide labor pool without the frictions of work-permit regimes, arbitraging wage and skill differentials across member states. They benefit from free movement without bearing the fiscal costs that land on receiving-state welfare systems.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, single_market_employers, beneficiary,
    powerful, generational, arbitrage, continental).

% Progressively expands the scope of free movement and equal-treatment doctrine through case law (Grzelczyk, Bidar, Dano-line jurisprudence), treating mobility rights as increasingly inseparable from citizenship status itself. Each ruling narrows the discretion member states retain to condition welfare access on prior contribution or genuine work.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, european_court_of_justice, agenda_setter,
    institutional, civilizational, analytical, continental).

% Enforces free movement directives against member states, initiates infringement proceedings where national welfare rules are read as discriminatory, and derives institutional legitimacy from deepening integration. Cannot be sued out of the system by any single member state.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_commission, agenda_setter,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, eu_commission, beneficiary).

% Compete against an enlarged labor pool for jobs and wages in low-skill and mid-skill sectors, with no comparable exit into other member states' labor markets (language, credential recognition, and social-network barriers bind far more tightly than the formal legal right to move). Bear downward wage pressure that the mobility right does not compensate them for.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_state_local_labor, payer,
    powerless, biographical, trapped, national).

% Administer welfare programs designed and funded around a national contributory base, and must extend benefits to mobile EU citizens under ECJ doctrine even where domestic political consensus assumed a closed beneficiary pool. Cannot unilaterally re-draw the boundary without treaty-level renegotiation or risking infringement action.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Fund welfare systems through taxation and absorb the fiscal cost of extended eligibility without having voted on the boundary redefinition, which occurred through treaty interpretation and case law rather than domestic legislative choice.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, net_contributor_taxpayers, payer,
    powerless, biographical, trapped, national).

% Retain formal sovereignty over welfare policy on paper but find the practical boundary of that sovereignty set by ECJ jurisprudence rather than domestic parliamentary majorities. Their attempts to reintroduce residency or contribution tests are frequently struck down or narrowed.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, member_state_governments, excluded).

% Lose working-age population and accumulated human capital to outward mobility, with no seat in the integration-primary reading's account of who benefits and who pays — the reading is framed entirely around the receiving-state welfare boundary and does not price the sending-state cost of departure.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, sending_state_labor_markets, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees that a single market in labor is real rather than nominal: a worker's right to move must include the right to be treated on equal terms once resident, or 'free movement' collapses into a right to relocate without protection — a right no one facing real dependency risk would exercise.
% TRANSFER_FUNCTION: Moves welfare-system access from a nationally bounded contributory pool to a continentally bounded citizenship pool; moves labor supply from sending states to receiving states; moves the fiscal and wage-adjustment costs of that reallocation onto receiving-state taxpayers and local labor rather than onto EU-level fiscal instruments.
% ABSENT_VOICES: Sending-state labor markets and communities losing working-age population have no seat in this reading's accounting — the doctrine evaluates costs and benefits entirely at the receiving-state boundary. Displaced local labor in receiving states is nominally represented by national governments, but those governments' capacity to act on the objection is itself constrained by the doctrine under contest.
% DISAPPEARANCE_RATIONALE: If the integration-primary reading were displaced by member_sovereignty_primary overnight, receiving states would reintroduce residency and contribution conditions on welfare access, ECJ case law expanding equal-treatment doctrine would be legislatively overridden or treaty-amended, and mobile workers' de facto welfare entitlements would contract sharply even though formal free-movement rights persisted on paper.
% FOUNDING_PROBLEM: The single market's labor-mobility pillar was hollow if workers who moved could be treated as permanent second-class residents ineligible for the welfare protections available to nationals — a two-tier status that would have made 'free movement' a right exercised only by the already-secure.
% FOUNDING_PROBLEM_CORROBORATION: The Commission and ECJ attest the problem remains live: absent equal-treatment enforcement, member states would revert to de facto exclusion of mobile citizens from welfare, undermining Treaty-guaranteed mobility. Independent public finance economists and several receiving-state finance ministries attest the problem, as originally framed, has been substantially solved — mobile EU workers are overwhelmingly net fiscal contributors on average — and that continued doctrinal expansion now addresses a different, unlegislated problem: extending citizenship-based solidarity beyond what any national electorate approved. This corroboration comes from outside the beneficiary set (independent fiscal studies, member-state government submissions to the Council), not from mobile workers or EU institutions themselves.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.32 to 0.58 over the interval, tracking the doctrinal expansion arc from early free-movement jurisprudence (narrowly tied to workers) through citizenship-based equal-treatment rulings (extending to job-seekers and, contested at the margin, to non-economically-active residents). Suppression sits moderately high (0.62) because member states' formal capacity to re-draw the welfare boundary is real on paper but constrained by infringement risk and the doctrine's own momentum — this is coercion by legal architecture, not by police power, but it is still suppression of the member-state alternative. Theater ratio is low-to-moderate (0.28): the coordination function (a genuinely single labor market) is real, not a cover story, but an increasing share of enforcement activity (infringement proceedings, doctrinal extension to marginal cases) defends the boundary expansion itself rather than core market functioning.
 *
 * PERSPECTIVAL GAP:
 *   The ECJ and Commission seats experience this as coordination successfully deepening — a single market functioning as intended, an entailment of citizenship rather than an imposition. The receiving-state local labor and welfare-system seats experience the identical structure as extraction imposed through a channel (case law) that bypassed the domestic legislative process that would ordinarily authorize welfare-boundary changes. The engine should compute divergent seat types from these structural facts; this divergence is the analytical payload of the story, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and single-market employers sit near the beneficiary end: the constraint subsidizes their mobility and labor-sourcing respectively. Receiving-state local labor and welfare systems sit near the target end: trapped or constrained exit, bearing costs generated by a boundary redefinition they did not vote for through ordinary domestic politics. Sending-state labor markets are excluded rather than positioned on the beneficiary/victim axis at all — the integration-primary reading's accounting apparatus does not price their loss, which is itself a structural feature of this reading (a sibling reading might treat sending-state depletion as a first-order cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — hollow mobility rights unless coupled to equal treatment — is contested as live: EU institutions treat it as still-operative (any relaxation invites de facto second-class status for movers), while independent fiscal analysis suggests the original problem is largely solved and the doctrine now pursues an unlegislated, more expansive solidarity mandate. This mismatch (contested status + world_rearranges disappearance verdict) is exactly the kind of case the R5 apparatus is built to flag for further scrutiny rather than adjudicate by narrative alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the integration-primary reading — treating free movement as constitutive of citizenship rather than conditional on national welfare sustainability — the correct reading of the EU treaties, or is it a doctrinal expansion beyond what member states consented to at accession?',
    'Comparative analysis of Treaty text, accession-era legislative history, and the trajectory of ECJ case law relative to earlier drafting intent; alternatively, resolution via treaty amendment or a landmark ruling that narrows equal-treatment doctrine back toward the economically-active-worker standard.',
    'If the integration-primary reading is the doctrinally correct one, the extraction measured here reflects the genuine (if costly) logic of a single market in citizenship terms. If the member_sovereignty_primary or selective_solidarity reading is correct, this reading''s expansion constitutes judicial overreach and the measured extraction is better attributed to institutional drift rather than founding design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this is the treaty-faithful reading of free movement or a doctrinal expansion beyond original consent.').

omega_variable(
    sibling_reading_structural_delta,
    'What specific case-law and policy elements would change under the member_sovereignty_primary or selective_solidarity readings, and where exactly does the disagreement locate?',
    'Trace the doctrinal fork points: Grzelczyk/Bidar-line rulings extending equal treatment to non-worker EU citizens are the specific locus where integration_primary and selective_solidarity diverge (contributory test vs. citizenship test); Dano and subsequent restrictive rulings are where integration_primary and member_sovereignty_primary diverge (whether member states can condition benefits on genuine work-seeking).',
    'Locating the disagreement at specific doctrinal fork points (rather than treating the kernel dispute as diffuse) would let policy analysts identify which specific rulings, if reversed, would shift the constraint from this reading toward a sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Precisely which case-law fork points separate this reading from member_sovereignty_primary and selective_solidarity.').

omega_variable(
    sending_state_cost_exclusion,
    'Is the exclusion of sending-state labor-market depletion from this reading''s cost accounting a structural feature of the integration-primary doctrine itself, or an artifact of which actors litigate before the ECJ (receiving-state governments and mobile individuals, never sending-state communities)?',
    'Examine whether any ECJ free-movement jurisprudence has ever weighed sending-state brain-drain or demographic costs as a relevant factor; absence across the full case-law record would support the structural-exclusion reading.',
    'If sending-state costs are structurally excluded from the doctrine''s own logic (not merely absent from litigation), that strengthens the case that this reading''s ε is systematically under-measured relative to a fuller accounting that included sending-state effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sending_state_cost_exclusion, empirical, 'Whether sending-state exclusion is doctrinal or merely a litigation-standing artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__integration_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t8, federation_membership_obligations__integration_primary, theater_ratio, 8, 0.15).
narrative_ontology:measurement(fede_tr_t16, federation_membership_obligations__integration_primary, theater_ratio, 16, 0.18).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__integration_primary, theater_ratio, 24, 0.22).
narrative_ontology:measurement(fede_tr_t32, federation_membership_obligations__integration_primary, theater_ratio, 32, 0.25).
narrative_ontology:measurement(fede_tr_t40, federation_membership_obligations__integration_primary, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__integration_primary, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fede_be_t8, federation_membership_obligations__integration_primary, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(fede_be_t16, federation_membership_obligations__integration_primary, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__integration_primary, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(fede_be_t32, federation_membership_obligations__integration_primary, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(fede_be_t40, federation_membership_obligations__integration_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__integration_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fede_su_t8, federation_membership_obligations__integration_primary, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(fede_su_t16, federation_membership_obligations__integration_primary, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__integration_primary, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(fede_su_t32, federation_membership_obligations__integration_primary, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(fede_su_t40, federation_membership_obligations__integration_primary, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the federation_membership_obligations kernel. integration_primary (this story) authors ECJ-driven doctrinal expansion as constitutive of EU citizenship, with rising extraction on receiving-state welfare systems and local labor. member_sovereignty_primary authors the same underlying treaty text as preserving national closure authority, with correspondingly minimal extraction from receiving states (the transfer this story measures does not occur under that reading's premises). selective_solidarity authors a middle reading where extraction is narrower, confined to the non-contributory subset of mobile beneficiaries. All three share beneficiary categories (mobile workers, employers) but diverge sharply on victim sets and epsilon magnitude — they are not measurements of one constraint but three distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
