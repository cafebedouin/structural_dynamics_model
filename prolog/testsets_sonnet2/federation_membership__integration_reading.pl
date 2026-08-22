% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Free Movement / Supranational Authority Reading)
 *   domain: political/economic
 *
 * SUMMARY:
 *   This story instantiates the integration reading of the
 *   federation-membership kernel: free movement is a constitutional right,
 *   membership is treated as irreversible except through full withdrawal, and
 *   supranational adjudicative authority is legitimate over member-state
 *   border policy. Under this reading, mobile skilled citizens and
 *   continent-spanning employers benefit from frictionless labor arbitrage,
 *   while geographically fixed local labor markets, border-region low-wage
 *   workers, and national welfare systems bear concentrated, non-negotiable
 *   costs. The sibling sovereignty reading (member-state border authority
 *   retained, free movement as negotiable treaty policy) is a structurally
 *   distinct constraint with a different beneficiary/victim map and a
 *   different ε — it is authored as a separate file and linked via
 *   network.affects_constraints, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - mobile_skilled_citizens: Primary beneficiary (organized/arbitrage) — exploits frictionless movement
 *   - federation_wide_employers: Primary beneficiary (powerful/arbitrage) — arbitrages continental wage differentials
 *   - supranational_institutions: Agenda-setter (institutional/analytical) — adjudicates and enforces the constitutional order
 *   - local_labor_markets: Primary target (powerless/trapped) — absorbs wage compression and displacement
 *   - border_region_low_wage_workers: Primary target (powerless/trapped) — bears concentrated competitive costs
 *   - national_welfare_systems: Fiscal payer (institutional/constrained) — funds services without inflow control
 *   - member_state_governments: Dual-positioned (institutional/constrained) — administers domestic policy but cannot restrict inbound movement
 *   - sovereignty_reading_advocates: Excluded voice (organized/constrained) — locked out of the adjudicative forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership__integration_reading, 0.61).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Free Movement / Supranational Authority Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political/economic").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '05087159-f051-4999-aecc-ed102801dd69').
narrative_ontology:cs_kernel_codification('05087159-f051-4999-aecc-ed102801dd69', formalized).
narrative_ontology:cs_authority_grounding('05087159-f051-4999-aecc-ed102801dd69', extraction).
narrative_ontology:cs_interpretation_layer_present('05087159-f051-4999-aecc-ed102801dd69').
narrative_ontology:cs_reading_relation('05087159-f051-4999-aecc-ed102801dd69', federation_membership__sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('05087159-f051-4999-aecc-ed102801dd69', foundational, membership_irreversible_absent_full_withdrawal).
narrative_ontology:cs_axiom_status(membership_irreversible_absent_full_withdrawal, holdable).
narrative_ontology:cs_axiom_grounding('05087159-f051-4999-aecc-ed102801dd69', membership_irreversible_absent_full_withdrawal, conventional).
narrative_ontology:cs_axiom('05087159-f051-4999-aecc-ed102801dd69', foundational, free_movement_is_constitutional_right_not_policy).
narrative_ontology:cs_axiom_status(free_movement_is_constitutional_right_not_policy, holdable).
narrative_ontology:cs_axiom_grounding('05087159-f051-4999-aecc-ed102801dd69', free_movement_is_constitutional_right_not_policy, deontological).
narrative_ontology:cs_reference_frame('05087159-f051-4999-aecc-ed102801dd69', founding_treaty_irreversible_integration).
narrative_ontology:cs_drift_state('05087159-f051-4999-aecc-ed102801dd69', contemporary_populist_contestation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('05087159-f051-4999-aecc-ed102801dd69', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_skilled_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, federation_wide_employers).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_institutions).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, border_region_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_welfare_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move freely across member-state borders to take jobs, study, and settle wherever wages or opportunities are best, treating the whole federation as a single labor and life market. Constitutional free-movement guarantees mean no member state can condition their residence on local labor-market tests. They benefit directly from the irreversibility of integration: no state can revoke the right without a treaty-level crisis.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_skilled_citizens, beneficiary,
    organized, biographical, arbitrage, continental).

% Draw on a continent-wide labor pool without work-permit friction, arbitraging wage differentials between member states and relocating hiring to wherever labor is cheapest within the union. The constitutional character of free movement removes any single state's ability to protect a domestic wage floor against this arbitrage.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, federation_wide_employers, beneficiary,
    powerful, generational, arbitrage, continental).

% Adjudicate and enforce free movement as a constitutional right that member states cannot suspend unilaterally; treat the founding treaties as having created an irreversible legal order superior to national border authority. Courts and commissions strike down member-state attempts to restrict inflows, and treat 'exit' from the arrangement as requiring full federation withdrawal rather than partial renegotiation.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_institutions, agenda_setter,
    institutional, civilizational, analytical, continental).

% Absorb sudden influxes of mobile labor into specific regional job markets, sectors, and housing stock, with wage compression and job displacement concentrated on workers who lack the mobility or credentials to relocate themselves. They have no ability to petition their national government for a border remedy, since border restriction against federation citizens is treated as constitutionally illegitimate.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, biographical, trapped, regional).

% Compete directly against incoming mobile workers for entry-level and low-skill positions in border regions, bearing the brunt of wage compression while lacking the language skills, savings, or credentials to exercise the same free-movement right in reverse. Their local governments cannot erect the protections they would prefer because doing so would violate the federation's constitutional order.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, border_region_low_wage_workers, payer,
    powerless, biographical, trapped, local).

% Fund social services, unemployment support, and housing assistance for both incoming and displaced populations, without control over the inflow volume that drives fiscal exposure. Attempts to condition benefits on residency duration or contribution history are frequently challenged as inconsistent with free-movement guarantees.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Administer domestic labor and welfare policy but cannot restrict inbound federation citizens even during acute local economic stress; their only lever is full withdrawal from the federation, which carries severe economic and diplomatic costs. They simultaneously participate in setting supranational policy and bear its distributive consequences domestically.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, member_state_governments, agenda_setter).

% Argue that free movement should be a negotiable treaty provision subject to national border authority, not a constitutional right beyond member-state control. Their position is treated by supranational courts and institutions as outside the legitimate scope of policy debate once membership is established, so their objections are litigated as illegal rather than negotiated as policy.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, sovereignty_reading_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of enabling a continent-wide single labor and consumer market without per-border friction, permits, or discriminatory treatment among citizens of member states.
% TRANSFER_FUNCTION: Moves labor-market rents, wage-setting power, and fiscal exposure from geographically fixed, low-mobility populations and their local welfare systems toward mobile skilled citizens and employers who can arbitrage wage differentials across the whole federation.
% ABSENT_VOICES: Sovereignty-reading advocates and border-region low-wage workers would object that local labor markets and national democratic control over borders are being permanently overridden by a legal order they did not individually ratify at this level of intrusion; they are present in national politics but structurally excluded from the supranational adjudicative forum that actually decides the constitutional question.
% DISAPPEARANCE_RATIONALE: If the constitutional status of free movement disappeared overnight, member states would reintroduce work-permit and residency controls, employers would lose continent-wide labor arbitrage, mobile citizens would face new friction relocating, and local labor markets would regain a lever they currently lack — the entire continental labor market would reorganize around national border authority again.
% FOUNDING_PROBLEM: Post-war and post-integration planners sought to prevent the economic fragmentation and nationalist border competition that had repeatedly destabilized the continent, by locking free movement and market integration into the federation's founding treaties as irreversible commitments.
% FOUNDING_PROBLEM_CORROBORATION: Supranational institutions and mobile-citizen advocacy groups attest the founding problem (nationalist fragmentation, discriminatory border competition) remains live and requires constitutional protection. Independent labor economists and regional development agencies outside the beneficiary set have documented persistent, geographically concentrated wage and employment costs in border and low-skill labor markets, supporting the contested reading that the arrangement now also functions as an entrenched distributive transfer rather than purely a fragmentation-prevention measure.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderately-high and rising (0.38 to 0.68 over the interval) because the coordination benefit (a genuinely unified labor market) is real but its costs increasingly concentrate on immobile, low-skill, geographically fixed populations who cannot exercise the same right in reverse. Suppression is moderate (0.61 at end) because member states retain formal control of most domestic policy levers except this one — but the one lever withheld (border control against federation citizens) is treated as constitutionally beyond negotiation, which is a real and rising suppressive force as case law hardens the doctrine. Theater ratio is low-moderate (0.28): the coordination function (single labor market, mutual recognition, dispute resolution) is substantially real, not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the supranational institutional seat, this is settled constitutional coordination preventing a return to nationalist fragmentation. From the local-labor-market and border-region-worker seats, the same structure operates as an enforced, non-negotiable transfer mechanism with no domestic remedy. The engine should compute these divergently from the declared power/exit/scope data; the claimed_type (tangled_rope) already reflects that both a genuine coordination function and asymmetric extraction coexist structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile skilled citizens and federation-wide employers sit near the beneficiary end: they hold arbitrage-grade exit and their relationship to the constraint is subsidized by it. Local labor markets and border-region low-wage workers sit near the full-target end: trapped exit options, powerless standing, and the constraint's operation (constitutionally-protected inflow) directly compresses their wages and displaces their jobs with no available remedy. National welfare systems and member-state governments sit in a mixed position — institutional power but constrained exit, bearing fiscal exposure they cannot fully control.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (nationalist border competition destabilizing the continent) is contested as live versus dead: supranational institutions maintain it is still the operative justification, while independent labor-market evidence suggests the arrangement now also functions as an entrenched distributive mechanism independent of that original crisis-prevention rationale. Classifying this as tangled_rope (rather than pure rope or pure snare) prevents mislabeling: it preserves the genuine coordination achievement (a functioning continental market) while refusing to erase the asymmetric, non-negotiable costs imposed on immobile populations — the two must be held together, not resolved into either a clean coordination story or a clean extraction story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_vs_negotiability,
    'Is federation membership genuinely irreversible except through full withdrawal, or is this a contingent legal-interpretive construction that could be revised through treaty renegotiation without triggering exit?',
    'Examine whether any historical instance exists of partial renegotiation of free-movement terms without full federation withdrawal; absence of such precedent supports the irreversibility claim, presence would undermine it.',
    'If genuinely irreversible, the constitutional-right framing is structurally sound and the extraction on local labor markets is locked in by design. If actually negotiable but merely treated as irreversible by institutional practice, the suppression score understates a constructed rigidity that could be relaxed through ordinary politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_vs_negotiability, conceptual, 'Whether membership irreversibility is a structural fact or an institutional construction.').

omega_variable(
    labor_displacement_magnitude,
    'How much of the wage compression and job displacement observed in border regions and local labor markets is causally attributable to free movement specifically, versus automation, trade, or other macroeconomic forces?',
    'Longitudinal labor-economics studies isolating free-movement-driven migration flows from other displacement causes in comparable regional labor markets.',
    'If free movement is a major causal driver, the victim-side extraction claim is well-grounded. If displacement is primarily attributable to other forces, the extractiveness score attributed to this specific constraint should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_displacement_magnitude, empirical, 'Causal attribution of labor market harm to free movement versus other economic forces.').

omega_variable(
    kernel_framing_choice,
    'Is the more defensible framing of this kernel the free-movement/border-authority axis (as authored here), or an alternative framing centered on fiscal-transfer legitimacy (who pays into and draws from federation-wide welfare and cohesion funds)?',
    'Compare classification outcomes under a fiscal-transfer-centered framing of the same kernel: if beneficiary/victim sets and ε diverge substantially from the free-movement framing, that signals genuine framing under-determination requiring a third sibling story.',
    'If the fiscal-transfer framing would classify differently, this indicates the kernel actually decomposes into three or more constraints rather than two, and the current two-reading family is incomplete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the free-movement framing exhausts the kernel''s contested structure or a fiscal-transfer framing would classify differently.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t8, federation_membership__integration_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(fede_tr_t16, federation_membership__integration_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(fede_tr_t24, federation_membership__integration_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(fede_tr_t32, federation_membership__integration_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(fede_tr_t40, federation_membership__integration_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t8, federation_membership__integration_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(fede_be_t16, federation_membership__integration_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(fede_be_t24, federation_membership__integration_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(fede_be_t32, federation_membership__integration_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(fede_be_t40, federation_membership__integration_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fede_su_t8, federation_membership__integration_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(fede_su_t16, federation_membership__integration_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(fede_su_t24, federation_membership__integration_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(fede_su_t32, federation_membership__integration_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(fede_su_t40, federation_membership__integration_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership__integration_reading, 0.15).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story (integration_reading) and federation_membership__sovereignty_reading decompose the single natural-language label 'federation membership and free movement' into two structurally distinct constraints per the ε-invariance principle. The integration reading treats free movement as a constitutional right immune to member-state border authority, producing high ε concentrated on immobile local labor and welfare systems. The sovereignty reading treats free movement as negotiable treaty policy subject to national border control, producing a different, generally lower ε since border restriction remains an available remedy. The two are linked bidirectionally; a change in either reading's institutional dominance (e.g. a supranational court ruling, or a member-state referendum) structurally pressures the other reading's legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
