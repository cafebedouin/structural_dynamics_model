% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Free Movement Conditioned on Member State Consent (Sovereignty-Primary Reading)
 *   domain: political/economic/federalism
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primary reading of the federation
 *   membership treaty's free movement kernel: free movement exists, but its
 *   exercise is conditional at every point on member state consent, expressed
 *   through safeguard clauses, transitional restrictions, and welfare
 *   residency tests. Over the interval, invocation of these mechanisms has
 *   intensified modestly as accession has widened to states with more
 *   divergent labor market conditions, producing gradual increases in
 *   effective extraction and enforcement machinery even though the treaty
 *   text has not changed. This reading treats the restriction apparatus as a
 *   legitimate and durable feature of the bargain, not a transitional defect
 *   — that is exactly what distinguishes it from the integration-primary
 *   reading, which treats the same apparatus as an illegitimate derogation
 *   pending removal.
 *
 * KEY AGENTS:
 *   - member_state_governments: agenda_setter/beneficiary (institutional/arbitrage) — administers and benefits from consent mechanisms
 *   - national_labor_markets and domestic_welfare_systems: beneficiaries (organized-institutional/constrained) — shielded from movement shocks
 *   - mobile_workers, cross_border_jobseekers, posted_workers_from_poorer_states: payers (moderate-to-powerless/constrained-trapped) — bear restricted access
 *   - federation_court: observer (institutional/analytical) — adjudicates the boundary of permissible restriction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.58).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.52).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Free Movement Conditioned on Member State Consent (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political/economic/federalism").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '8afc3bb8-76a1-4f00-a59a-88d503aee396').
narrative_ontology:cs_kernel_codification('8afc3bb8-76a1-4f00-a59a-88d503aee396', fixed_text).
narrative_ontology:cs_authority_grounding('8afc3bb8-76a1-4f00-a59a-88d503aee396', lineage).
narrative_ontology:cs_interpretation_layer_present('8afc3bb8-76a1-4f00-a59a-88d503aee396').
narrative_ontology:cs_reading_relation('8afc3bb8-76a1-4f00-a59a-88d503aee396', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('8afc3bb8-76a1-4f00-a59a-88d503aee396', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('8afc3bb8-76a1-4f00-a59a-88d503aee396', foundational, state_consent_is_the_operative_legitimating_principle).
narrative_ontology:cs_axiom_status(state_consent_is_the_operative_legitimating_principle, holdable).
narrative_ontology:cs_axiom_grounding('8afc3bb8-76a1-4f00-a59a-88d503aee396', state_consent_is_the_operative_legitimating_principle, conventional).
narrative_ontology:cs_axiom('8afc3bb8-76a1-4f00-a59a-88d503aee396', secondary, national_welfare_solvency_takes_priority_over_uniform_access).
narrative_ontology:cs_axiom_status(national_welfare_solvency_takes_priority_over_uniform_access, holdable).
narrative_ontology:cs_axiom_grounding('8afc3bb8-76a1-4f00-a59a-88d503aee396', national_welfare_solvency_takes_priority_over_uniform_access, instrumental).
narrative_ontology:cs_reference_frame('8afc3bb8-76a1-4f00-a59a-88d503aee396', intergovernmental_accession_bargain).
narrative_ontology:cs_drift_state('8afc3bb8-76a1-4f00-a59a-88d503aee396', post_eastern_enlargement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8afc3bb8-76a1-4f00-a59a-88d503aee396', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, domestic_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, incumbent_national_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_jobseekers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, posted_workers_from_poorer_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain treaty-recognized authority to impose transitional labor market restrictions, welfare residency requirements, and emergency safeguard clauses on inbound movement. Administer work permits, benefit eligibility tests, and quota mechanisms. Can invoke safeguard procedures unilaterally when domestic political pressure rises, and can slow-walk mutual recognition of qualifications.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, member_state_governments, beneficiary).

% Domestic wage floors and employment structures are shielded from the full force of cross-border labor supply shocks. Sectoral unions and employer associations lobby for the restrictions and treat them as protecting bargaining position; the buffer is credited with slowing wage compression in exposed sectors.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_markets, beneficiary,
    organized, generational, constrained, national).

% Benefit eligibility is conditioned on residency duration and habitual-residence tests that fall disproportionately on recent arrivals, preserving system solvency projections and insulating the contribution base from sudden expansion of claimants.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, domestic_welfare_systems, beneficiary,
    institutional, generational, constrained, national).

% Face reduced competition for jobs and welfare slots from mobile workers in periods when safeguard clauses are active. Retain full mobility rights themselves and are not structurally trapped by the arrangement — their gain is the buffered labor market, not a restriction on their own movement.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, incumbent_national_workers, beneficiary,
    moderate, biographical, mobile, national).

% Face transitional arrangements, quotas, permit backlogs, and welfare residency clocks when moving between member states. Their formal treaty right to move is real but conditioned at the point of exercise on the destination state's active consent, which can be withdrawn or tightened without their participation in the decision.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    moderate, biographical, constrained, continental).

% Arrive seeking work and encounter the sharpest edge of the consent mechanism — jobseeker residence limits, proof-of-genuine-prospect tests, and welfare exclusion during the search period. Have the least leverage of any affected group and the shortest time horizon before exit becomes involuntary.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_jobseekers, payer,
    powerless, immediate, trapped, continental).

% Sent by employers in lower-wage member states to work temporarily in higher-wage states under posting rules; subject to host-state restrictions on duration, sector, and social benefit access designed explicitly to prevent their movement from depressing local wage floors. Their labor generates value captured largely by employers and the destination market's incumbents, not by themselves.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, posted_workers_from_poorer_states, payer,
    powerless, biographical, constrained, continental).

% Would prefer faster, less restricted access to the continental labor pool to fill vacancies and would object to safeguard clauses and permit delays; their objections are heard through commercial lobbying channels but do not have a formal veto over member state consent decisions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, employers_in_labor_scarce_regions, excluded,
    organized, biographical, constrained, national).

% Adjudicates disputes over whether specific national restrictions exceed what the treaty's consent-reservation permits, producing case law that narrows or widens the sovereignty-primary reading's practical scope without displacing the underlying premise that consent is conditional.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_court, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows member states to join a continental mobility framework while retaining a credible mechanism to protect domestic labor markets and welfare solvency from movement shocks they did not individually choose the pace of — this is what makes political accession and continued membership sustainable for states with weaker labor markets or younger welfare systems.
% TRANSFER_FUNCTION: Moves exposure to labor market competition and welfare claims away from incumbent workers and domestic welfare systems and onto mobile workers, jobseekers, and posted workers, who absorb permit delays, residency-based exclusion, and wage suppression in the destination market as the cost of the arrangement's political durability.
% ABSENT_VOICES: Mobile workers and jobseekers themselves have no seat in the intergovernmental negotiations that set safeguard clause thresholds or residency test parameters — those are negotiated state-to-state. Employers in labor-scarce regions lobby but do not control the consent decision either.
% DISAPPEARANCE_RATIONALE: If member state consent authority disappeared overnight and free movement became unconditional, national labor markets in states with active safeguard clauses would face immediate unshielded competition, welfare systems would face expanded near-term claimant pools, and the political coalition sustaining several states' continued federation membership would be put under acute strain — the arrangement is load-bearing for the federation's own survival, not decorative.
% FOUNDING_PROBLEM: The federation could not secure or retain the accession of member states with weaker labor markets, younger welfare systems, or politically volatile immigration debates unless those states retained a credible, treaty-recognized capacity to slow or condition inbound movement during transition periods and shocks.
% FOUNDING_PROBLEM_CORROBORATION: Independent federation-level economic reviews and the federation court's own case law confirm the safeguard mechanisms are still actively invoked during accession transitions and labor market shocks, not merely retained as unused treaty text; migrant advocacy organizations and posted-worker unions — outside the beneficiary set — corroborate that the mechanisms materially restrict access in practice, which supports treating the founding problem as live rather than resolved-but-persisting.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58) reflects a real but bounded transfer: mobile workers and posted workers absorb quota delays, residency-based welfare exclusion, and wage suppression, while incumbent structures are shielded. Suppression (0.52) is moderate rather than severe because formal movement rights persist and the federation court provides a check on how far restriction can go; it is not zero because safeguard invocation is backed by real enforcement (border checks, permit denial, benefit refusal) that mobile workers cannot bypass by appeal alone. Theater ratio (0.28) is modest — the mechanisms do genuine work protecting labor markets and welfare solvency, they are not primarily performative, though a growing share of invocation is politically symbolic (safeguard clauses triggered for domestic signaling rather than demonstrated labor market harm), which the rising trajectory partly captures.
 *
 * PERSPECTIVAL GAP:
 *   Member state governments and national labor market constituencies experience the arrangement as protective infrastructure they built and can adjust; mobile workers and posted workers experience the identical clauses as an access barrier applied to them without their participation in setting its terms. The engine computes this divergence from the declared power/exit/scope data across the two seat clusters rather than from any authored claim about which experience is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments and the constituencies they answer to (domestic labor markets, welfare systems, incumbent workers) are declared beneficiaries — the consent mechanism exists to protect their position and they administer it. Mobile workers, jobseekers, and posted workers are declared victims because the same mechanism's operation is what restricts their access; they bear the cost of the transfer through delay, exclusion, and suppressed bargaining power in destination markets, with the least mobile (jobseekers, posted workers from poorer states) sitting closest to the full-target end of directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (accession sustainability for labor-market-divergent states) is authored as live, not dead — safeguard clauses are still actively invoked, not merely retained as unused treaty text. This blocks a premature snare classification: the coordination function (making federation membership politically viable for weaker-labor-market states) is real and ongoing, which is why the type is authored as tangled_rope rather than pure snare — the story requires both the coordination story to be genuine AND the extraction to be real, and both conditions are met here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_reservation_vs_market_constitution,
    'Is member state consent the treaty''s foundational organizing principle (this reading) or a derogation from a constitutively free market that the integration-primary reading treats as primary?',
    'Textual and drafting-history analysis of the treaty''s accession protocols, combined with federation court jurisprudence on the burden of proof for invoking safeguard clauses — a consent-primary reading places the burden on movement-rights claimants; an integration-primary reading places it on the restricting state.',
    'If the drafting history and case law place the burden on the restricting state, the integration-primary reading has stronger textual support and this reading''s extraction/suppression figures would be read as evidence of illegitimate derogation rather than legitimate treaty operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_reservation_vs_market_constitution, conceptual, 'Where the kernel''s disagreement is located: burden of justification for restriction, not the existence of the restriction mechanism itself.').

omega_variable(
    safeguard_invocation_genuineness,
    'Are safeguard clause invocations tracking genuine, demonstrable labor market or welfare system strain, or increasingly tracking domestic political signaling independent of measured strain?',
    'Compare safeguard invocation timing and duration against independent labor market indicators (unemployment among comparable domestic cohorts, welfare system dependency ratios) in the invoking states.',
    'If invocation has decoupled from measured strain, the rising theater_ratio trajectory understates the drift and the coordination function is weaker than authored — pushing the classification toward snare; if invocation tracks strain, tangled_rope is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safeguard_invocation_genuineness, empirical, 'Whether rising restriction invocation reflects genuine coordination need or accumulating extraction dressed as coordination.').

omega_variable(
    posted_worker_capture_ambiguity,
    'Do posted workers from poorer member states net-benefit from access to higher-wage destination markets despite the restrictions (their home-state wage counterfactual is lower), complicating their status as pure victims?',
    'Compare posted workers'' realized earnings and conditions against their best available home-state counterfactual, net of restriction-imposed costs (duration limits, benefit exclusion, employer capture of wage differential).',
    'If posted workers substantially benefit net of restriction costs relative to their counterfactual, their directionality is less purely victim-coded than authored, and the extraction figure may overstate their net cost; if employers capture most of the differential, the victim coding holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posted_worker_capture_ambiguity, empirical, 'Whether posted workers are unambiguous victims or partial beneficiaries relative to their counterfactual, net of employer capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t4, federation_membership_treaty__sovereignty_primary, theater_ratio, 4, 0.17).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__sovereignty_primary, theater_ratio, 8, 0.2).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__sovereignty_primary, theater_ratio, 12, 0.23).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__sovereignty_primary, theater_ratio, 16, 0.25).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.27).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__sovereignty_primary, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t4, federation_membership_treaty__sovereignty_primary, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__sovereignty_primary, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__sovereignty_primary, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__sovereignty_primary, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__sovereignty_primary, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fede_su_t4, federation_membership_treaty__sovereignty_primary, suppression_requirement, 4, 0.41).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__sovereignty_primary, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__sovereignty_primary, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__sovereignty_primary, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__sovereignty_primary, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the federation_membership_treaty kernel, decomposed per the ε-invariance principle: the natural-language concept 'free movement conditionality' resolves differently depending on which principle (consent, market constitution, or proportionality) is read as foundational. sovereignty_primary (this file) authors ε=0.58 with local labor markets and welfare systems as beneficiaries and mobile/posted workers as victims; integration_primary would author markedly lower ε and different beneficiary/victim structure (restrictions as illegitimate derogation, mobile workers closer to beneficiaries of the underlying right); subsidiarity_balance would author an intermediate ε with proportionality review as the operative constraint on both restriction and movement. All three are linked here rather than merged because their claimed_type, ε, and stakeholder structure differ substantially, not merely their evaluative gloss on shared facts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
