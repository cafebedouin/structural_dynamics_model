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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Free Movement Constitutional Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint models the INTEGRATION READING of the
 *   federation-membership kernel: free movement is treated as a
 *   constitutional right flowing from irreversible pooled sovereignty, and
 *   supranational authority to adjudicate it is treated as legitimate
 *   independent of any single member state's current consent. Under this
 *   reading, member states cannot unilaterally restrict inflow without
 *   challenging the federation's foundational commitments. This is NOT a
 *   description of the sovereignty reading (where membership is a conditional
 *   treaty and border control retains national legitimacy) — that is a
 *   separate constraint, linked here structurally, not blended into this
 *   one's metrics.
 *
 * KEY AGENTS:
 *   - supranational_administrative_bodies: agenda-setter administering the constitutional framing
 *   - mobile_citizens: primary beneficiary, high mobility and arbitrage exit
 *   - cross_border_employers: secondary beneficiary, powerful and mobile
 *   - local_labor_markets and peripheral_wage_workers: primary payers, trapped exit
 *   - border_region_public_services: fiscal payer without commensurate transfer
 *   - member_state_governments_seeking_restriction: excluded voice, structurally foreclosed from restricting inflow
 *   - federal_courts: analytical observer reinforcing the constitutional characterization
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
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Free Movement Constitutional Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '7ab25309-1355-49d3-bce6-381b83ea6092').
narrative_ontology:cs_kernel_codification('7ab25309-1355-49d3-bce6-381b83ea6092', formalized).
narrative_ontology:cs_authority_grounding('7ab25309-1355-49d3-bce6-381b83ea6092', extraction).
narrative_ontology:cs_interpretation_layer_present('7ab25309-1355-49d3-bce6-381b83ea6092').
narrative_ontology:cs_reading_relation('7ab25309-1355-49d3-bce6-381b83ea6092', federation_membership__sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('7ab25309-1355-49d3-bce6-381b83ea6092', foundational, pooled_sovereignty_is_irrevocable).
narrative_ontology:cs_axiom_status(pooled_sovereignty_is_irrevocable, holdable).
narrative_ontology:cs_axiom_grounding('7ab25309-1355-49d3-bce6-381b83ea6092', pooled_sovereignty_is_irrevocable, conventional).
narrative_ontology:cs_axiom('7ab25309-1355-49d3-bce6-381b83ea6092', foundational, free_movement_is_constitutional_not_policy).
narrative_ontology:cs_axiom_status(free_movement_is_constitutional_not_policy, holdable).
narrative_ontology:cs_axiom_grounding('7ab25309-1355-49d3-bce6-381b83ea6092', free_movement_is_constitutional_not_policy, deontological).
narrative_ontology:cs_reference_frame('7ab25309-1355-49d3-bce6-381b83ea6092', founding_treaty_internal_market_completion).
narrative_ontology:cs_drift_state('7ab25309-1355-49d3-bce6-381b83ea6092', contemporary_post_enlargement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7ab25309-1355-49d3-bce6-381b83ea6092', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_administrative_bodies).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, peripheral_wage_workers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, border_region_public_services).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, supranational_authority_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, irreversible_pooled_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the free-movement guarantee as a constitutional right rather than a treaty concession, adjudicates member-state attempts to restrict it, and treats accession as a one-way ratchet — no exit path for the free-movement provision short of full federation withdrawal. Collects legitimacy and jurisdiction from the irreversibility framing itself.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_administrative_bodies, agenda_setter,
    institutional, civilizational, analytical, continental).

% Can relocate, work, and access services anywhere in the federation without visas or labor-market tests. This population is disproportionately higher-skilled and capital-mobile; they move toward wage differentials and treat the right as settled constitutional bedrock rather than a policy the current majority could revoke.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    organized, biographical, arbitrage, continental).

% Draws on a continent-wide labor pool without the friction of work-visa sponsorship, wage floors set by national labor law, or local-hire requirements. Lobbies to keep free movement classified as an inviolable constitutional right precisely because that classification removes it from ordinary political bargaining.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, cross_border_employers, beneficiary,
    powerful, generational, mobile, continental).

% Wage levels and employment in low-skill and mid-skill sectors compress as inbound labor supply from lower-wage member states increases faster than local demand absorbs it. Because free movement is framed as a constitutional right, local political representatives cannot legislate labor-market protections without challenging the federation's foundational commitments — an option effectively foreclosed.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, biographical, trapped, regional).

% Compete directly against newly arrived mobile labor for the same jobs and cannot relocate as easily themselves — lacking savings, language capital, or portable credentials. Bear the wage and displacement costs of the arrangement without access to the same arbitrage mobile citizens exercise.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, peripheral_wage_workers, payer,
    powerless, biographical, trapped, local).

% Schools, housing authorities, and clinics in high-inflow regions absorb population increases without proportional fiscal transfers from the federation, since free movement is treated as a rights matter rather than a resourced program. Local administrators can complain but cannot restrict inflow without violating the constitutional guarantee.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, border_region_public_services, payer,
    moderate, immediate, constrained, regional).

% Would prefer to negotiate labor-market safeguards, transitional controls, or emergency brakes on inflow in response to domestic political pressure, but the integration reading treats such proposals as illegitimate challenges to a constitutional right rather than negotiable policy adjustments — their objections are heard but structurally cannot prevail within this reading's framework.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, member_state_governments_seeking_restriction, excluded,
    institutional, biographical, trapped, national).

% Rules on disputes between member states and the supranational body over the scope of free movement, generally upholding the constitutional characterization and striking down member-state restrictions, thereby reinforcing the irreversibility premise this reading depends on.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, federal_courts, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, diffuse).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a genuinely large, integrated labor and consumer market: firms hire across the whole federation without visa friction, workers can move toward opportunity, and a single legal standard replaces dozens of bilateral labor treaties.
% TRANSFER_FUNCTION: Moves labor-market rents from locally-rooted, less-mobile workers and the fiscal capacity of high-inflow border regions toward mobile citizens and the employers who hire them, under the protection of a constitutional (rather than renegotiable) legal status.
% ABSENT_VOICES: Local labor unions and border-region municipal governments raise displacement and service-capacity concerns in domestic politics but have no forum within the federation's own constitutional order to contest the free-movement guarantee itself — objections register as noise against a settled right, not as claims requiring adjudication.
% DISAPPEARANCE_RATIONALE: If free movement lost its constitutional status and reverted to ordinary treaty policy, member states would immediately reintroduce labor-market tests, emergency brakes, and quotas in response to domestic pressure; cross-border employers would face renewed hiring friction; and the federation's labor market would fragment along national lines within a single electoral cycle.
% FOUNDING_PROBLEM: Fragmented national labor markets prevented workers from moving to opportunity and prevented employers from accessing the scale needed to compete globally; free movement was designed to complete the internal market the federation's founding treaties promised.
% FOUNDING_PROBLEM_CORROBORATION: Supranational bodies and mobile-citizen advocacy groups attest the founding problem remains live and the constitutional framing is necessary to prevent backsliding. Independent labor economists studying border regions and municipal associations outside the federation's own institutions attest that the arrangement has shifted from solving labor-market fragmentation to imposing undercompensated adjustment costs on peripheral and low-mobility populations — a shift the constitutional framing insulates from renegotiation.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.68 at interval end) reflecting labor displacement and uncompensated fiscal absorption in border regions, growing over the interval as cumulative inflow compounds without matching transfer mechanisms. Suppression (0.61) reflects the structural foreclosure of the ordinary political remedy (border/labor-market restriction) rather than physical coercion — the suppression here is doctrinal: the constitutional characterization itself removes the option from legislative reach. Theater ratio is moderate-low (0.28) because the coordination function (integrated labor market) is real and substantial, not merely performed, but a growing share of enforcement activity (court rulings striking down member-state safeguards) defends the irreversibility doctrine rather than delivering net-new coordination value.
 *
 * PERSPECTIVAL GAP:
 *   From the supranational agenda-setter's seat, this is settled constitutional law protecting a foundational right — Mountain-like in its own self-conception. From the trapped local-labor-market seat, the same structure operates as an actively-enforced transfer mechanism that forecloses their political remedies. The engine computes this seat divergence from the structural exit-option and power data; the claimed_type (tangled_rope) is authored as the analytical seat's best read, independent of either party's self-characterization.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens and cross-border employers sit near the beneficiary end: they capture wage-arbitrage and hiring-scale gains and hold arbitrage/mobile exit options that let them respond to opportunity rather than being fixed by circumstance. Local labor markets, peripheral wage workers, and border-region public services sit near the target end: trapped or constrained exit, no comparable capacity to relocate or reallocate costs, and bear a transfer they cannot legislate against because the transfer is constitutionally insulated. Supranational administrative bodies are the agenda-setter whose institutional legitimacy is itself the vindicated proposition this reading protects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented national labor markets blocking efficient allocation) was genuinely live at the federation's founding and partially remains live in aggregate economic terms — this prevents dismissing the whole arrangement as pure extraction. But the CONSTITUTIONAL, irreversible framing of the remedy is the mandatrophied element: a policy tool (labor mobility) has hardened into an unamendable right, foreclosing exactly the adjustment mechanisms (transitional controls, emergency brakes, compensatory transfers) that would let the coordination function persist while redistributing its costs. Classifying this as tangled_rope rather than snare or rope prevents both errors: it is not pure extraction (the coordination gain for mobile citizens and cross-continental employers is real) and it is not clean coordination (the enforcement apparatus exists specifically to suppress the compensating adjustments local losers would otherwise obtain through ordinary politics).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_as_genuine_commitment_vs_ratchet_doctrine,
    'Is the irreversibility of federation membership and free movement a genuine, functionally necessary commitment device (preventing destructive short-term defection from a positive-sum arrangement) or a doctrinal ratchet that has been extended beyond its original coordination purpose to insulate a specific distributional outcome from renegotiation?',
    'Compare federations/unions with genuine exit or renegotiation clauses for internal labor-mobility provisions against this federation''s outcomes: if labor-market and fiscal metrics in border regions are not meaningfully worse under renegotiable regimes, irreversibility is not functionally necessary and the doctrine reads as constructed rent-protection.',
    'If irreversibility is functionally necessary, the high suppression score partially reflects legitimate commitment-device cost rather than pure extraction-protection, softening the tangled_rope reading toward rope. If it is not necessary, the doctrine is closer to pure extraction protection layered on a real coordination core.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreversibility_as_genuine_commitment_vs_ratchet_doctrine, conceptual, 'Whether membership irreversibility is a necessary commitment device or a distributional ratchet.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly does the integration reading and the sovereignty reading disagree — is it a factual dispute about whether restriction would actually improve outcomes, or an irreducible normative dispute about whether supranational authority can ever legitimately bind a member state without ongoing consent?',
    'This is not resolvable by data alone; it requires distinguishing the empirical claim (restriction would/would not improve border-region outcomes) from the normative claim (consent must be ongoing vs. can be pooled irrevocably at founding). The two readings could converge empirically while remaining normatively opposed.',
    'If the disagreement is purely normative, the two constraint stories (integration_reading and sovereignty_reading) will never converge in ε regardless of new data — they are genuinely different constraints, not different estimates of one constraint, confirming the decomposition was correct rather than a modeling artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the kernel contest between readings is empirical or irreducibly normative.').

omega_variable(
    fiscal_transfer_compensation_availability,
    'Could a fiscal transfer mechanism compensate border regions and displaced workers for absorption costs while preserving free movement''s constitutional status, or does the constitutional framing itself foreclose the political process needed to establish such transfers?',
    'Examine whether any federation-level compensatory transfer program has been proposed, and what political or legal obstacles it encountered; track whether the same courts that strike down restriction proposals have ever mandated or upheld compensation requirements.',
    'If compensation is legally and politically available but simply not adopted, extraction is a policy failure correctable within the current reading (softening severity). If the constitutional framing itself blocks compensation as a form of restriction-by-other-means, the extraction is structurally locked in by the same doctrine that produces the coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_transfer_compensation_availability, empirical, 'Whether compensatory mechanisms for displaced local labor are structurally available or foreclosed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t8, federation_membership__integration_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(fede_tr_t16, federation_membership__integration_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(fede_tr_t24, federation_membership__integration_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(fede_tr_t32, federation_membership__integration_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(fede_tr_t40, federation_membership__integration_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t8, federation_membership__integration_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(fede_be_t16, federation_membership__integration_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(fede_be_t24, federation_membership__integration_reading, base_extractiveness, 24, 0.61).
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
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story and federation_membership__sovereignty_reading are two readings of the same kernel (federation_membership), decomposed per the ε-invariance principle: measuring 'is free movement legitimate' through the integration reading's lens (constitutional right, irreversible pooled sovereignty) yields high ε from locked-in labor displacement; measuring it through the sovereignty reading's lens (conditional treaty, retained national border authority) would yield a structurally different, lower ε because restriction remains a live, legitimate remedy in that reading. They are not the same constraint measured two ways — they are two constraints with different beneficiary/victim sets, different claimed types, and different persistence conditions, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
