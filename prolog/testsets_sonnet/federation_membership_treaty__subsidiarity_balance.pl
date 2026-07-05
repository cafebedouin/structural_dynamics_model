% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Proportionality-Balanced Free Movement Regime
 *   domain: political economy / federalism / migration policy
 *
 * SUMMARY:
 *   This constraint instantiates the subsidiarity-balance reading of the
 *   federation membership treaty kernel: free movement as a right bounded by
 *   proportionality, where national interests can justify graduated
 *   restrictions but not categorical exclusion. This is structurally distinct
 *   from the integration_primary reading (which treats restrictions as
 *   presumptively illegitimate) and the sovereignty_primary reading (which
 *   treats mobility as conditional on state consent) — those are separate
 *   constraints with separate ε values, linked here via network edges, not
 *   folded into this one. Under this reading, the coordination function
 *   (avoiding both welfare-shock and market-fragmentation) is real, but the
 *   judicial body administering the balance accumulates interpretive power,
 *   and specific groups — posted low-wage workers and sending-region
 *   communities — absorb costs the balancing test was never designed to
 *   weigh.
 *
 * KEY AGENTS:
 *   - federation_judicial_authority: agenda_setter (institutional/analytical) — administers the proportionality test and accumulates interpretive authority
 *   - mobile_skilled_workers: primary beneficiary (moderate/mobile) — mobility functions closest to advertised for this group
 *   - posted_low_wage_workers: primary target (powerless/constrained) — bears costs of the balance without a real litigation voice
 *   - cross_border_employers: secondary beneficiary (powerful/arbitrage) — exploits the regulatory gap the balance leaves open
 *   - sending_state_regions_facing_brain_drain: excluded cost-bearer (powerless/trapped) — has no standing in the balancing test at all
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.42).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.48).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.42).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Proportionality-Balanced Free Movement Regime").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political economy / federalism / migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, 'c159a861-5355-4f39-8598-c5b8c209f051').
narrative_ontology:cs_kernel_codification('c159a861-5355-4f39-8598-c5b8c209f051', formalized).
narrative_ontology:cs_authority_grounding('c159a861-5355-4f39-8598-c5b8c209f051', lineage).
narrative_ontology:cs_interpretation_layer_present('c159a861-5355-4f39-8598-c5b8c209f051').
narrative_ontology:cs_reading_relation('c159a861-5355-4f39-8598-c5b8c209f051', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('c159a861-5355-4f39-8598-c5b8c209f051', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('c159a861-5355-4f39-8598-c5b8c209f051', foundational, proportionality_as_adjudicative_default).
narrative_ontology:cs_axiom_status(proportionality_as_adjudicative_default, holdable).
narrative_ontology:cs_axiom_grounding('c159a861-5355-4f39-8598-c5b8c209f051', proportionality_as_adjudicative_default, conventional).
narrative_ontology:cs_axiom('c159a861-5355-4f39-8598-c5b8c209f051', foundational, national_interest_constrains_but_does_not_eliminate_mobility).
narrative_ontology:cs_axiom_status(national_interest_constrains_but_does_not_eliminate_mobility, holdable).
narrative_ontology:cs_axiom_grounding('c159a861-5355-4f39-8598-c5b8c209f051', national_interest_constrains_but_does_not_eliminate_mobility, instrumental).
narrative_ontology:cs_reference_frame('c159a861-5355-4f39-8598-c5b8c209f051', graduated_proportionality_equilibrium).
narrative_ontology:cs_drift_state('c159a861-5355-4f39-8598-c5b8c209f051', post_enlargement_labor_mobility_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c159a861-5355-4f39-8598-c5b8c209f051', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_skilled_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, host_state_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, federation_judicial_authority).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, posted_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, host_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, sending_state_regions_facing_brain_drain).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, host_state_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, national_governments_seeking_restriction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates proportionality case-by-case: whether a member state's restriction on mobility is a legitimate, narrowly-tailored protection of a genuine public interest or a disguised barrier to movement. Builds doctrine incrementally through rulings rather than a fixed bright-line rule, and in doing so accumulates interpretive authority over both member states and mobile persons.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_judicial_authority, agenda_setter,
    institutional, generational, analytical, continental).

% Cross borders for employment holding recognized qualifications and language capital; proportionality doctrine mostly protects their mobility because restrictions targeting them rarely survive the narrow-tailoring test. Their exit options remain genuinely open — this is the group for whom the regime functions closest to advertised.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_skilled_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Sent by employers to work temporarily in higher-wage member states under posting arrangements; formally covered by free movement but subject to a thicket of proportionality-justified host-state labor protections (minimum wage floors, registration duties, sectoral restrictions) that employers pass costs of onto them through reduced take-home pay or precarious status. Cannot easily litigate the proportionality of restrictions that affect them; they experience the doctrine's balancing as something done to them, not for them.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, posted_low_wage_workers, payer,
    powerless, immediate, constrained, continental).

% Domestic workers and unions benefit when proportionality doctrine upholds wage floors and registration requirements against low-cost incoming labor, but bear costs when the doctrine strikes down protective measures as disproportionate. Their position shifts case by case depending on which line of doctrine prevails, giving them a stake in continually contesting where the proportionality line sits.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, host_state_labor_markets, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, host_state_labor_markets, payer).

% Must extend certain benefits to mobile citizens under proportionality-tempered non-discrimination rules while retaining some but not unlimited ability to impose residency or contribution conditions. Bears the fiscal cost of benefit access that the proportionality standard permits but cannot fully exclude, and must continuously defend residency-based restrictions in litigation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, host_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Peripheral regions and towns that lose skilled and working-age population to outward mobility have no standing in the proportionality analysis at all — the doctrine balances host-state interests against mobility rights but has no mechanism for weighing sending-region depopulation costs. They bear a structural cost the framework was never built to register.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sending_state_regions_facing_brain_drain, payer,
    powerless, generational, trapped, regional).

% Structure workforces to exploit wage and regulatory differentials across member states, using posting arrangements and mobility rights to access lower-cost labor while proportionality doctrine limits how far host states can restrict this without appearing to violate free movement. Can relocate operations or postings in response to doctrinal shifts, giving them the most robust exit option of any stakeholder.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, cross_border_employers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Elected governments responding to domestic political pressure to restrict mobility (housing pressure, wage competition, integration strain) find their preferred policies subject to override if judicial authority deems them disproportionate. Their democratic mandate carries weight in political discourse but not dispositive weight in the proportionality test itself, which is set by judicial doctrine rather than electoral outcome.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, national_governments_seeking_restriction, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, national_governments_seeking_restriction, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework letting a federation of states maintain a genuine internal mobility right while allowing member states some room to protect specific, demonstrable public interests (public health, core labor standards, fiscal sustainability of welfare systems) — avoiding both the instability of unrestricted mobility crashing into unprepared welfare/labor systems and the fragmentation of member states unilaterally re-erecting full border controls.
% TRANSFER_FUNCTION: Moves adjudicative authority over the boundary between mobility rights and national regulatory autonomy from national legislatures to federation-level judicial bodies; moves labor-cost advantages from posted/low-wage workers to employers who can arbitrage regulatory gaps; moves fiscal exposure from mobile individuals to host-state welfare systems within the bounds the doctrine permits.
% ABSENT_VOICES: Sending-region communities facing depopulation and brain drain have no seat in the proportionality balancing test, which is framed entirely around host-state interest versus mobility right. National governments facing electoral pressure to restrict mobility further than the doctrine allows are heard in political discourse but cannot override judicial proportionality findings through ordinary legislation.
% DISAPPEARANCE_RATIONALE: If the proportionality-balance framework disappeared, either full unrestricted mobility or full member-state sovereign control over borders would have to fill the vacuum — both a substantial departure from the current graduated equilibrium. Employers would lose the specific arbitrage opportunities created by partial-but-not-full harmonization; welfare systems would either face unrestrained benefit claims or regain full gatekeeping power; posted workers' protections would shift entirely depending on which alternative regime replaced this one.
% FOUNDING_PROBLEM: Early federation design faced two extremes that each threatened the union's survival: fully open borders risked welfare-system collapse and political backlash in wealthier states, while fully sovereign border control risked undermining the single market and fragmenting the union's core economic rationale. Proportionality doctrine was built to let member states retain narrowly-tailored defensive measures without licensing wholesale retreat from free movement.
% FOUNDING_PROBLEM_CORROBORATION: Federation judicial authority and integration-oriented member states attest the founding problem remains live — labor market and welfare shocks from unrestricted mobility are cited as an ongoing risk requiring case-by-case balancing. Independent labor economists and posted-worker advocacy organizations, outside both the judicial authority's own institutional interest and the employer beneficiary group, attest that the doctrine now functions less to prevent shock and more to manage employer arbitrage and depoliticize a contested policy question by routing it through courts rather than legislatures.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).
:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) reflecting genuine coordination value alongside real but non-maximal extraction concentrated on posted workers and sending regions. Suppression is moderate (0.48) because the doctrine actively constrains BOTH unrestricted mobility claims and blanket national restrictions — it is not a pure liberty-maximizing or pure sovereignty-maximizing regime, consistent with the graduated structural delta specified for this reading. Theater ratio is modest but rising (0.12 to 0.28) as the doctrine matures and case volume increases without proportionate resolution of underlying labor/welfare tension — some proportionality litigation increasingly serves to legitimate outcomes reached on other grounds rather than to genuinely test necessity.
 *
 * PERSPECTIVAL GAP:
 *   The judicial authority experiences this constraint as principled boundary-drawing exercising real analytical discretion. Mobile skilled workers experience it as functioning protection. Posted low-wage workers and sending-region communities experience the same structure as a balance struck without them, whose costs are structurally invisible to the test itself. Cross-border employers experience it as a navigable, arbitrage-rich environment. The engine's per-seat computation should register these as genuinely different structural positions, not merely different opinions about one position.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile skilled workers and cross-border employers sit near the beneficiary end: mobility functions as advertised for the former, and the latter can arbitrage differentials with mobile exit options. Posted low-wage workers sit near the target end: constrained exit, immediate time horizon, no meaningful litigation access to the doctrine that governs their situation. Sending-region communities sit at the extreme target end despite not being formal 'victims' of an enforcement action — their exit option is trapped (regional decline is not something individuals can easily arbitrage away from at the community level) and the doctrine structurally excludes their interest from the balancing calculus altogether. Host-state welfare systems and labor markets sit near symmetric — protected in some rulings, overridden in others, contingent on which proportionality line prevails in a given case.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing both welfare-shock from unrestricted mobility and market-fragmentation from unrestricted sovereignty) still has partisans who assert it is live, but independent corroboration suggests the doctrine has partly shifted from crisis-prevention toward routine management of employer arbitrage and depoliticized adjudication of what were originally legislative questions. Classifying this as tangled_rope rather than snare or rope preserves the genuine coordination function (a graduated balance IS better than either extreme for many stakeholders) while registering the asymmetric extraction on posted workers and sending regions that a pure-rope classification would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_selection,
    'Is the subsidiarity_balance reading the structurally dominant interpretation of the federation membership treaty''s free movement provisions, or is it itself unstable, sliding toward either the integration_primary or sovereignty_primary reading depending on which political coalition controls appointments to the judicial authority?',
    'Track the judicial authority''s ruling pattern over multiple electoral and appointment cycles: a stable proportionality doctrine would show consistent case outcomes independent of coalition composition; an unstable one would show doctrine drifting toward one sibling reading as appointments shift.',
    'If the reading is unstable, this story''s claimed graduated structure is itself a snapshot of a kernel in motion rather than a settled interpretation — the beneficiary/victim sets and ε value authored here would need re-derivation at each stable point, per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_selection, conceptual, 'Whether the subsidiarity_balance reading is a stable equilibrium or a transitional state between the two sibling readings.').

omega_variable(
    sending_region_standing_gap,
    'Is the exclusion of sending-region brain-drain costs from the proportionality balancing test a structural feature of this reading specifically, or would any reading of the kernel (including the sibling readings) share the same gap?',
    'Compare whether integration_primary or sovereignty_primary readings incorporate sending-state regional cost into their respective legitimacy tests; if neither sibling does either, the gap is a kernel-level feature rather than specific to subsidiarity_balance.',
    'If the gap is kernel-wide, sending-region exclusion should be documented as a shared omega across all three reading-stories rather than treated as a distinguishing feature of this reading alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_region_standing_gap, empirical, 'Whether sending-region exclusion from the balancing test is specific to this reading or a kernel-wide structural gap.').

omega_variable(
    posted_worker_extraction_magnitude,
    'How much of the cost borne by posted low-wage workers is attributable to the proportionality doctrine''s specific balancing choices versus attributable to underlying wage differentials that would exist under any mobility regime?',
    'Comparative analysis of posted-worker outcomes in periods/jurisdictions with materially different proportionality case law, controlling for underlying wage differential.',
    'If most of the cost is attributable to underlying differentials rather than doctrine-specific choices, the extractiveness authored here may overstate this constraint''s specific contribution versus the baseline economic structure it operates within.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posted_worker_extraction_magnitude, empirical, 'Whether measured extraction reflects doctrine-specific choices or baseline wage-differential effects independent of the doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__subsidiarity_balance, theater_ratio, 8, 0.15).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__subsidiarity_balance, theater_ratio, 16, 0.19).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.22).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__subsidiarity_balance, theater_ratio, 32, 0.25).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__subsidiarity_balance, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 8, 0.29).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 32, 0.45).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__subsidiarity_balance, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the federation_membership_treaty kernel, decomposed per the ε-invariance principle: integration_primary, sovereignty_primary, and this subsidiarity_balance reading each instantiate a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, despite sharing a colloquial label ('free movement law'). This reading's ε (0.42) sits between what would be expected for integration_primary (lower, since restrictions are presumptively disfavored and extraction from mobile persons should be minimal) and sovereignty_primary (potentially higher for mobile persons, since state consent can more readily restrict movement). Do not average these three ε values into a single 'free movement' figure — that would violate ε-invariance by conflating three distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
