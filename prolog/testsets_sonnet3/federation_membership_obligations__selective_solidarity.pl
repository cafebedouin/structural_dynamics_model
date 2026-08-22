% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Contribution-Tiered Free Movement and Welfare Access (Selective Solidarity Reading)
 *   domain: political economy / federalism / migration policy / welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the 'selective solidarity' reading of the
 *   federation membership obligations kernel: free movement rights are
 *   preserved in form for all citizens of member states, but welfare access
 *   is bifurcated according to contribution/economic-activity status rather
 *   than citizenship or nationality per se. Employed mobile workers who pay
 *   into the host social insurance system receive parity with host nationals;
 *   economically inactive movers, long-term jobseekers, and precarious
 *   low-wage workers whose 'worker' status is contestable face
 *   habitual-residence tests, sufficient-resources requirements, and waiting
 *   periods that restrict their welfare access relative to citizens. This is
 *   a genuinely distinct constraint from the integration_primary reading
 *   (which would treat welfare parity as constitutive of free movement
 *   itself, unqualified by contribution) and the member_sovereignty_primary
 *   reading (which would treat welfare closure as a residual national
 *   sovereignty prerogative, qualified by mobility only where states choose
 *   to permit it). The contributory principle is the load-bearing
 *   distinction: it reallocates the site of exclusion from nationality to
 *   economic activity, producing a different victim class
 *   (inactive/precarious movers of ANY nationality, including host nationals
 *   not currently in work who fail similar tests) than either sibling reading
 *   would produce.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.58).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.52).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Contribution-Tiered Free Movement and Welfare Access (Selective Solidarity Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political economy / federalism / migration policy / welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '8fcae0a8-188c-4534-96d6-1cd36201f470').
narrative_ontology:cs_kernel_codification('8fcae0a8-188c-4534-96d6-1cd36201f470', formalized).
narrative_ontology:cs_authority_grounding('8fcae0a8-188c-4534-96d6-1cd36201f470', lineage).
narrative_ontology:cs_interpretation_layer_present('8fcae0a8-188c-4534-96d6-1cd36201f470').
narrative_ontology:cs_reading_relation('8fcae0a8-188c-4534-96d6-1cd36201f470', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('8fcae0a8-188c-4534-96d6-1cd36201f470', federation_membership_obligations__member_sovereignty_primary, influences).
narrative_ontology:cs_axiom('8fcae0a8-188c-4534-96d6-1cd36201f470', foundational, contribution_not_citizenship_grounds_entitlement).
narrative_ontology:cs_axiom_status(contribution_not_citizenship_grounds_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('8fcae0a8-188c-4534-96d6-1cd36201f470', contribution_not_citizenship_grounds_entitlement, conventional).
narrative_ontology:cs_axiom('8fcae0a8-188c-4534-96d6-1cd36201f470', secondary, economic_activity_status_is_legitimate_differentiator).
narrative_ontology:cs_axiom_status(economic_activity_status_is_legitimate_differentiator, holdable).
narrative_ontology:cs_axiom_grounding('8fcae0a8-188c-4534-96d6-1cd36201f470', economic_activity_status_is_legitimate_differentiator, instrumental).
narrative_ontology:cs_reference_frame('8fcae0a8-188c-4534-96d6-1cd36201f470', post_maastricht_market_citizenship_settlement).
narrative_ontology:cs_drift_state('8fcae0a8-188c-4534-96d6-1cd36201f470', post_2014_welfare_tourism_debate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8fcae0a8-188c-4534-96d6-1cd36201f470', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, employed_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_state_treasuries).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, sending_state_governments).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, long_term_jobseekers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, low_wage_precarious_mobile_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, low_wage_precarious_mobile_workers).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, contributory_reciprocity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move across member states to take up employment and, by virtue of paying into the host state's social insurance and tax system, receive full welfare parity with host nationals — unemployment benefits, family benefits, healthcare access — on the same terms as citizens. Their standing is earned through contribution, not granted through free movement itself, so it is durable as long as employment or recent employment history persists.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, employed_mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Relocated under free movement law but are not in work — students without means, early retirees, those unable to find employment quickly, or those caring for family. They face habitual-residence tests, means tests, and waiting periods before welfare access, and can be required to demonstrate 'sufficient resources' or face removal. Free movement grants them the right to be present but not the right to draw on the host welfare state on citizen terms.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens, payer,
    powerless, biographical, constrained, continental).

% Moved to seek work and initially qualified for some jobseeker support, but as job search extends past the permitted window, they lose entitlement to means-tested benefits and can face expulsion proceedings. Having relocated and built a life abroad, returning home is costly and often not a real option, but continued residence without work or benefits is precarious.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, long_term_jobseekers, payer,
    powerless, biographical, trapped, continental).

% Work intermittently or in marginal, part-time, or gig arrangements that put their 'worker status' in continual question. Whether they count as contributing workers (full access) or economically inactive (restricted access) is adjudicated case by case, often by host administrations with incentive to reclassify them downward. They bear the administrative burden of proving their status repeatedly.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, low_wage_precarious_mobile_workers, payer,
    powerless, immediate, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, low_wage_precarious_mobile_workers, beneficiary).

% Administer the contribution-status tests, set habitual-residence and sufficient-resources thresholds, and thereby control fiscal exposure to inbound mobile populations. They gain the benefits of mobile labor (tax and social contributions from employed movers) while capping the liability side (welfare payments to inactive movers), and defend the tiering as fiscally necessary to sustain free movement's political legitimacy.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_treasuries, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, host_state_treasuries, agenda_setter).

% Benefit from emigration of surplus or unemployed labor and remittance flows, while host states absorb integration costs for economically active emigrants. When emigrants become economically inactive abroad and are pushed back through habitual-residence tests, the sending state may absorb the returning caseload, but this cost is diffuse and delayed relative to the immediate fiscal relief of outward mobility.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, sending_state_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Adjudicates disputes over whether contribution-status tiering is consistent with free movement law, drawing lines between permissible fiscal safeguarding and impermissible discrimination on grounds of nationality. Its case law incrementally defines how tight or loose the contributory principle can be drawn.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federal_court, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, host_state_treasuries).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a federation-wide free movement zone to persist politically by reassuring host electorates that welfare access is earned through contribution rather than automatically granted by cross-border presence — solving the genuine collective-action problem of sustaining open borders against welfare-magnet anxieties that would otherwise generate closure pressure.
% TRANSFER_FUNCTION: Moves welfare entitlement away from a citizenship-equality baseline toward a work-contribution baseline: fiscal risk is transferred from host-state treasuries onto economically inactive and marginally-employed movers, who bear the cost of proving eligibility or being excluded, while the free movement right itself is preserved in form for all citizens.
% ABSENT_VOICES: Precarious and inactive movers rarely have organized representation in the negotiations that set contribution thresholds — those thresholds are set in bargaining between member state governments and federal institutions, with individual movers represented only through the mediated, ex-post channel of litigation years after the rules take effect.
% DISAPPEARANCE_RATIONALE: If contribution-tiering disappeared and welfare access reverted strictly to residence/citizenship-equality terms, host state treasuries would face materially higher and less predictable fiscal exposure to inbound populations, which historically has generated exactly the political backlash (welfare-magnet narratives, calls to restrict free movement itself) that tiering was designed to defuse — removing it would likely destabilize the broader free movement settlement, not merely reallocate money.
% FOUNDING_PROBLEM: Free movement without any welfare qualification risked generating a political backlash — the perception, whether accurate or not, that mobile citizens could relocate purely to draw on more generous welfare systems ('welfare tourism') — which threatened public support for free movement as a whole.
% FOUNDING_PROBLEM_CORROBORATION: Host state governments and treasuries attest the welfare-magnet risk remains live, citing fiscal exposure studies. Independent labor economists and migration researchers outside the beneficiary governments have repeatedly found actual welfare-tourism effects to be small relative to the political salience of the claim, suggesting the founding problem is substantially overstated relative to the administrative burden the tiering now imposes on economically inactive movers.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is moderate-high and rising: contribution thresholds have tightened over the interval as fiscal pressure and political salience of migration have increased, shifting more of the burden onto inactive and precarious movers. Suppression (0.52) reflects the real coercive apparatus behind the tiering — habitual-residence tests, removal proceedings for those who exhaust jobseeker windows, administrative reclassification of marginal workers — but it is not total: judicial review exists, and case law has periodically constrained how aggressively states can apply the tests. Theater ratio (0.40) is meaningfully non-trivial: a portion of the administrative apparatus (repeated status reverification, documentation demands) serves signaling/deterrence functions beyond what accurate contribution-tracking requires, and this share has grown as the tiering system has been used more as a political reassurance mechanism than a narrowly fiscal one.
 *
 * PERSPECTIVAL GAP:
 *   From the host-treasury agenda-setting seat, contribution-tiering is a rope: it is the mechanism that makes free movement politically and fiscally sustainable, and treasury actors experience it as legitimate cost-containment. From the economically-inactive or precarious-worker payer seats, the same rule set experiences as enforced exclusion — a right (free movement) rendered largely nominal by welfare-access barriers that require ongoing, burdensome proof of contribution. The engine's per-seat computation should reveal exactly this divergence: agenda-setter seats reading closer to coordination, powerless/constrained payer seats reading closer to extraction, without either seat's reading being definitionally wrong.
 *
 * DIRECTIONALITY LOGIC:
 *   Employed mobile workers and the two treasuries (host and sending) sit toward the beneficiary end: employed movers receive the welfare parity the coordination promises, hosts limit fiscal exposure, senders externalize labor-market slack. Economically inactive citizens, long-term jobseekers, and precarious workers sit toward the target end: the same tiering structure that protects the coordination function for employed movers extracts security from those whose economic-activity status is marginal or absent. The powerless/trapped or constrained exit options for these groups (having relocated, unwinding that move is costly) push their derived directionality further toward full target than a purely formal reading of 'they retain the right to return home' would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (welfare-magnet backlash risk) is contested as still live: host treasuries assert it persists, while independent research suggests actual fiscal impact from inactive mobile citizens is smaller than the political narrative implies. This creates a mandatrophy-adjacent structure: the administrative machinery of contribution-testing may have outgrown the scale of the problem it was built to manage, continuing to expand (rising extraction and theater ratio) even as the empirical justification weakens. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (sustaining free movement's political viability) that would be lost by a pure-extraction reading, while the enforcement requirement and victim declarations preserve the asymmetric cost that a pure-rope reading would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contributory_principle_neutral_or_disguised_nationality_proxy,
    'Does the contributory/economic-activity test operate as a genuinely nationality-neutral sorting mechanism, or does it function in practice as a disguised proxy for excluding nationals of poorer or higher-emigration member states, who are statistically more likely to arrive without immediate employment?',
    'Disaggregate habitual-residence and sufficient-resources test outcomes by sending-state per-capita income and compare rejection/restriction rates; if rejection correlates strongly with sending-state wealth after controlling for actual economic activity, the neutral contributory framing is empirically undermined.',
    'If the test functions as a nationality proxy, this reading collapses toward member_sovereignty_primary in substance despite its contributory framing, and the tangled_rope classification would need re-examination for whether victimhood is better modeled along nationality lines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributory_principle_neutral_or_disguised_nationality_proxy, empirical, 'Whether contribution-status tiering is nationality-neutral in practice or a disguised nationality filter.').

omega_variable(
    welfare_magnet_problem_liveness,
    'Is the founding welfare-magnet problem still empirically live at a scale that justifies the current administrative burden, or has the apparatus outgrown a problem that was always more politically salient than fiscally real?',
    'Compare independent migration-economics estimates of welfare-driven relocation against the fiscal and administrative cost of maintaining the contribution-testing apparatus over the same period.',
    'If the problem was always smaller than the response, the rising extraction and theater_ratio trend documents mandatrophy — enforcement infrastructure persisting and hardening past its justification — which would strengthen a case for reclassifying toward piton or snare in a future revision of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_magnet_problem_liveness, empirical, 'Whether the administrative apparatus is proportionate to the actual (versus perceived) welfare-magnet risk.').

omega_variable(
    worker_status_classification_discretion,
    'How much discretion do host administrations retain in classifying marginal/gig workers as ''economically active'' versus ''inactive,'' and does that discretion get exercised asymmetrically to minimize fiscal exposure regardless of the worker''s actual contribution history?',
    'Audit administrative reclassification decisions for precarious workers against objective contribution records (tax/social-insurance payment history) to see whether classification outcomes track the record or track fiscal-exposure minimization independent of it.',
    'High discretion exercised to minimize exposure would indicate the contributory principle is applied asymmetrically as cover for extraction from a class the rule nominally protects, sharpening the case for the low_wage_precarious_mobile_workers victim declaration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(worker_status_classification_discretion, empirical, 'Whether worker-status classification discretion is exercised neutrally or as a fiscal-exposure-minimizing lever.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.22).
narrative_ontology:measurement(fede_tr_t6, federation_membership_obligations__selective_solidarity, theater_ratio, 6, 0.27).
narrative_ontology:measurement(fede_tr_t12, federation_membership_obligations__selective_solidarity, theater_ratio, 12, 0.31).
narrative_ontology:measurement(fede_tr_t18, federation_membership_obligations__selective_solidarity, theater_ratio, 18, 0.35).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__selective_solidarity, theater_ratio, 24, 0.38).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__selective_solidarity, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t6, federation_membership_obligations__selective_solidarity, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(fede_be_t12, federation_membership_obligations__selective_solidarity, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(fede_be_t18, federation_membership_obligations__selective_solidarity, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__selective_solidarity, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__selective_solidarity, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fede_su_t6, federation_membership_obligations__selective_solidarity, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(fede_su_t12, federation_membership_obligations__selective_solidarity, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(fede_su_t18, federation_membership_obligations__selective_solidarity, suppression_requirement, 18, 0.47).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__selective_solidarity, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__selective_solidarity, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__selective_solidarity, 0.12).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% Three constraints share the federation_membership_obligations kernel: integration_primary (welfare parity as constitutive of free movement), member_sovereignty_primary (welfare closure as retained national prerogative), and this story, selective_solidarity (welfare access gated by contribution/economic-activity status). Each reading has a distinct ε, victim set, and classification: integration_primary is expected to read closer to rope/mountain-of-law (mobility rights largely uncontested, low extraction); member_sovereignty_primary is expected to read closer to snare or tangled_rope from the mobile-worker seat (explicit sovereignty-based exclusion); this reading occupies an intermediate tangled_rope position because it preserves formal free movement while gating substantive welfare access on a contribution axis that cuts across nationality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
