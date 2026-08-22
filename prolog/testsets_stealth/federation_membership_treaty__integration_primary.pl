% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive of the Single Market (Integration-Primary Reading)
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   This story instantiates the integration-primary reading of the
 *   federation_membership_treaty kernel: free movement of workers is
 *   constitutive of the single market, and national restrictions are
 *   presumptively illegitimate unless narrowly justified. The standing
 *   arrangement under contest — and therefore the referent of epsilon — is
 *   the existing free-movement regime as this reading assesses it, NOT the
 *   sovereignty-primary alternative this reading rejects; per the
 *   epsilon-referent rule, the endorsed alternative never supplies the
 *   referent. The regime coordinates genuinely (a continental labor market
 *   cannot allocate itself across closed national segments) while extracting
 *   asymmetrically (destination-region low-wage workers and welfare budgets
 *   absorb adjustment costs they did not consent to, and national restriction
 *   authority is displaced upward). The claim/metric gap is deliberate: the
 *   regime is CLAIMED as tangled_rope from this reading's own structural
 *   assessment, and the metrics are authored independently as descriptively
 *   true — the engine computes per-seat types from the structural data. Time
 *   points 0-24 map to 2004-2024, the post-enlargement era during which
 *   intra-union flows, doctrinal consolidation, and political backlash all
 *   matured. KEY AGENTS (by structural relationship): - mobile_eu_workers:
 *   Primary beneficiary (moderate/mobile) — captures wage differentials, exit
 *   always available - labor_exporting_member_states: Secondary beneficiary
 *   (institutional/constrained) — remittances and unemployment relief -
 *   cross_border_employers: Concentrated beneficiary (powerful/arbitrage) —
 *   flexible staffing, wage moderation, residual claimant of the mobility
 *   surplus - destination_low_wage_workers: Primary target
 *   (powerless/trapped) — bears localized wage and housing pressure -
 *   destination_welfare_administrators: Secondary target
 *   (institutional/constrained) — absorbs uncapped fiscal exposure -
 *   restriction_minded_member_governments: Dual-positioned payer/beneficiary
 *   (institutional/constrained) — loses restriction authority, gains from
 *   outward mobility - european_commission and cjeu: Agenda setters
 *   (institutional/identity_locked) — enforce and elaborate the constitutive
 *   doctrine - restriction_minded_national_electorates: Excluded voice
 *   (organized/constrained) — objects but reaches the table only through
 *   losing litigation - comparative_labor_economists: Analytical observer —
 *   sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.62).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.72).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive of the Single Market (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '2d4d6dd6-26db-4da6-b651-0642907021ea').
narrative_ontology:cs_kernel_codification('2d4d6dd6-26db-4da6-b651-0642907021ea', fixed_text).
narrative_ontology:cs_authority_grounding('2d4d6dd6-26db-4da6-b651-0642907021ea', lineage).
narrative_ontology:cs_interpretation_layer_present('2d4d6dd6-26db-4da6-b651-0642907021ea').
narrative_ontology:cs_reading_relation('2d4d6dd6-26db-4da6-b651-0642907021ea', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('2d4d6dd6-26db-4da6-b651-0642907021ea', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('2d4d6dd6-26db-4da6-b651-0642907021ea', foundational, free_movement_constitutive_of_single_market).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_single_market, holdable).
narrative_ontology:cs_axiom_grounding('2d4d6dd6-26db-4da6-b651-0642907021ea', free_movement_constitutive_of_single_market, instrumental).
narrative_ontology:cs_axiom('2d4d6dd6-26db-4da6-b651-0642907021ea', foundational, restrictions_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(restrictions_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('2d4d6dd6-26db-4da6-b651-0642907021ea', restrictions_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('2d4d6dd6-26db-4da6-b651-0642907021ea', ever_closer_union_constitutive_mobility).
narrative_ontology:cs_drift_state('2d4d6dd6-26db-4da6-b651-0642907021ea', post_enlargement_backlash_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2d4d6dd6-26db-4da6-b651-0642907021ea', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, labor_exporting_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, cross_border_employers).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, destination_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, destination_welfare_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, restriction_minded_member_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, restriction_minded_member_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise treaty rights to take up employment in any member state, capturing wage differentials between origin and destination economies. Rely on court-backed non-discrimination rules and mutual recognition of qualifications. Exit is structurally easy: return home or move to a third member state remains available at all times.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Send workers toward higher-wage member regions, receiving remittance inflows and relief from domestic unemployment while retaining the workers' citizenship and pension claims. Cannot restrict their own nationals' departure and remain bound by the reciprocal opening of their own labor markets; treaty membership makes unilateral suspension unavailable short of withdrawal.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, labor_exporting_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Recruit across the entire union, staff flexibly across national boundaries, and moderate wage growth in tight labor segments. Can relocate operations toward cheaper labor pools inside the single market. Organize the most durable lobbying support for preserving unrestricted mobility rights.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cross_border_employers, beneficiary,
    powerful, generational, arbitrage, continental).

% Compete in housing markets and low-skill job segments with incoming workers; measured wage pressure concentrates in exactly the occupations they hold. Dispersed across regions and often cross-pressured (some are themselves recent arrivals or have migrant family ties), so coalition formation is weak. Exit means internal migration on thin financial margins.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, destination_low_wage_workers, payer,
    powerless, immediate, trapped, regional).

% Administer social insurance systems that coordination regulations keep open to mobile workers. Prior-residence exclusions and discriminatory benefit tests are struck down as inconsistent with free movement, so fiscal exposure and caseload are absorbed rather than capped. Benefit design may be adjusted only within non-discrimination limits policed from above.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, destination_welfare_administrators, payer,
    institutional, generational, constrained, national).

% Elected on platforms promising caps on inflows or restricted benefit access, then find their draft measures deterred or struck down under the presumption that restrictions are illegitimate. Simultaneously benefit as their own nationals work abroad and as remittances return. One government has tested full exit; the demonstrated cost deters repetition by others.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, restriction_minded_member_governments, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, restriction_minded_member_governments, beneficiary).

% Opens infringement proceedings against restrictive member states and issues guidance narrowing the justifications states may offer. Its institutional identity is fused with guardianship of the single market acquis; retreating from the constitutive framing of mobility would disavow the body of enforcement it has built its legitimacy on.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, european_commission, agenda_setter,
    institutional, generational, identity_locked, continental).

% Adjudicates national restrictions under proportionality review, allocating the burden of justification to the restricting state. Decades of doctrine define who counts as a worker, what counts as indirect discrimination, and how narrow a justification must be. Reversing the constitutive premise would require overturning its own constitutional case law.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cjeu, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Pluralities or majorities in several member states consistently poll for tighter controls on intra-union movement. Their preferences reach the supranational framework only filtered through governments that litigate and usually lose; the one direct popular instrument tried, a withdrawal referendum, proved available but at catastrophic cost, deterring repetition.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, restriction_minded_national_electorates, excluded,
    organized, biographical, constrained, national).

% Estimate wage effects, fiscal incidence, and displacement from intra-union migration. The literature finds small aggregate wage effects with localized low-skill pressure and contested net fiscal balances for welfare systems. Their estimates are cited by every other seat in the contest.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, comparative_labor_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, cross_border_employers).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of a fragmented continental labor market: allocates labor across dozens of national economies, removes border frictions and credential barriers, matches skills to demand where they arise, and forecloses beggar-thy-neighbor labor-market closures among members.
% TRANSFER_FUNCTION: Moves labor, and the consumption, tax contributions, and welfare claims attached to it, from lower-wage to higher-wage regions; moves the adjustment costs of that reallocation onto destination-region low-wage workers and destination welfare budgets; moves regulatory authority over labor-market access from national capitals to supranational institutions.
% ABSENT_VOICES: Restriction-minded national electorates would object that their democratically expressed preference for tighter controls is treated as presumptively illegitimate before argument begins. They are present in national politics but absent from the supranational adjudication table, where they appear only through governments that litigate under a burden of justification stacked against them.
% DISAPPEARANCE_RATIONALE: If the constitutive free-movement regime vanished overnight, labor allocation across the single market would reorganize within months: bilateral labor agreements would proliferate, wage and welfare-load distributions would shift sharply at former destination regions, employers would re-price relocation and recruiting, and the four-freedoms architecture of the single market would lose one of its pillars.
% FOUNDING_PROBLEM: Post-war European economic fragmentation: the interwar collapse into protectionism, currency blocs, and closed labor markets was understood by the founders as a precondition of repeated war. Free movement of workers was written into the common market to make borders economically irreversible and to complete a common market that could not function with immobile factors.
% FOUNDING_PROBLEM_CORROBORATION: No neutral arbiter attests the founding problem remains live in its original form. Integration historians outside the beneficiary set (notably the Milward school) document that the arrangement was built to serve national economic rescue as much as federal ideal, cutting against a purely constitutive genealogy. Member-state behavior corroborates the contest: decades of restriction attempts and one completed exit are revealed-preference testimony that the parties themselves dispute whether the founding problem still binds.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.62 at interval end) because the surplus from labor reallocation accrues unevenly: employers capture staffing flexibility and wage moderation, mobile workers capture wage differentials, while the adjustment costs concentrate on low-wage destination workers and welfare budgets with no compensating mechanism. Suppression is high (0.72) and is a RAW structural property, unscaled by power or scope — only extractiveness is scaled by the engine. The suppression is predominantly structural (roughly 70%): treaty supremacy, Commission infringement power, and burden-of-proof allocation make restriction attempts fail before they operate. A minority component (roughly 30%) is internalized: national officials draft restriction proposals knowing the presumption, so the option space self-narrows — chilling that persists even where formal enforcement is not invoked. Theater is moderate-low (0.32): proportionality review performs real legal work, but a growing share of the justification machinery functions as legitimation ritual, with member states mounting defenses they expect to lose. Accessibility collapse is moderate (0.55): alternatives do not vanish — safeguard clauses, transitional controls, and full withdrawal exist — but each is narrow or catastrophically priced, so the understood option space contracts substantially. Resistance is high (0.68): episodic surges (financial crisis, 2015-16, the withdrawal referendum) recur, yet the series stays monotonic rather than cyclical because enforcement capacity hardened faster than each resistance wave could erode it; the oscillation is in political salience, not in the constraint's operative force. All three metric series are authored on one shared seven-point grid so the engine samples a fully populated row at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the beneficiary seats (mobile workers, exporting states, employers), the arrangement presents as coordination: a rights regime that expanded everyone's option space. From the payer seats (destination low-wage workers, welfare administrators, restriction-seeking governments), the same structure presents as enforced extraction: costs localized on them, authority displaced away from them, exit priced prohibitively. From the agenda-setter seats, it presents as constitution-building — the Commission and Court experience the constraint as their own institutional identity, which is why both carry identity_locked exit: the organization has become its function, and abandoning the constitutive framing would dissolve the legitimacy they have accumulated across decades of doctrine. The engine computes this divergence from power, exit, and directional data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers sit nearest the beneficiary pole (d near 0.0): the constraint subsidizes their option space and their exit is arbitrage-grade within the system. Cross-border employers likewise sit near the beneficiary pole with the strongest capture of the surplus. Labor-exporting states sit low but not at zero: they gain remittances and unemployment relief while accepting reciprocal exposure. Destination low-wage workers sit nearest the target pole (d near 1.0): they bear concentrated costs with trapped exit. Welfare administrators sit high as well: uncapped exposure with constrained adjustment. Restriction-minded governments are genuinely dual-positioned — the derivation from their payer role would push d high, but their secondary beneficiary position (outward mobility for their nationals) pulls it back toward symmetry; the structural data carries both roles so the engine weights the ambivalence rather than the story overriding it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the regime as pure snare would erase the real coordination function — a continental labor market genuinely cannot allocate itself across closed national segments, and mobile workers are net beneficiaries, not cover-story props. Reading it as pure rope would launder the asymmetric extraction: the same structure that coordinates also localizes costs on seats that did not consent and displaces national democratic authority. The R5 interview locates the tension precisely: the founding problem (anti-fragmentation entanglement) is CONTESTED while disappearance would rearrange the world — the arrangement is load-bearing but no longer unanimously about its founding purpose. Because status is contested rather than dead, the status-by-verdict mismatch consumer should not fire a zombie flag; the honest state is a live hybrid whose coordination and extraction components the omegas below keep separable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the federation_membership_treaty kernel. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative compilation of the three readings'' beneficiary/victim sets and default rules: sovereignty_primary protects national restriction capacity (destination labor markets leave the victim set, national authorities gain a defended good); subsidiarity_balance replaces the presumption of illegitimacy with case-by-case proportionality. The disagreement is located in the DEFAULT RULE governing national restrictions: presumptively illegitimate versus presumptively lawful versus balanced.',
    'If sovereignty_primary were adopted, this story''s suppression measure collapses toward low (restrictions face no structural bar), the victim set shrinks, and the computed type at payer seats moves toward rope; if subsidiarity_balance were adopted, extraction falls moderately while coordination is preserved. The epsilon authored here is valid only for the integration-primary instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: which kernel reading this is and what siblings would change').

omega_variable(
    constitutive_status_empirical_basis,
    'Is labor mobility genuinely constitutive of a functioning single market, or is the constitutive framing a constructed treaty choice that identifiable agents benefit from treating as necessary?',
    'Comparative analysis of deep economic-integration arrangements lacking labor-mobility guarantees (goods-and-capital-only frameworks): if such arrangements sustain market integration without the extraction profile observed here, the constitutive claim is contingent rather than structural.',
    'If mobility is separable from market integration, the constitutive framing functions as ideological cover, effective extraction rises above the authored base, and the classification pushes toward snare at the payer seats; if inseparable, part of the measured extraction is the irreducible price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_status_empirical_basis, empirical, 'Whether the constitutive premise is structural necessity or constructed choice').

omega_variable(
    welfare_cost_attribution,
    'How much of the destination welfare systems'' fiscal exposure is attributable to free movement as such, versus demographic aging and domestic benefit-design choices?',
    'Quasi-experimental fiscal-incidence studies exploiting staggered inflow shocks across destination regions, separating compositional effects of mobility from baseline entitlement growth.',
    'If most exposure is attributable to the constraint, the welfare-administrator seat''s target-directionality is confirmed and epsilon stands; if mostly coincidental, that seat''s extraction share falls and the victim set effectively narrows to destination low-wage workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_cost_attribution, empirical, 'Attribution of welfare-system costs to the constraint versus confounders').

omega_variable(
    sending_region_depletion,
    'Does the regime also extract from sending regions through selective emigration of younger workers, making labor-exporting regions hidden victims beyond the declared beneficiary set?',
    'Demographic and fiscal tracking of sending regions: dependency-ratio deterioration, care-sector shortages, and human-capital stock changes attributable to outflow selection.',
    'If depletion is substantial, the victim set expands, the exporting-state seat''s directionality rises from beneficiary toward mixed, and total epsilon increases — pushing some seats'' computed types toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sending_region_depletion, empirical, 'Hidden extraction from sending regions via selective emigration').

omega_variable(
    suppression_durability,
    'Is the high suppression of national restrictions durable, or does accumulated resistance (completed exit, opt-outs, escalating infringement defiance) erode enforcement capacity over the coming interval?',
    'Track enforcement outcomes: infringement success rates, member-state compliance latency, proliferation of de facto restrictions tolerated without action, and the deterrent effect of the demonstrated withdrawal path on further exit attempts.',
    'If enforcement capacity decays, suppression_requirement trends downward, restriction attempts succeed more often, and the computed type at payer seats migrates from tangled_rope toward rope; if enforcement hardens further, the regime drifts toward snare characteristics at the target seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_durability, empirical, 'Durability of the enforcement machinery suppressing national restrictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmt_integ_primary_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fmt_integ_primary_tr_t4, federation_membership_treaty__integration_primary, theater_ratio, 4, 0.2).
narrative_ontology:measurement(fmt_integ_primary_tr_t8, federation_membership_treaty__integration_primary, theater_ratio, 8, 0.23).
narrative_ontology:measurement(fmt_integ_primary_tr_t12, federation_membership_treaty__integration_primary, theater_ratio, 12, 0.26).
narrative_ontology:measurement(fmt_integ_primary_tr_t16, federation_membership_treaty__integration_primary, theater_ratio, 16, 0.28).
narrative_ontology:measurement(fmt_integ_primary_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.3).
narrative_ontology:measurement(fmt_integ_primary_tr_t24, federation_membership_treaty__integration_primary, theater_ratio, 24, 0.32).

% Extraction over time
narrative_ontology:measurement(fmt_integ_primary_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fmt_integ_primary_be_t4, federation_membership_treaty__integration_primary, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(fmt_integ_primary_be_t8, federation_membership_treaty__integration_primary, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(fmt_integ_primary_be_t12, federation_membership_treaty__integration_primary, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(fmt_integ_primary_be_t16, federation_membership_treaty__integration_primary, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(fmt_integ_primary_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(fmt_integ_primary_be_t24, federation_membership_treaty__integration_primary, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fmt_integ_primary_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fmt_integ_primary_su_t4, federation_membership_treaty__integration_primary, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(fmt_integ_primary_su_t8, federation_membership_treaty__integration_primary, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(fmt_integ_primary_su_t12, federation_membership_treaty__integration_primary, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(fmt_integ_primary_su_t16, federation_membership_treaty__integration_primary, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(fmt_integ_primary_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(fmt_integ_primary_su_t24, federation_membership_treaty__integration_primary, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the federation_membership_treaty kernel. The colloquial label 'EU free movement' covers three structurally distinct constraints corresponding to the three readings: integration_primary (this file — restrictions presumptively illegitimate; mobile workers benefit, destination labor markets and welfare systems pay, national restrictions actively suppressed), sovereignty_primary (states retain protective authority; victim set inverts), and subsidiarity_balance (proportionality mediates; intermediate extraction). Each story carries its own stable epsilon over the same standing arrangement; they are linked here because the integration-primary reading is the constitutional baseline the other two negotiate against — its doctrinal wins shift the legitimacy conditions and resource availability the siblings operate under. The upstream reading's higher enforcement intensity typically feeds the downstream readings' contest dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
