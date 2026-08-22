% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Occupational Licensing Statute Mandate (Rent-Seeking Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   Statutory credential requirements — occupational licensing statutes
 *   administered by profession-staffed boards — govern entry into roughly a
 *   quarter of the workforce. This story instantiates the
 *   rent_seeking_suppression reading of the licensing_statute_mandate kernel:
 *   the standing arrangement under contest is the licensing regime itself,
 *   and epsilon is authored for that regime as this reading sees it (an
 *   artificial-scarcity machine whose safety justification functions as
 *   cover), never for the safety-justified arrangement the
 *   public_safety_coordination sibling would describe. Wage-premium studies,
 *   entry-delay data, and price effects are the observable record; the
 *   reading's claim is about what the statutes are FOR. Family links to both
 *   sibling readings run through network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - incumbent_license_holders: Primary
 *   beneficiary (organized/identity_locked) — collects wage premiums; defends
 *   the barrier - licensing_board_members: Agenda setter with beneficiary
 *   secondary position (institutional/arbitrage) — administers and enforces;
 *   drawn from the regulated profession - aspiring_practitioners: Primary
 *   target (powerless/trapped) — bears entry costs and denial -
 *   consumers_of_licensed_services: Secondary target (moderate/constrained) —
 *   pays above-competitive prices - accredited_training_providers: Ancillary
 *   beneficiary (organized/mobile) — sells statutorily mandated inputs -
 *   informal_market_practitioners: Excluded competitor (powerless/trapped) —
 *   criminalized substitute supply - state_legislatures: Enacting agenda
 *   setter (institutional/arbitrage) — holds the pen, lobbied by
 *   beneficiaries - competition_authorities: Analytical observer
 *   (institutional/analytical) — litigates restraint but cannot repeal
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.74).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.8).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.74).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Occupational Licensing Statute Mandate (Rent-Seeking Reading)").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "economic/political").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, 'b80348cd-1e15-49e2-b44b-4defa19cdbd1').
narrative_ontology:cs_kernel_codification('b80348cd-1e15-49e2-b44b-4defa19cdbd1', formalized).
narrative_ontology:cs_authority_grounding('b80348cd-1e15-49e2-b44b-4defa19cdbd1', extraction).
narrative_ontology:cs_interpretation_layer_present('b80348cd-1e15-49e2-b44b-4defa19cdbd1').
narrative_ontology:cs_reading_relation('b80348cd-1e15-49e2-b44b-4defa19cdbd1', licensing_statute_mandate__public_safety_coordination, influences).
narrative_ontology:cs_reading_relation('b80348cd-1e15-49e2-b44b-4defa19cdbd1', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('b80348cd-1e15-49e2-b44b-4defa19cdbd1', foundational, supply_restriction_is_primary_function).
narrative_ontology:cs_axiom_status(supply_restriction_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('b80348cd-1e15-49e2-b44b-4defa19cdbd1', supply_restriction_is_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('b80348cd-1e15-49e2-b44b-4defa19cdbd1', secondary, harm_prevention_justification_is_secondary_cover).
narrative_ontology:cs_axiom_status(harm_prevention_justification_is_secondary_cover, holdable).
narrative_ontology:cs_axiom_grounding('b80348cd-1e15-49e2-b44b-4defa19cdbd1', harm_prevention_justification_is_secondary_cover, empirically_contingent).
narrative_ontology:cs_reference_frame('b80348cd-1e15-49e2-b44b-4defa19cdbd1', competitive_labor_market_baseline).
narrative_ontology:cs_drift_state('b80348cd-1e15-49e2-b44b-4defa19cdbd1', contemporary_full_coverage_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b80348cd-1e15-49e2-b44b-4defa19cdbd1', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_license_holders).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, accredited_training_providers).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, licensing_board_members).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, aspiring_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, informal_market_practitioners).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, supply_restriction_wage_premium).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, board_discretion_expansion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold licenses acquired through years of mandated training, examinations, and fees; earn documented wage premiums over comparable unlicensed workers; fund and staff professional associations that lobby boards and legislatures. Their credential's market value depends on the entry barrier staying up, and their professional identity is built on the licensed status — leaving the occupation would strand both the investment and the self-concept.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_license_holders, beneficiary,
    organized, biographical, identity_locked, national).

% Appointed predominantly from the licensed profession itself; write implementing rules, set fees, define scope of practice, and prosecute unlicensed competitors. Board service confers professional standing and post-tenure returns; members routinely rotate back into private practice within the arrangements they administered.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, licensing_board_members, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, licensing_board_members, beneficiary).

% Sell mandated pre-license education hours, exam preparation, and continuing-education credits to a customer base created by statute rather than by demand. Enrollment volume tracks legislative expansions of hour requirements; exit means reorienting toward voluntary certification markets with thinner margins.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, accredited_training_providers, beneficiary,
    organized, generational, mobile, national).

% Face tuition for mandated classroom hours, examination and licensing fees, waiting periods, and arbitrary denial, before earning anything in the occupation. There is no lawful alternative path to practice; the realistic fallbacks are adjacent lower-paid unlicensed work or relocation to jurisdictions that may not recognize their training.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, aspiring_practitioners, payer,
    powerless, immediate, trapped, national).

% Pay above-competitive prices embedded in licensed services and bear reduced access — fewer practitioners, longer waits, thinner rural coverage. Substitutes are limited where licensing covers an entire service category, and quality differences attributable to licensure are difficult for an individual buyer to observe.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services, payer,
    moderate, immediate, constrained, national).

% Provide services without licenses at lower prices, operating under threat of fines, cease-and-desist orders, and prosecution. They are barred from the lawful market the statute defines and would compete openly if the barrier fell; many are immigrants or low-capital workers for whom the licensed path is unreachable.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, informal_market_practitioners, excluded,
    powerless, immediate, trapped, local).

% Enact and amend licensing statutes and delegate rule-writing to boards staffed by licensees. Professional associations supply campaign contributions, electoral endorsements, and ready-made bill text; existing titles are rarely revisited unless litigation, federal pressure, or interstate competition forces review.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, state_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Litigate against board conduct that restrains trade under the active-supervision doctrine, publish competition-impact assessments of proposed licensing bills, and testify in favor of sunrise clauses. They lack standing to repeal statutes directly and depend on legislatures and courts to act on their findings.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_license_holders).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes and signals minimum competence in services where consumers cannot cheaply evaluate quality before purchase, and maintains a searchable public registry of authorized practitioners.
% TRANSFER_FUNCTION: Moves income from consumers (through above-competitive prices) and from aspiring entrants (through mandated tuition, fees, and foregone earnings during training and waiting periods) to incumbent license holders, training institutions, and board operations funded by licensee fees.
% ABSENT_VOICES: Aspiring practitioners denied entry, low-income consumers priced out of services, and informal practitioners facing prosecution are absent from board proceedings; public comment windows are formally open but dominated by licensee associations, so the apparent consensus behind expansions reflects who is in the room rather than who is affected.
% DISAPPEARANCE_RATIONALE: Wage premiums would compress as entry opened, prices in licensed services would fall toward competitive levels, mandated-training programs would shrink to voluntary demand, and quality assurance would migrate to certification, bonding, insurance, and reputation systems; the legal status and investment values of millions of practitioners would rearrange overnight.
% FOUNDING_PROBLEM: Consumer vulnerability to incompetent and fraudulent practitioners in markets with severe information asymmetry — the historical targets included patent medicine, untrained surgeons, and fraudulent title and design practices.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor-economics research (wage-premium and quality-outcome studies) and antitrust-agency competition reports attest from outside the beneficiary set that demonstrated harm is negligible in most licensed occupations and that current scope far exceeds any documented harm threshold; emergency-care outcome data partially corroborate a live harm-prevention function in a narrow high-stakes tier. No source outside the benefiting parties attests that the full current scope of licensure tracks demonstrated consumer harm.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74) because documented wage premiums of roughly 10-15 percent in licensed occupations, multi-year entry delays, and above-competitive service prices are decoupled from any demonstrated marginal safety gain in most covered occupations. Suppression is higher still (0.80) because persistence depends on criminal prohibition of unlicensed practice, board subpoena and discipline powers, and active exclusion of substitute supply — not on participant preference. Theater ratio (0.41) reflects a growing share of regime activity — mandated classroom hours unrelated to harm profiles, continuing-education paperwork, ceremonial board process — detached from demonstrable safety function. Accessibility_collapse (0.60) is partial: lawful practice collapses without a license, but adjacent unlicensed occupations, interstate variation, and informal supply persist. Resistance (0.55) is real and sustained — constitutional litigation, deregulation campaigns, universal-recognition reforms — meeting entrenched incumbent defense. The three temporal series run on one shared grid (1950, 1965, 1980, 1995, 2010, 2025) so every metric is authored at every examined point; the suppression_requirement series deliberately traces enforcement-capacity buildup (boards, investigators, penalty escalation), which is the dynamic this story tracks. Suppression is authored as a raw structural property — the engine scales only extractiveness, by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute differently. From the entrant's position the statute is a wall with a toll: no lawful path exists except through the gauntlet, so the arrangement presents as enforced exclusion. From the incumbent's position the same statute is earned status and the guarantor of an investment made in good faith. From the board's position it is public protection administered by experts. The engine computes these divergent per-seat classifications from the structural data — divergent exit options (trapped entrants versus arbitrage-positioned board members) and divergent directionality drive the gap, not differing information.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: incumbent_license_holders collect the wage premium the barrier produces and are identity_locked into defending it (their credential's market value and professional self-concept depend on the barrier staying up); accredited_training_providers sell statutorily mandated inputs to a captive market; licensing_board_members administer the arrangement and rotate back into the practice it protects. Targets sit near the full-target end: aspiring_practitioners are trapped (no lawful alternative path to the occupation), informal_market_practitioners are excluded outright (maximal target position among payers), and consumers_of_licensed_services bear elevated prices with only partial substitution available (constrained, slightly moderated). state_legislatures are nominally neutral enactors but structurally beneficiary-adjacent through campaign-finance dependence on professional associations; the automatic derivation likely underweights this capture gradient, which is noted here rather than forced through an override keyed to a power atom shared with genuinely neutral actors.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification prevents mislabeling in both directions. Taking the safety justification at face value files licensing as pure coordination and misses the asymmetric extraction the wage-premium record documents; reading every licensing statute as pure extraction erases the genuine coordination residue in high-stakes tiers (physicians, electricians), which the occupational_risk_heterogeneity omega routes to per-tier decomposition rather than forcing into this aggregate story. On mandatrophy proper: the founding problem (quackery, fraudulent practitioners in information-opaque markets) was substantially solved for most covered occupations decades ago, yet the machinery expanded in scope and stringency throughout the interval — the contested founding_problem_status paired with a world_rearranges disappearance verdict preserves the mismatch signal for per-tier investigation without auto-flagging the aggregate as zombie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates the rent_seeking_suppression reading of the licensing_statute_mandate kernel — is that the correct instantiation, and what would the sibling readings change structurally?',
    'Cross-reading comparison: compile the public_safety_coordination and graduated_access_filter stories and compare computed types, victim sets, and epsilon over the same statutory referent.',
    'Under public_safety_coordination, epsilon drops sharply (genuine harm prevention) and classification moves toward rope/tangled_rope; under graduated_access_filter, the victim set persists but the mechanism relocates to class-stratified access. The disagreement is located in the teleological premise — what the statute is FOR — not in the observable record.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    unlicensed_harm_counterfactual,
    'Would consumer harm measurably rise if licensure were replaced with voluntary certification in mid-risk occupations?',
    'Natural experiments from partial deregulation and scope-of-practice expansions (dental hygienists, nurse practitioners, tree trimmers, interior designers) tracking injury and complaint rates against matched licensed jurisdictions.',
    'Flat harm curves raise the extraction share of measured epsilon and push classification toward snare; rising harm validates a coordination core and pulls toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unlicensed_harm_counterfactual, empirical, 'Whether the safety function is separable from the supply restriction.').

omega_variable(
    occupational_risk_heterogeneity,
    'Does the statutory apparatus constitute one constraint or a family spanning near-zero-risk occupations (florists, hair braiders) and high-risk ones (physicians, electricians) with very different epsilon?',
    'Decompose by occupation risk tier and author separate stories per tier; compare epsilon and victim structure across tiers.',
    'Low-risk tiers compute as near-pure extraction; high-risk tiers retain genuine coordination cores and may compute as tangled_rope — the aggregate story may misstate both, so per-tier decomposition is the safe resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(occupational_risk_heterogeneity, conceptual, 'Aggregation ambiguity across occupation risk tiers within one statutory form.').

omega_variable(
    reform_asymmetry_persistence,
    'Why does licensing reform stall despite bipartisan support and favorable economic evidence — concentrated incumbent mobilization versus diffuse consumer and entrant interests?',
    'Compare reform outcomes in states adopting universal recognition or sunset review against matched controls; track professional-association lobbying intensity around repeal bills.',
    'Confirms Olsonian asymmetry: fixing is costly not because removal is technically hard but because beneficiaries are concentrated and organized while payers are diffuse — sustaining snare classification rather than decay toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_asymmetry_persistence, preference, 'Political economy of persistence against known criticism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t1950, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(lice_tr_t1950, observed).
narrative_ontology:measurement(lice_tr_t1965, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1965, 0.24).
narrative_ontology:measurement_basis(lice_tr_t1965, observed).
narrative_ontology:measurement(lice_tr_t1980, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1980, 0.28).
narrative_ontology:measurement_basis(lice_tr_t1980, observed).
narrative_ontology:measurement(lice_tr_t1995, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1995, 0.32).
narrative_ontology:measurement_basis(lice_tr_t1995, observed).
narrative_ontology:measurement(lice_tr_t2010, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2010, 0.37).
narrative_ontology:measurement_basis(lice_tr_t2010, observed).
narrative_ontology:measurement(lice_tr_t2025, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(lice_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(lice_be_t1950, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement_basis(lice_be_t1950, observed).
narrative_ontology:measurement(lice_be_t1965, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1965, 0.54).
narrative_ontology:measurement_basis(lice_be_t1965, observed).
narrative_ontology:measurement(lice_be_t1980, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement_basis(lice_be_t1980, observed).
narrative_ontology:measurement(lice_be_t1995, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1995, 0.66).
narrative_ontology:measurement_basis(lice_be_t1995, observed).
narrative_ontology:measurement(lice_be_t2010, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2010, 0.71).
narrative_ontology:measurement_basis(lice_be_t2010, observed).
narrative_ontology:measurement(lice_be_t2025, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(lice_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t1950, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1950, 0.62).
narrative_ontology:measurement_basis(lice_su_t1950, observed).
narrative_ontology:measurement(lice_su_t1965, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1965, 0.67).
narrative_ontology:measurement_basis(lice_su_t1965, observed).
narrative_ontology:measurement(lice_su_t1980, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1980, 0.71).
narrative_ontology:measurement_basis(lice_su_t1980, observed).
narrative_ontology:measurement(lice_su_t1995, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement_basis(lice_su_t1995, observed).
narrative_ontology:measurement(lice_su_t2010, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement_basis(lice_su_t2010, observed).
narrative_ontology:measurement(lice_su_t2025, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2025, 0.8).
narrative_ontology:measurement_basis(lice_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, resource_allocation).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, graduated_access_filter).

% DUAL FORMULATION NOTE:
% Constraint family: 'occupational licensing' decomposes into at least three structurally distinct constraints sharing one statutory kernel. This member authors epsilon for the standing licensing arrangement as seen from the rent-seeking reading (high extraction, artificial scarcity). public_safety_coordination authors low epsilon over the same referent; graduated_access_filter authors intermediate epsilon with a class-stratified victim structure. Edge direction: the rent-seeking reading's empirical program (wage-premium and quality-null studies) supplies the evidentiary pressure that erodes the safety reading's legitimacy conditions — hence the influences edge toward public_safety_coordination — while graduated_access_filter coexists as an independent critical faction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
