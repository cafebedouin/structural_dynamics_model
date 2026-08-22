% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement Supremacy over National Welfare Boundaries (Integration-Primary Reading)
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   Under the integration-primary reading of the
 *   federation_membership_obligations kernel, free movement is constitutive
 *   of Union citizenship and single market functioning, and member state
 *   welfare boundaries must yield to mobility rights. The standing
 *   arrangement under contest is this one: a supranational mobility floor,
 *   enforced by the European Court of Justice through supremacy and direct
 *   effect, that admits mobile workers into host-state welfare systems on
 *   terms host states cannot unilaterally narrow. The arrangement performs
 *   genuine coordination, portable entitlements and a continental labor pool
 *   solve a real collective-action problem, while the same structure
 *   concentrates adjustment costs on parties who never consented to them:
 *   displaced local labor, host-state taxpayers, and national administrations
 *   whose boundary-setting authority has been transferred upward. KEY AGENTS
 *   (by structural relationship): mobile_eu_workers: primary beneficiary
 *   (organized/mobile) — enters full host welfare beneficiary set;
 *   labor_recruiting_employers: secondary beneficiary (powerful/arbitrage) —
 *   widened labor pool; welfare_exporting_member_states: secondary
 *   beneficiary (institutional/constrained) — sheds welfare burdens via
 *   emigration; european_court_of_justice: agenda setter
 *   (institutional/identity_locked) — authority expands with each settled
 *   dispute; eu_commission: enforcement arm (institutional/identity_locked);
 *   displaced_local_labor: primary target (moderate/trapped) — bears
 *   adjustment costs; receiving_state_taxpayers: target
 *   (moderate/constrained) — funds equal access regardless of contribution
 *   history; national_welfare_administrations: target with residual
 *   authorship (institutional/constrained); welfare_closure_movements:
 *   excluded challenger (organized/constrained);
 *   national_constitutional_courts: checking observer
 *   (institutional/analytical). FAMILY NOTE: the colloquial label 'free
 *   movement versus welfare boundaries' covers structurally distinct claims
 *   depending on which reading of the kernel governs. Per the
 *   epsilon-invariance discipline, this story authors epsilon for the
 *   integration-primary arrangement only, assessed from this reading's own
 *   seat; the sibling stories member_sovereignty_primary and
 *   selective_solidarity instantiate different constraints with their own
 *   epsilon values, beneficiary structures, and classifications, and are
 *   linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - mobile_eu_workers: primary beneficiary (organized/mobile) — gains host-state welfare access and portable entitlements
 *   - labor_recruiting_employers: secondary beneficiary (powerful/arbitrage) — continental recruiting pool, relocation leverage
 *   - welfare_exporting_member_states: secondary beneficiary (institutional/constrained) — burden-shedding through emigration
 *   - european_court_of_justice: agenda setter (institutional/identity_locked) — expands authority via case law
 *   - eu_commission: enforcement arm (institutional/identity_locked) — infringement machinery and directive drafting
 *   - displaced_local_labor: primary target (moderate/trapped) — bears wage, housing, and restructuring costs
 *   - receiving_state_taxpayers: target (moderate/constrained) — funds equal access without contribution filter
 *   - national_welfare_administrations: target with residual authorship (institutional/constrained) — administers floors it cannot set
 *   - welfare_closure_movements: excluded challenger (organized/constrained) — electoral voice without adjudicative voice
 *   - national_constitutional_courts: checking observer (institutional/analytical) — identity-review reserve powers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.62).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.68).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement Supremacy over National Welfare Boundaries (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, 'd70d8a7c-49ac-41d9-829c-f0087aebfb67').
narrative_ontology:cs_kernel_codification('d70d8a7c-49ac-41d9-829c-f0087aebfb67', formalized).
narrative_ontology:cs_authority_grounding('d70d8a7c-49ac-41d9-829c-f0087aebfb67', lineage).
narrative_ontology:cs_interpretation_layer_present('d70d8a7c-49ac-41d9-829c-f0087aebfb67').
narrative_ontology:cs_reading_relation('d70d8a7c-49ac-41d9-829c-f0087aebfb67', federation_membership_obligations__member_sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('d70d8a7c-49ac-41d9-829c-f0087aebfb67', federation_membership_obligations__selective_solidarity, forecloses).
narrative_ontology:cs_axiom('d70d8a7c-49ac-41d9-829c-f0087aebfb67', foundational, union_citizenship_entails_equal_welfare_access).
narrative_ontology:cs_axiom_status(union_citizenship_entails_equal_welfare_access, holdable).
narrative_ontology:cs_axiom_grounding('d70d8a7c-49ac-41d9-829c-f0087aebfb67', union_citizenship_entails_equal_welfare_access, deontological).
narrative_ontology:cs_axiom('d70d8a7c-49ac-41d9-829c-f0087aebfb67', secondary, single_market_requires_unimpeded_labor_mobility).
narrative_ontology:cs_axiom_status(single_market_requires_unimpeded_labor_mobility, holdable).
narrative_ontology:cs_axiom_grounding('d70d8a7c-49ac-41d9-829c-f0087aebfb67', single_market_requires_unimpeded_labor_mobility, empirically_contingent).
narrative_ontology:cs_reference_frame('d70d8a7c-49ac-41d9-829c-f0087aebfb67', teleological_integration_mandate).
narrative_ontology:cs_drift_state('d70d8a7c-49ac-41d9-829c-f0087aebfb67', post_brexit_benefit_tourism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d70d8a7c-49ac-41d9-829c-f0087aebfb67', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, labor_recruiting_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, welfare_exporting_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, european_court_of_justice).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_state_taxpayers).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, national_welfare_administrations).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, eu_law_supremacy_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, direct_effect_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, non_discrimination_on_nationality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens of one member state who take up employment, job-seeking, or study in another. They enter the host state's welfare system on terms the host state cannot narrow below the common floor, and their social security records follow them across borders. Their exit is the arrangement itself: they can relocate to any member state carrying acquired rights with them.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    organized, biographical, mobile, continental).

% Firms that hire across borders and locate operations where needed labor sits. Mobility rights widen their recruiting pool and let them shift staffing between countries without sponsoring immigration. They can relocate production or headquarters if any single country's cost structure turns unfavorable.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, labor_recruiting_employers, beneficiary,
    powerful, biographical, arbitrage, global).

% Lower-income member states whose nationals work abroad and send remittances home, and whose unemployed can draw support in richer host states rather than at home. They gain relief for domestic budgets and labor markets but cannot alter the rules their nationals rely on, since treaty change requires unanimity.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, welfare_exporting_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Adjudicates every conflict between national welfare rules and mobility rights. Each ruling extends or clarifies the reach of the mobility floor, and its docket and doctrinal territory grow with each dispute it settles. Its institutional self-understanding is bound up with the integration project it polices; retreating from its own case-law line would undercut the legitimacy narrative that sustains its authority.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, european_court_of_justice, agenda_setter,
    institutional, generational, identity_locked, continental).

% Initiates infringement proceedings against member states that restrict welfare access for mobile citizens and proposes the directives that set the floor. Its identity as guardian of the treaties and motor of integration depends on enforcing the arrangement; it has no way out of the enforcement role short of treaty change.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_commission, agenda_setter,
    institutional, generational, identity_locked, continental).

% Workers in receiving regions, often lower-wage and lower-skill, who compete with incoming labor and absorb wage pressure, housing-cost increases, and workplace restructuring. They vote nationally but hold no seat in the forum where the rules are made or adjudicated; leaving affected regions means giving up family networks, housing equity, and local seniority.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    moderate, immediate, trapped, regional).

% Residents of host states whose taxes fund welfare systems that must admit newly arrived mobile citizens on equal terms regardless of prior contribution. They can change governments but not the underlying obligation; relocating abroad to escape the tax base means severing residence, employment, and family.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_state_taxpayers, payer,
    moderate, biographical, constrained, national).

% Ministries and agencies that operate benefit systems under eligibility floors they did not choose alone. They co-draft implementing directives in the Council and defend national rules in litigation, but final say in conflicts belongs to the European Court of Justice, and rules drafted without anticipating its case law fail.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_welfare_administrations, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, national_welfare_administrations, agenda_setter).

% National parties and campaigns pressing to restore residence-based welfare eligibility, citing benefit migration as evidence. They win elections and referendums, but their preferred instruments, such as residence tests and contribution periods for newcomers, are repeatedly struck down in court, leaving them without a voice in the venue that actually decides the question.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, welfare_closure_movements, excluded,
    organized, biographical, constrained, national).

% Constitutional tribunals in member states that review whether EU-derived obligations exceed what national identity and competence reserves permit. They take no part in setting the rules but periodically assert authority to disapply them in extremis, and their reservations shape how far enforcement can push.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of many separately closed labor markets and welfare systems: it provides a common non-discrimination floor for workers who move, portable social security coordination, and a continental labor pool that no state could open unilaterally without exposing itself to free-riding by the others.
% TRANSFER_FUNCTION: Moves welfare obligations toward whichever member state hosts the mobile worker; moves labor-market adjustment costs onto receiving-region workers; moves adjudicative authority over membership boundaries from national institutions to the European Court of Justice; moves remittance and burden-shedding gains toward exporting states.
% ABSENT_VOICES: Displaced local labor in receiving regions and receiving-state welfare administrators were absent from the founding conversation, which was negotiated around market-integration aims with social adjustment costs unrepresented. Today they speak through national elections but not in the courtroom where the binding decisions are made; welfare_closure_movements hold office yet cannot translate electoral wins into durable rules because the adjudicating forum is beyond their reach.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, member states would reimpose residence-based welfare eligibility within months, social security coordination would collapse into bilateral renegotiation, millions of mobile citizens would lose acquired rights mid-career, cross-border labor flows would contract sharply, and the European Court of Justice would lose one of its principal jurisdictional pillars. The single market's labor dimension would reorganize around nationality conditions.
% FOUNDING_PROBLEM: Postwar Western Europe sought to make interstate war impossible by interweaving national economies irreversibly; national closure of labor markets and welfare systems was treated as an engine of autarky and nationalism. The founding problem was locking national markets open so that closure could not be restored by ordinary politics.
% FOUNDING_PROBLEM_CORROBORATION: Member-state governments, the paying side of the arrangement, attest the founding problem's liveness through continued treaty compliance despite sustained grievance: they litigate individual rules but do not move to dissolve the mobility floor, indicating they still rate national closure as the greater danger. Historians of European integration and security scholars outside the beneficiary set corroborate the autarky-and-fragmentation genealogy. No corroborating source outside the beneficiary set attests the stronger claim that the original problem, rather than institutional momentum, is what currently sustains the arrangement.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.62: the arrangement carries a real coordination floor (portability, non-discrimination, solved free-rider problem) but its cost incidence is asymmetric — the seats that bear adjustment costs and open-ended fiscal exposure did not author the terms, and the Court's case law ratchets access outward faster than compensation mechanisms arrive. Suppression 0.68 is a raw structural property, unscaled by power or scope: supremacy and direct effect remove the member-state alternative of unilateral closure entirely, and the infringement machinery actively strikes down attempted closures. Theater ratio 0.32: the market-coordination core is functional, but a growing share of activity is citizenship rhetoric and commemorative integration language draped over what is substantively an economic-mobility regime — the gap between the constitutive-citizenship claim and the worker-centered case law widens over the interval. Accessibility collapse 0.52: once supremacy is understood, unilateral closure alternatives collapse, but partial exits persist (Danish opt-outs, accession transition periods, the Brexit demonstration that exit is possible at prohibitive cost). Resistance 0.66: sustained member-state litigation, benefit-tourism electoral mobilization, and one completed member exit. The measurement series run on one shared time grid (1990, 1996, 2002, 2008, 2014, 2020) with every tracked metric authored at every point; all points are observed. Receipt surface: gain_flow is authored as 'diffuse' after checking every named seat — material gains split three ways among mobile workers (welfare access), employers (labor flexibility), and exporting states (burden-shedding), with the Court accruing a non-material authority gain, and no single seat capturing the flow. fixing_cost is 'prohibitive': the seats that could fix the arrangement (member states collectively, or any one unilaterally) face unanimous-treaty-revision costs or exit costs demonstrated by Brexit to exceed any single seat's benefit from fixing. Claim and metrics are independent authored facts: the tangled_rope claim reflects the judgment that coordination and cost-transfer run through the same structure; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from identical treaty text. From the Court's and Commission's positions the arrangement is a rights charter they built and police, with each ruling completing an unfinished market; from the displaced-labor and taxpayer seats the same rulings are uncompensated cost imposition decided in a forum they cannot reach; from the exporting-state seat it is a subsidy their budgets quietly receive. National welfare administrations straddle the line: they co-author the implementing directives yet lose every boundary conflict that reaches adjudication. The engine computes this divergence from power, exit, and directional data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers sit near the beneficiary pole (declared beneficiary, mobile exit — the arrangement subsidizes their relocation). Employers sit nearest the pole (beneficiary, arbitrage-grade exit). Exporting states are beneficiaries with constrained exit — they collect burden-shedding but cannot shape terms. The Court is a beneficiary of its own docket growth while administering the arrangement; its identity lock keeps it from moderating the line that feeds its authority. Displaced local labor sits near the full-target pole (victim, trapped exit — region-bound, absorbing costs with no adjudicative voice). Receiving-state taxpayers are targets with constrained exit. National welfare administrations are declared victims but hold a secondary agenda-setter seat through Council participation, tempering their target-side position slightly; no directionality override is authored because the beneficiary/victim declarations plus exit options already track these relationships, and the dual-position nuance is carried by the secondary_role declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite misreadings. Reading the arrangement as pure coordination (rope) hides the uncompensated adjustment costs borne by trapped regional labor and unfiltered fiscal exposure borne by host taxpayers — costs that persist and accumulate precisely because the structure's enforcement suppresses the exits that would price them. Reading it as pure extraction (snare) misses the genuine coordination achievement: portable entitlements and a common non-discrimination floor solve a free-rider problem no member state could solve alone, and dismantling the floor would impose real losses on the very workers the extraction critique defends. The tangled-rope structure keeps both facts load-bearing. On obsolescence: the founding problem (irreversibility of market opening against nationalist closure) remains live — corroborated by the paying parties' own continued compliance — so no mandatrophy resolution is declared; the arrangement's persistence tracks its function, degraded at the margins by citizenship rhetoric but not yet maintained as performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'This story instantiates the integration-primary reading of the federation_membership_obligations kernel; would the member-sovereignty-primary or selective-solidarity readings produce a different beneficiary/victim structure and classification for the same treaty text?',
    'Classify the sibling stories member_sovereignty_primary and selective_solidarity and compare computed per-seat types; the disagreement is located in who holds closure authority over welfare access — the supranational adjudicator, the national welfare state, or the contribution record.',
    'Under member-sovereignty-primary the victim and beneficiary sets largely invert: member-state governments become the defended seat and mobile workers the constrained party. Under selective-solidarity the taxpayer seat is shielded by the contributory filter and displaced-labor exposure narrows. Effective extraction redistributes across seats rather than disappearing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Which reading of the membership-obligations kernel governs changes the constraint''s entire beneficiary/victim geometry.').

omega_variable(
    displacement_cost_incidence,
    'How large and persistent are the adjustment costs borne by displaced local labor in receiving regions, relative to the aggregate gains from labor mobility?',
    'Quasi-experimental labor economics on accession shocks: wage, housing-cost, and employment effects in receiving regions, with duration analysis of whether displacement persists or is absorbed.',
    'Small, localized, transient costs push the arrangement toward the coordination-dominated end; large, persistent, geographically concentrated costs push it toward the extraction-dominated end and strengthen coalition-power prospects for the trapped seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_cost_incidence, empirical, 'Magnitude and persistence of the adjustment costs the arrangement transfers onto receiving-region labor.').

omega_variable(
    fiscal_balance_of_mobile_households,
    'Do mobile workers net-contribute to or net-draw on host-state welfare systems over a full working life?',
    'Longitudinal administrative fiscal-incidence studies tracking tax contributions against benefit receipts for mobile cohorts across the life cycle.',
    'Net contribution supports a mutual-insurance framing and lowers the taxpayer seat''s effective burden; net drawing supports the burden-transfer framing and raises the taxpayer seat''s effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_balance_of_mobile_households, empirical, 'Life-cycle fiscal sign of the mobile-worker population in host welfare systems.').

omega_variable(
    ecj_authority_feedback_loop,
    'Is the Court''s expanding authority a byproduct of resolving genuine disputes, or a self-reinforcing driver that shapes the incentive landscape producing the disputes it then resolves?',
    'Docket-composition analysis: the share of mobility cases initiated by private litigants strategically invoking acquired rights versus referrals arising independently of the case-law line.',
    'If self-reinforcing, the authority structure drifts from lineage toward extraction grounding, changing the commitment-system classification and the Court seat''s effective directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecj_authority_feedback_loop, conceptual, 'Whether the Court''s authority growth is exogenous dispute resolution or endogenous self-expansion.').

omega_variable(
    anticipatory_compliance_share,
    'How much of the observed closure of national welfare-policy options is imposed by adverse rulings versus produced by national drafters self-censoring to survive anticipated review?',
    'Compare pre-enactment legal advice, withdrawn or never-proposed bills, and legislative-history records against actual strike-down rates in Court litigation.',
    'A high internalized share means the arrangement''s suppressive force exceeds what litigation data shows and would persist even if the Court moderated — the measured suppression understates the true constraint on member-state choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anticipatory_compliance_share, empirical, 'Split between Court-imposed and internally anticipated restriction of national welfare policy space.').

omega_variable(
    identity_lock_reversibility,
    'Would the Court''s and Commission''s enforcement behavior change if their integrationist identity frame broke, for instance after a successful mass-exit precedent?',
    'Counterfactual and comparative analysis of enforcement intensity before and after Brexit: infringement initiation rates, deference in welfare-boundary cases, and doctrinal hedging in subsequent rulings.',
    'If enforcement is identity-driven rather than interest-driven, the exit options attributed to these institutional seats are misclassified and their effective directionality shifts toward the target pole once the identity frame breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, conceptual, 'Whether the enforcing institutions'' behavior is constitutively tied to the integrationist identity frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmo_integration_primary_tr_t1990, federation_membership_obligations__integration_primary, theater_ratio, 1990, 0.14).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t1990, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t1996, federation_membership_obligations__integration_primary, theater_ratio, 1996, 0.19).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t1996, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2002, federation_membership_obligations__integration_primary, theater_ratio, 2002, 0.22).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2002, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2008, federation_membership_obligations__integration_primary, theater_ratio, 2008, 0.26).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2008, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2014, federation_membership_obligations__integration_primary, theater_ratio, 2014, 0.3).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2014, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2020, federation_membership_obligations__integration_primary, theater_ratio, 2020, 0.32).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(fmo_integration_primary_be_t1990, federation_membership_obligations__integration_primary, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t1990, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t1996, federation_membership_obligations__integration_primary, base_extractiveness, 1996, 0.46).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t1996, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2002, federation_membership_obligations__integration_primary, base_extractiveness, 2002, 0.5).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2002, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2008, federation_membership_obligations__integration_primary, base_extractiveness, 2008, 0.57).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2008, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2014, federation_membership_obligations__integration_primary, base_extractiveness, 2014, 0.61).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2014, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2020, federation_membership_obligations__integration_primary, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(fmo_integration_primary_su_t1990, federation_membership_obligations__integration_primary, suppression_requirement, 1990, 0.44).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t1990, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t1996, federation_membership_obligations__integration_primary, suppression_requirement, 1996, 0.5).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t1996, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2002, federation_membership_obligations__integration_primary, suppression_requirement, 2002, 0.55).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2002, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2008, federation_membership_obligations__integration_primary, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2008, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2014, federation_membership_obligations__integration_primary, suppression_requirement, 2014, 0.66).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2014, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2020, federation_membership_obligations__integration_primary, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, selective_solidarity).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the federation_membership_obligations kernel. The colloquial label 'free movement versus welfare boundaries' conflates three structurally distinct claims: integration_primary (this story — mobility rights constitutive, welfare boundaries yield, epsilon authored over the integration-primary arrangement from that reading's seat), member_sovereignty_primary (national closure authority retained, mobility conditional), and selective_solidarity (tiered access by contribution history). Each is a separate file with its own epsilon, beneficiaries, victims, and classification; they are linked here because the upstream integration-primary case law is cited as authoritative ground by the other readings' proponents and opponents alike, and because contamination propagates across the family: a Court retrenchment reshapes the operating environment of both siblings without resolving the contest among them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
