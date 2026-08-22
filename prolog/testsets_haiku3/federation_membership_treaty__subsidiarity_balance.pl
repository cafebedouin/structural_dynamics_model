% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Federation Member States' Subsidiarity-Balanced Mobility Rights
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The federation membership treaty's free-movement provision operates under
 *   a subsidiarity-balanced proportionality test: member states retain
 *   authority to restrict cross-border labor mobility where they can justify
 *   a 'legitimate national interest' and the restriction is 'necessary' and
 *   'proportionate.' This is ONE reading of a contested kernel. The
 *   integration-primary reading treats any restriction as presumptively
 *   illegitimate unless meeting strict scrutiny. The sovereignty-primary
 *   reading grants member states near-unilateral authority. The
 *   subsidiarity-balance reading (this one) treats mobility as a baseline
 *   right constrained by proportionality review. The constraint's
 *   extractiveness reflects asymmetric access: mobile workers and
 *   multinational employers benefit from baseline coordination; immobile
 *   workers and local labor administrators bear the cost. The suppression
 *   metric captures the active enforcement of proportionality
 *   boundaries—federal courts suppress both unrestricted mobility claims
 *   (proportionality limits the baseline) and blanket state closures
 *   (proportionality prevents total exclusion). Theater ratio reflects the
 *   interpretive gap: proportionality review is real adjudication, but
 *   significant energy goes to maintaining the fiction that both sides 'win'
 *   (members preserve regulatory discretion, workers retain mobility).
 *
 * KEY AGENTS:
 *   - mobile_workers_with_skills — benefit from baseline right to cross-border labor access; face member state restrictions justified by proportionality
 *   - multinational_employers — benefit from federated labor-market coordination; face graduated regulatory burdens per sector and jurisdiction
 *   - federal_union_authority — agenda-setter; administers proportionality test and shapes beneficiary/victim structure through case law
 *   - immobile_domestic_workers — targeted victims; experience wage pressure and employment displacement in labor markets opened to competition
 *   - local_welfare_administrators — payer seats; manage welfare systems under fiscal pressure from intra-federation mobility, constrained by proportionality limits on residency requirements
 *   - member_states_labor_protection — dual role; extract via justified restrictions, benefit from federated coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.58).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.51).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Federation Member States' Subsidiarity-Balanced Mobility Rights").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '4c8bf1a7-5882-4568-a088-61cd2c2d5b77').
narrative_ontology:cs_kernel_codification('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', fixed_text).
narrative_ontology:cs_authority_grounding('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', lineage).
narrative_ontology:cs_interpretation_layer_present('4c8bf1a7-5882-4568-a088-61cd2c2d5b77').
narrative_ontology:cs_reading_relation('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', foundational, proportionality_as_mediator).
narrative_ontology:cs_axiom_status(proportionality_as_mediator, holdable).
narrative_ontology:cs_axiom_grounding('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', proportionality_as_mediator, conventional).
narrative_ontology:cs_axiom('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', foundational, dual_legitimacy_both_mobility_and_protection).
narrative_ontology:cs_axiom_status(dual_legitimacy_both_mobility_and_protection, holdable).
narrative_ontology:cs_axiom_grounding('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', dual_legitimacy_both_mobility_and_protection, deontological).
narrative_ontology:cs_reference_frame('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', treaty_founding_balance).
narrative_ontology:cs_drift_state('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', contemporary_member_state_welfare_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c8bf1a7-5882-4568-a088-61cd2c2d5b77', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_workers_with_skills).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, multinational_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, federal_union_authority).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, immobile_domestic_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, local_welfare_administrators).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_states_labor_protection).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, member_states_labor_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access to labor markets across member states; can arbitrage wage differentials, taxation, and skill demand across borders. Proportionality framing protects their baseline right while allowing member states to impose differential regulatory burdens (licensing, language certification, welfare access conditions). They benefit from the federation guarantee against total closure, though specific restrictions apply domain-by-domain.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_workers_with_skills, beneficiary,
    powerful, biographical, arbitrage, global).

% Access to a unified labor pool; can deploy workforce across member states within proportionality constraints. Subsidiarity framing prevents any single member state from imposing conditions that would fragment their operations, while proportionality allows targeted restrictions (e.g., minimum wage harmonization, sector-specific licensing). The arrangement coordinates their supply chains across borders.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, multinational_employers, beneficiary,
    institutional, generational, mobile, global).

% Administers and interprets the proportionality test; decides what constitutes a 'legitimate national interest,' what restrictions are 'necessary,' and what becomes 'disproportionate.' Collects legitimacy from the founding treaty and ongoing supranational jurisprudence. Defends both the baseline mobility right and member state regulatory discretion as complementary, not zero-sum.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federal_union_authority, agenda_setter,
    institutional, generational, analytical, universal).

% Face wage pressure and employment displacement in local markets where mobile cross-border workers compete. Proportionality framing allows member states to impose labor-market safeguards (residence periods, seniority rules, domestic hiring requirements), but the baseline mobility right is not eliminated—restrictions must be justified and narrowly tailored. They bear the cost of labor-market segmentation without necessarily benefiting from mobility.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, immobile_domestic_workers, payer,
    organized, biographical, constrained, regional).

% Manage welfare systems (housing, healthcare, unemployment) under fiscal pressure from intra-federation mobility. Subsidiarity principle allows them to impose residency requirements (e.g., three-year contribution before access to benefits), but proportionality limits how restrictive these can be. They navigate between protecting local welfare solvency and respecting federated mobility guarantees.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, local_welfare_administrators, payer,
    moderate, biographical, constrained, local).

% Retain regulatory authority to protect domestic labor markets and welfare systems but under the proportionality constraint. They can impose restrictions (sector-specific visa quotas, language requirements, credential recognition delays) if justified by legitimate national interests, but cannot eliminate mobility rights wholesale. This dual role means they both extract (collect restrictions on foreign workers) and benefit (access to federation's economic coordination).
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states_labor_protection, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, member_states_labor_protection, beneficiary).

% Would argue that any state-imposed mobility restriction is presumptively illegitimate unless meeting the strictest judicial scrutiny. They are structurally outside this reading's decision-making frame; subsidiarity-balance explicitly grants member states discretion that integration-primary reading would deny them. Their preferred alternative is a different constraint (integration_primary reading) not this one.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, integration_primary_advocates, excluded,
    organized, generational, trapped, universal).

% Would argue that member states should retain unilateral authority to restrict mobility to protect labor markets and welfare systems without proportionality review. They are excluded from the subsidiarity-balance frame, which subordinates sovereignty claims to a proportionality test administered by federal authority. Their preferred alternative is the sovereignty_primary reading.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sovereignty_primary_advocates, excluded,
    organized, generational, trapped, national).

% Interpret and enforce the proportionality standard; review member state restrictions and advance opinions on whether a given measure is 'necessary' and 'proportionate.' Their case law operationalizes the abstract principle and creates binding precedent that both constrains and justifies state action. They do not author policy but shape its interpretive boundaries.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federal_courts_and_tribunals, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__subsidiarity_balance, multinational_employers).
narrative_ontology:fixing_cost_class(federation_membership_treaty__subsidiarity_balance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a federated labor market where workers and employers can operate across member state boundaries, reducing transaction costs of cross-border employment and labor matching. Balances this coordination benefit against member states' legitimate interest in managing local labor-market outcomes and welfare sustainability.
% TRANSFER_FUNCTION: Moves labor-market access (the ability to compete for jobs across state lines) from immobile domestic workers toward mobile cross-border workers and multinational employers. Simultaneously transfers regulatory authority (the ability to impose protective restrictions) from member states toward federal arbiters interpreting proportionality.
% ABSENT_VOICES: Member state legislatures whose electorates did not consent to proportionality review of labor-market restrictions, and immobile workers organized at the local level who would argue that wage pressure from cross-border competition outweighs coordination gains. Both are nominally represented through member state governments but face structural exclusion from proportionality-review processes dominated by federal judges and employers.
% DISAPPEARANCE_RATIONALE: If the proportionality-balanced subsidiarity constraint vanished, member states would revert to either near-total mobility (integration-primary reading) or near-total closure (sovereignty-primary reading) depending on political power; labor markets would re-segment by national boundary; multinational employers would face fragmented hiring rules; federal union authority's legitimacy would collapse into a choice between unilateral state sovereignty or unilateral supranational integration.
% FOUNDING_PROBLEM: Early federation integration fragmented labor markets by national boundary despite economically inefficient outcome; unrestricted mobility created welfare-system stress and local labor-market disruption; unilateral state closure prevented any federated coordination. The subsidiarity-balance was designed to split the difference: maintain baseline coordination while preserving state protective capacity.
% FOUNDING_PROBLEM_CORROBORATION: Federal authority and multinational employers attest the founding problem (fragmentation, inefficiency) remains live and justifies proportionality review. Domestic worker advocates and some member state legislatures attest the founding problem was solved by coordination itself and proportionality review now protects only employer and federal interests, not local labor-market stability. Economic analyses from independent researchers show mixed outcomes: labor-market efficiency gains in some sectors offset by wage depression in others; welfare-system pressure persistent in high-immigration member states.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type (tangled_rope) reflects the hybrid structure: genuine coordination function (federated labor market solves matching inefficiency and employer supply-chain fragmentation) paired with asymmetric extraction (mobile workers and employers gain baseline access; immobile workers and locals pay via wage pressure and welfare strain). Extractiveness at 0.58 (moderate, rising) captures this hybrid: the proportionality test is real and constrains both directions, but the structural outcome favors the mobile/institutional seats. Suppression at 0.51 reflects the enforcement of proportionality boundaries themselves: federal courts suppress both extremes (unrestricted mobility and blanket closures), making the median position the only stable equilibrium. Theater at 0.42 reflects the interpretive cost: maintaining the fiction that proportionality is an objective test when it is contestable (what counts as 'legitimate national interest'? how 'necessary' must a restriction be?). The measurement series shows extractiveness rising to 0.60 at t=25 (proportionality doctrine accumulating precedent favoring mobility), then slightly declining at t=35 (backlash from member states and domestic worker coalitions introducing new 'legitimate interests' like anti-polarization and regional cohesion). Suppression remains stable (0.48–0.56 range), indicating the proportionality enforcer-role is consistent. Theater peaks at t=15–25 as case-law density increases, then slightly subsides.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (mobile workers, multinational employers, federal authority) experience this as genuine coordination—a market-enabling mechanism with appropriate safeguards. The payer seats (immobile workers, local administrators, member state governments) experience it as coordinated extraction—coordination benefits the mobile/corporate/supranational players; constraints on their own protective measures are the extractive component. Federal courts experience it as neutral adjudication; member state capitals experience it as loss of regulatory authority to supranational judges. The engine should compute type divergence from these structural positions: the same constraint might classify as rope from the beneficiary seats (coordination gains, acceptable constraints) and as snare from the payer seats (no meaningful coordination benefit, costs unshared).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mobile workers, institutional employers, federal authority) have low directionality (d = 0.1–0.3): the constraint subsidizes their access and legitimizes their positions. Victims (immobile workers, local administrators, member state protective capacity) have high directionality (d = 0.65–0.85): they bear labor-market cost, welfare pressure, and regulatory constraint. The federal union authority has d near 0 (it authors and enforces, collecting legitimacy not extraction). Multinational employers have d = 0.2 (gradient constraints per sector, but baseline access secured). Member states in dual role (payer + beneficiary) land near d = 0.5–0.6 depending on their economic profile: net-immigration states extract through restrictions (higher d), net-emigration states benefit from labor access (lower d). No override needed; the derivation captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (labor-market fragmentation, welfare sustainability risk, inefficient matching) is contested as to status. Federal authority and economic analyses claim it remains live—mobility benefits outweigh welfare costs. Member state legislatures and immobile-worker coalitions claim it is substantially solved by coordination itself and proportionality review now protects only multinational and mobile interests. If founding_problem_status = 'dead' while disappearance_verdict = 'world_rearranges', the constraint is a zombie—persisting because beneficiaries (federal authority, multinational employers) maintain it, not because the original coordination problem exists. The measurement series shows extractiveness stabilizing around 0.58–0.60 despite case-law accumulation, suggesting the constraint is neither accumulating new coordination value nor pure rent-seeking, but rather holding at a hybrid position. Mandatrophy is not triggered; instead, the constraint sits at a contested equilibrium where proportionality review is the only mechanism preventing either integration-primary (unrestricted mobility) or sovereignty-primary (near-total closure) dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_test_operationalization,
    'What constitutes a ''legitimate national interest'' and how strictly must ''necessity'' and ''proportionality'' be demonstrated before a member state''s mobility restriction is upheld?',
    'Systematic audit of federal court rulings: measure what fraction of state restrictions pass proportionality review; map the evolution of the test''s rigor; identify which claimed ''national interests'' (labor protection, welfare solvency, integration-resistance, public order) are most frequently validated.',
    'If the test is applied strictly, extractiveness tilts toward integration-primary (beneficiary seats) and proportionality becomes theater. If applied loosely, extractiveness tilts toward sovereignty-primary (victims regain protective capacity). The measurement''s tightness determines whether this is genuine subsidiarity-balance or masquerade for either extreme.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_test_operationalization, empirical, 'Whether proportionality review is genuinely balanced or masks a hidden bias.').

omega_variable(
    kernel_reading_contestation,
    'Is the subsidiarity-balance reading coherent as a stable equilibrium, or does it collapse toward either integration-primary or sovereignty-primary under material pressure (labor crises, welfare shocks, political backlash)?',
    'Track constraint drift under stress: observe member state behavior during labor-market downturns, welfare-system fiscal crises, and political anti-immigration campaigns. Does proportionality review hold as a stable framework, or do member states retreat to either blanket closures or federal authority override state restrictions?',
    'If subsidiarity-balance is structurally stable, this constraint is a genuine three-way compromise. If it collapses, the kernel''s contested nature means one of the sibling readings (integration_primary or sovereignty_primary) will dominate and this constraint becomes historically contingent rather than foundationally stable. The engine''s atractor calculation should track this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the subsidiarity-balance reading is an equilibrium or a metastable state.').

omega_variable(
    beneficiary_invisibility_asymmetry,
    'Are the beneficiaries of this constraint (mobile workers, multinational employers, federal authority) visible enough in democratic discourse to sustain proportionality review as legitimate, or does the asymmetric burden on immobile workers and member states create a legitimacy deficit?',
    'Measure public opinion in member states on free movement and proportionality constraints; track legislative voting patterns on migration restrictions; examine whether anti-movement coalitions frame proportionality as theater or genuine safeguard.',
    'High legitimacy deficit would predict sustained pressure toward sovereignty-primary reading (member state populations demanding restoration of protective authority). Low deficit would predict stable subsidiarity-balance. This feeds into whether suppression_requirement stays moderate (0.51) or escalates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_invisibility_asymmetry, empirical, 'Whether the constraint''s legitimacy is sustainable across member state democracies.').

omega_variable(
    sibling_reading_asymmetry_integration_vs_sovereignty,
    'Why does integration_primary reading treat member state restrictions as presumptively illegitimate, while sovereignty_primary reading treats federal constraints as presumptively illegitimate? Does this asymmetry reflect genuine structural logic or power differentials among the readings'' constituencies?',
    'Examine the founding treaty''s text and negotiation history; compare the institutional power of integration advocates (multinational employers, federal bureaucracy) with sovereignty advocates (member state governments, domestic labor unions). Assess whether asymmetry is textual or structural.',
    'If asymmetry is structural (one side has more institutional power), proportionality review may be biased and subsidiarity-balance is masquerade. If asymmetry is textual (the treaty genuinely reflects two-sided commitment), proportionality review is more credible. This informs whether the constraint is tangled_rope (genuine hybrid) or snare (extraction-with-coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_asymmetry_integration_vs_sovereignty, conceptual, 'Whether the sibling readings are structurally symmetric or one dominates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_treaty__subsidiarity_balance, theater_ratio, 5, 0.37).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__subsidiarity_balance, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_treaty__subsidiarity_balance, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_treaty__subsidiarity_balance, theater_ratio, 25, 0.45).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t35, federation_membership_treaty__subsidiarity_balance, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(fede_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t35, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 35, 0.58).
narrative_ontology:measurement_basis(fede_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 25, 0.56).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t35, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 35, 0.51).
narrative_ontology:measurement_basis(fede_su_t35, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(fede_grid_01, federation_membership_treaty__subsidiarity_balance, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(fede_grid_02, federation_membership_treaty__subsidiarity_balance, accessibility_collapse(class), 35, 0.68).
narrative_ontology:measurement(fede_grid_03, federation_membership_treaty__subsidiarity_balance, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(fede_grid_04, federation_membership_treaty__subsidiarity_balance, accessibility_collapse(individual), 35, 0.7).
narrative_ontology:measurement(fede_grid_05, federation_membership_treaty__subsidiarity_balance, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(fede_grid_06, federation_membership_treaty__subsidiarity_balance, accessibility_collapse(organizational), 35, 0.65).
narrative_ontology:measurement(fede_grid_07, federation_membership_treaty__subsidiarity_balance, accessibility_collapse(structural), 0, 0.75).
narrative_ontology:measurement(fede_grid_08, federation_membership_treaty__subsidiarity_balance, accessibility_collapse(structural), 35, 0.72).
narrative_ontology:measurement(fede_grid_09, federation_membership_treaty__subsidiarity_balance, resistance(class), 0, 0.78).
narrative_ontology:measurement(fede_grid_10, federation_membership_treaty__subsidiarity_balance, resistance(class), 35, 0.75).
narrative_ontology:measurement(fede_grid_11, federation_membership_treaty__subsidiarity_balance, resistance(individual), 0, 0.68).
narrative_ontology:measurement(fede_grid_12, federation_membership_treaty__subsidiarity_balance, resistance(individual), 35, 0.74).
narrative_ontology:measurement(fede_grid_13, federation_membership_treaty__subsidiarity_balance, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(fede_grid_14, federation_membership_treaty__subsidiarity_balance, resistance(organizational), 35, 0.68).
narrative_ontology:measurement(fede_grid_15, federation_membership_treaty__subsidiarity_balance, resistance(structural), 0, 0.72).
narrative_ontology:measurement(fede_grid_16, federation_membership_treaty__subsidiarity_balance, resistance(structural), 35, 0.7).
narrative_ontology:measurement(fede_grid_17, federation_membership_treaty__subsidiarity_balance, stakes_inflation(class), 0, 0.65).
narrative_ontology:measurement(fede_grid_18, federation_membership_treaty__subsidiarity_balance, stakes_inflation(class), 35, 0.61).
narrative_ontology:measurement(fede_grid_19, federation_membership_treaty__subsidiarity_balance, stakes_inflation(individual), 0, 0.55).
narrative_ontology:measurement(fede_grid_20, federation_membership_treaty__subsidiarity_balance, stakes_inflation(individual), 35, 0.62).
narrative_ontology:measurement(fede_grid_21, federation_membership_treaty__subsidiarity_balance, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(fede_grid_22, federation_membership_treaty__subsidiarity_balance, stakes_inflation(organizational), 35, 0.52).
narrative_ontology:measurement(fede_grid_23, federation_membership_treaty__subsidiarity_balance, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(fede_grid_24, federation_membership_treaty__subsidiarity_balance, stakes_inflation(structural), 35, 0.58).
narrative_ontology:measurement(fede_grid_25, federation_membership_treaty__subsidiarity_balance, suppression(class), 0, 0.58).
narrative_ontology:measurement(fede_grid_26, federation_membership_treaty__subsidiarity_balance, suppression(class), 35, 0.54).
narrative_ontology:measurement(fede_grid_27, federation_membership_treaty__subsidiarity_balance, suppression(individual), 0, 0.45).
narrative_ontology:measurement(fede_grid_28, federation_membership_treaty__subsidiarity_balance, suppression(individual), 35, 0.48).
narrative_ontology:measurement(fede_grid_29, federation_membership_treaty__subsidiarity_balance, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(fede_grid_30, federation_membership_treaty__subsidiarity_balance, suppression(organizational), 35, 0.55).
narrative_ontology:measurement(fede_grid_31, federation_membership_treaty__subsidiarity_balance, suppression(structural), 0, 0.5).
narrative_ontology:measurement(fede_grid_32, federation_membership_treaty__subsidiarity_balance, suppression(structural), 35, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__subsidiarity_balance, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% The federation_membership_treaty kernel instantiates three constraint stories, one per reading. The subsidiarity_balance reading (this one) treats free movement and member state regulatory authority as both legitimate but mediated by proportionality review. The integration_primary reading elevates mobility to presumptive override, treating state restrictions as presumptively illegitimate. The sovereignty_primary reading elevates state authority to presumptive override, treating federal mobility guarantees as conditional. All three share the same referent (the treaty's free-movement provision) but author different ε values and beneficiary/victim structures from their respective readings' perspectives. The readings coexist across different member states and constituencies; no single frame contains all three. Network effects: integration_primary reading's success (more restrictions overturned) would increase effective extraction on member states; sovereignty_primary reading's success (more restrictions upheld) would decrease effective extraction on mobile workers. The subsidiarity_balance reading (this one) persists by maintaining a dynamic equilibrium where both sides claim partial victory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
