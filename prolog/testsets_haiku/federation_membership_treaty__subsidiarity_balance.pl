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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Subsidiarity Balance in Federation Member Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The subsidiarity balance reading of the federation's free-movement
 *   commitment stakes a middle ground between integration-primary (movement
 *   is a constitutive right, restrictions presumptively illegitimate) and
 *   sovereignty-primary (movement is conditional on state consent, states
 *   retain labor-market control). The balance asserts: free movement is a
 *   federation right, but member states retain authority to impose
 *   restrictions on grounds of legitimate national interests (labor-market
 *   stability, welfare-system integrity, public health, public order),
 *   subject to proportionality testing by federation courts. This reading
 *   instantiates the constraint through proportionality doctrine:
 *   restrictions are permissible if they pursue a legitimate aim and are
 *   proportionate to that aim. The constraint therefore distributes
 *   extraction asymmetrically—high-skill mobile workers and service providers
 *   benefit from broad movement rights; stationary workers and local welfare
 *   systems bear costs; territorial regulators face high enforcement burden
 *   navigating proportionality standards. The court (European Court of
 *   Justice) acts as the agenda-setter, defining 'legitimate' and
 *   'proportionate' through case law. Suppression is moderate—not blanket
 *   restriction of movement, but active enforcement of proportionality
 *   doctrine against member-state restrictions—and both upward and downward
 *   mobility restrictions are suppressed (unrestricted movement AND blanket
 *   bans are both policed). Theater has increased over time as
 *   proportionality doctrine becomes more formalized and litigation-driven;
 *   the founding substantive coordination (integrating labor markets) now
 *   sits alongside performative compliance with proportionality standards.
 *
 * KEY AGENTS:
 *   - Mobile high-skill labor: beneficiaries of expanded movement rights; no extraction cost
 *   - Service providers cross-border: beneficiaries capturing regulatory arbitrage; institutional power
 *   - Stationary low-skill labor: payers bearing wage competition; powerless, trapped
 *   - Local welfare systems: payers bearing access-cost pressure; institutional constraint
 *   - Territorial labor-market regulators: dual-positioned payers and agenda-setters; navigating proportionality bounds
 *   - European Court of Justice: agenda-setter interpreting and enforcing proportionality doctrine
 *   - Integration-primary coalition: excluded, contesting the reading's legitimacy
 *   - Sovereignty-primary coalition: excluded, contesting the reading's adequacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.58).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.52).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Subsidiarity Balance in Federation Member Free Movement").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, 'c6822057-f154-4e89-926e-53f527f90d7f').
narrative_ontology:cs_kernel_codification('c6822057-f154-4e89-926e-53f527f90d7f', formalized).
narrative_ontology:cs_authority_grounding('c6822057-f154-4e89-926e-53f527f90d7f', lineage).
narrative_ontology:cs_interpretation_layer_present('c6822057-f154-4e89-926e-53f527f90d7f').
narrative_ontology:cs_reading_relation('c6822057-f154-4e89-926e-53f527f90d7f', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('c6822057-f154-4e89-926e-53f527f90d7f', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('c6822057-f154-4e89-926e-53f527f90d7f', foundational, proportionality_bounded_movement).
narrative_ontology:cs_axiom_status(proportionality_bounded_movement, holdable).
narrative_ontology:cs_axiom_grounding('c6822057-f154-4e89-926e-53f527f90d7f', proportionality_bounded_movement, deontological).
narrative_ontology:cs_axiom('c6822057-f154-4e89-926e-53f527f90d7f', foundational, legitimate_member_state_interests_permissible).
narrative_ontology:cs_axiom_status(legitimate_member_state_interests_permissible, holdable).
narrative_ontology:cs_axiom_grounding('c6822057-f154-4e89-926e-53f527f90d7f', legitimate_member_state_interests_permissible, deontological).
narrative_ontology:cs_reference_frame('c6822057-f154-4e89-926e-53f527f90d7f', proportionality_constrained_free_movement).
narrative_ontology:cs_drift_state('c6822057-f154-4e89-926e-53f527f90d7f', contemporary_post_covid_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c6822057-f154-4e89-926e-53f527f90d7f', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_labor_high_skill).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, service_providers_cross_border).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, federation_institutional_legitimacy).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, stationary_labor_low_skill).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, local_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, territorial_labor_market_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access expanded labor market opportunities across member states; can pursue career paths that require geographic mobility. Bear no direct extraction; gain significantly from the right to move and establish themselves where compensation and opportunity clusters exist. Their legal status is portable across borders; they are the primary constituency supporting unconditional mobility rights.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_labor_high_skill, beneficiary,
    moderate, biographical, mobile, continental).

% Professional service firms (legal, consulting, accounting, engineering) operate continent-wide under mutual recognition frameworks. The subsidiarity balance permits them to move personnel across borders while respecting local regulatory regimes. They capture regulatory arbitrage opportunities — locating teams where regulatory costs are lower while serving clients continent-wide.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, service_providers_cross_border, beneficiary,
    organized, generational, arbitrage, continental).

% Face wage and employment competition from mobile workers willing to accept lower compensation. Cannot easily retrain or relocate; geographic and skill lock-in keeps them locally dependent. The subsidiarity framework permits member states to impose some residency and integration requirements, but these are enforced unevenly and mobile workers often face lower barriers than resident applicants. The constraint extracts from them through compressed wage competition.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, stationary_labor_low_skill, payer,
    powerless, biographical, trapped, local).

% Must absorb the cost of providing access to unemployment benefits, family allowances, and social housing to newly mobile workers while facing fiscal pressure from aging populations and economic cycles. The subsidiarity framework permits eligibility restrictions (work history, residency periods) but these are subject to proportionality review and often invalidated. The constraint forces welfare systems to bear costs they cannot fully exclude through means-testing without triggering legal challenges.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, local_welfare_systems, payer,
    institutional, generational, constrained, national).

% Tasked with maintaining labor market stability and skill matching in their jurisdiction, but constrained by the subsidiarity balance from using blunt geographic exclusions. They can impose licensing requirements, skill certifications, and integration conditions, but must justify each restriction through proportionality analysis. The constraint forces them to negotiate between federation-level free movement rules and local labor market conditions; enforcement costs are high and outcomes are contested in courts.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, territorial_labor_market_regulators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, territorial_labor_market_regulators, agenda_setter).

% The subsidiarity balance is vindicated as the federation's foundational principle: neither pure integration (which would delegitimize the federation in sovereignty-protective member states) nor pure sovereignty (which would collapse the single market and federation's raison d'être). The framework's existence — as a middle path — sustains the federation's internal coalition and external legitimacy. This is not an agent; it is a vindicated institutional arrangement.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_institutional_legitimacy, beneficiary,
    analytical, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__subsidiarity_balance, federation_institutional_legitimacy).

% European Parliament members, progressive member-state governments, and civil-rights organizations that endorse the 'integration_primary' reading—viewing free movement as constitutive of the single market and its restrictions as presumptively illegitimate. They contest the legitimacy of the subsidiarity balance itself, arguing it smuggles sovereignty-protecting carve-outs into what should be an unconditional right. They are excluded from the agenda-setting process that interprets and enforces the balance.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, integration_primary_reading_coalition, excluded,
    organized, generational, constrained, continental).

% Member-state governments emphasizing labor-market protection, welfare-system integrity, and national labor-force preference. They contest the subsidiarity balance as an insufficient shield against unfettered mobile labor, viewing it as a constraint imposed by supranational courts and institutions against their will. They lack enforcement capacity to unilaterally override federation rules but maintain political pressure for stricter residency and integration conditions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sovereignty_primary_reading_coalition, excluded,
    organized, generational, constrained, national).

% Interprets and applies the subsidiarity balance through proportionality doctrine. Adjudicates member-state restrictions on free movement by testing whether the restriction is proportionate to a legitimate aim (labor-market stability, welfare-system integrity, public-health safety). Controls the standard of review and therefore the empirical scope of what 'legitimate' and 'proportionate' mean. Their case law shapes which restrictions are permissible and which are struck down.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, european_court_of_justice, agenda_setter,
    institutional, generational, analytical, continental).

% Enact and enforce restrictions on free movement within the bounds set by the subsidiarity framework and proportionality doctrine. Each state is a co-author of the federation's rules but constrained by federation law in their implementation. They bear the political cost of both mobility (labor-market disruption, welfare costs) and restrictions (claims of discrimination, federation-law violations). They navigate between constituent demands for protection and federation law's requirements.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_state_governments, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, member_state_governments, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__subsidiarity_balance, european_court_of_justice).
narrative_ontology:fixing_cost_class(federation_membership_treaty__subsidiarity_balance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits movement of labor and services across member-state borders while preserving member states' authority to regulate labor markets and welfare-system access. Solves the coordination problem of integrating distinct labor markets (with different wage levels, skill distributions, and welfare-system generosity) under a single institutional roof without delegitimizing the federation in sovereignty-protective states or collapsing the single market in integration-oriented ones.
% TRANSFER_FUNCTION: Moves labor and service workers across borders, concentrating benefits on mobile high-skill workers and service providers while distributing costs across stationary low-skill workers, local labor-market regulators, and welfare systems. The transfer is not monetary but positional: mobility rights accrue to those with the skills and resources to exercise them; protection and stability benefits accrue less reliably to those geographically or skill-locked.
% ABSENT_VOICES: Stationary low-skill workers lack organized representation in federation-level policy-setting and litigation; they face the wage and employment competition but do not participate in proportionality-standard debates that shape what restrictions are permissible. Sovereignty-primary member-state constituencies (labor unions, welfare-system advocates) contest the reading itself but from a delegitimized position—their objections are framed as 'protectionism' rather than legitimate interests. Small member states with constrained welfare systems lack the institutional capacity to challenge court-imposed standards.
% DISAPPEARANCE_RATIONALE: If the subsidiarity balance disappeared, labor mobility would either collapse entirely (if the sovereignty-primary reading fully prevailed) or become unconditional (if the integration-primary reading prevailed). Either path would destabilize the federation: collapse would fragment the single market and trigger exit crises in mobility-dependent economies; unconditional movement would trigger welfare-system and labor-market crises in generous, geographically proximate member states. The equilibrium depends on the balance's continued enforcement.
% FOUNDING_PROBLEM: Early federation membership combined a commitment to free movement with member states' sovereign control of welfare systems and labor markets. The founding problem was: how can a federation integrate labor markets without either imposing a single welfare standard (politically impossible across states with different generosity levels and funding capacity) or collapsing mobility rights (economically destructive to the federation project)?
% FOUNDING_PROBLEM_CORROBORATION: Federation institutional architects (European Commission founding texts, Maastricht Treaty preamble) attest the founding problem was real and urgent. The European Court of Justice's case law (Carpenter, Baumbast, Chen lines) attests the subsidiarity balance is an active solution, not a settled principle. Member-state governments contest the balance's adequacy, arguing the founding problem has shifted—welfare-system strain and labor-market disruption in receiving states now demand stricter restrictions. Critical scholarship (Menéndez, Schiek) outside the benefiting-parties coalition corroborates that the balance is actively maintained and repeatedly contested, not a natural equilibrium.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness has risen from 0.38 (1992, early federation period, movement rights nascent and welfare systems absorbing costs without acute pressure) to 0.58 (2024, mature period with substantial cross-border labor flows and sustained welfare-system strain). The rise reflects: (1) East European enlargement (2004+) increased the skill and wage differential attracting mobility, raising extraction from low-skill local labor; (2) service-provider integration deepened regulatory arbitrage opportunities; (3) welfare-system strain in receiving states (aging, fiscal pressure) made absorption costs more salient. The slight decline from 0.59 (2020) to 0.58 (2024) reflects post-COVID labor-market rebalancing and increased enforcement of integration conditions (language, skills certifications), which slightly constrain unrestricted mobility. Theater ratio has risen from 0.22 to 0.41, indicating increasing proportion of proportionality-standard litigation and compliance activity relative to actual labor-market integration work. This signals the constraint is shifting from a coordination function (integrating markets) toward a governance function (managing litigation and compliance). Suppression has held moderate and stable—neither collapsing entirely (unrestricted movement is actively policed, low-skill protections are available) nor becoming total (blanket restrictions are struck down as disproportionate). The moderate suppression reflects the subsidiarity framework's design: both unrestricted movement AND blanket restrictions are suppressed; only proportionate restrictions are permitted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mobile workers and service providers, the constraint is a coordination mechanism that solved an integration problem—they can move freely without facing blanket barriers. From the perspective of stationary workers and welfare systems, the constraint is enforced extraction—mobility rights are protected at their expense, and the proportionality standard is applied asymmetrically (proportionality-testing of member-state restrictions is strict; proportionality-testing of extraction is weak). From the court's perspective, the constraint is a neutral proportionality arbiter. From member-state governments' perspective, the constraint is a federation override of their sovereign authority—they set restrictions that are then invalidated by proportional review. The engine should compute these as distinct per-seat classifications: the beneficiary seats (mobile labor, service providers, federation institutional legitimacy) should compute as rope or coordination; the payer seats (stationary labor, welfare systems) should compute as snare or tangled rope with extraction dominating; the agenda-setter seat (court) should compute as institutional coordination but with reduced beneficiary weight.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies dramatically by power level. Mobile high-skill workers (moderate power, mobile exit): d ≈ 0.15 (beneficiary end)—they benefit substantially from movement rights and face low costs. Service providers (organized power, arbitrage exit): d ≈ 0.2—they benefit from regulatory arbitrage and have strong exit options (can relocate service centers). Stationary low-skill labor (powerless, trapped exit): d ≈ 0.85 (target end)—they face wage competition, cannot exit geographically or skill-wise, and bear the cost of mobility without the benefit. Local welfare systems (institutional power, constrained exit): d ≈ 0.72—they bear substantial access costs and cannot exclude mobile workers through means-testing without proportionality challenge; their exit options are limited to federation-level negotiation (low success rate) or service retrenchment (politically costly). Territorial regulators (institutional power, constrained exit): d ≈ 0.65—they face compliance burden and cannot defend local protections without proportionality justification, but retain some rule-setting authority. Court and federation institutions: d ≈ 0.4 (near-symmetric, slight beneficiary lean)—they administer and update the rule, moderately constrained by federation law but not directly paying. The core asymmetry: those with resources to move benefit; those without absorb costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The subsidiarity balance appears to be a live constraint (founding_problem_status=contested, not dead), so mandatrophy is not the primary issue. However, the theater-ratio rise (0.22→0.41) signals drift toward performative compliance: proportionality testing has become highly formalized and litigation-driven, and the actual labor-market integration work (the founding substantive coordination) is now shadowed by compliance theater. This is a warning sign of constraint degradation—the framework's binding function is eroding as it becomes a compliance checklist rather than a genuine proportionality arbiter. The slight extractiveness decline (0.59→0.58) at the end of the interval suggests member states may be successfully pushing back (through increased integration requirements, language conditions, skills certifications), which slightly reduces the constraint's asymmetry but does not resolve the underlying cost-burden distribution. If theater ratio continues to rise and extractiveness remains high, the constraint may drift toward piton (maintained primarily through institutional habit and litigation, not genuine coordination or proportionate protection).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_standard_drift,
    'Is the European Court of Justice''s proportionality standard itself drifting toward integration-primary (stricter scrutiny of member-state restrictions) or toward sovereignty-primary (deference to member-state judgment)?',
    'Content analysis of ECJ case law (Metock, Dano, Alimanovic lines) over decade-long periods; tracking the ratio of restrictions struck down vs. upheld; interviewing ECJ judges and advocates-general about their standard of review.',
    'Drift toward integration-primary would tip the effective constraint toward unconditional movement and away from legitimate-interest protection (the balance would degrade toward integration-primary reading). Drift toward sovereignty-primary would permit more member-state restrictions and move the balance toward piton (a formal subsidiarity framework that increasingly permits member-state carve-outs in practice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_standard_drift, empirical, 'Direction of doctrine drift in proportionality-standard application').

omega_variable(
    welfare_system_strain_threshold,
    'At what level of welfare-system strain (measured by fiscal burden, service quality decline, or political backlash) do proportionality-permitted restrictions become insufficient and member states abandon the balance entirely?',
    'Monitoring member-state welfare-system statistics (spending, coverage, recipient demographics); tracking member-state legislative proposals for new restrictions; observing member-state defiance or threat of defiance against ECJ rulings.',
    'If the threshold is crossed (e.g., a major receiving state experiences visible welfare-system collapse or triggers sustained political backlash), the subsidiarity balance may fail—member states may impose blanket restrictions regardless of proportionality doctrine, triggering federation crisis. This would test whether the constraint is enforced coercively or consensually.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_system_strain_threshold, empirical, 'Sustainability of member-state compliance with proportionality-constrained restrictions').

omega_variable(
    integration_vs_sovereignty_reading_distinction,
    'Is the subsidiarity balance genuinely a third position that resolves the founding problem, or is it a disguised sovereignty-primary reading that uses proportionality language to appear balanced?',
    'Comparative textual analysis of integration-primary, sovereignty-primary, and subsidiarity-balance policy documents and case law; assessment of whether proportionality doctrine systematically favors either pole in practice (asymmetric application would suggest the balance is not genuinely triadic).',
    'If the balance is revealed as a disguised sovereignty-primary reading, then the founding problem is not actually solved—it is suppressed by institutional narrative. This would reclassify the constraint from tangled_rope (genuine coordination with extraction) toward snare (pure extraction with a coordination cover story). If the balance is revealed as disguised integration-primary, then member-state interests are systematically not protected despite the frame—this would also suggest snare dynamics (extraction protected by integration-primary ideology).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_reading_distinction, conceptual, 'Whether the subsidiarity balance is a genuine third position or a disguised reading of one of the sibling readings').

omega_variable(
    stationary_labor_coalition_power,
    'Why has stationary low-skill labor failed to organize a collective challenge to the subsidiarity balance despite bearing concentrated costs?',
    'Political-economy analysis of labor union organization in receiving member states; interviews with labor representatives about their strategic choices; tracking of legislative/litigation initiatives on behalf of stationary workers.',
    'If stationary labor remains unorganized, the constraint persists because the cost-bearing group lacks political voice to challenge it—this points toward snare dynamics (extraction without organized resistance). If stationary labor begins organizing collective action (transnational unions, political coalitions), the constraint faces legitimacy pressure and may evolve toward tighter proportionality bounds or member-state carve-outs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationary_labor_coalition_power, empirical, 'Organizational capacity of cost-bearing stationary labor groups').

omega_variable(
    reading_distinction_in_cs_kernel,
    'In the commitment-system framing, which reading (integration-primary, sovereignty-primary, or subsidiarity-balance) grounds the federation''s authority structure''s legitimacy?',
    'Textual analysis of Lisbon Treaty, CJEU preamble, founding treaties; assessment of which reading the authority structure treats as its foundational legitimacy claim (e.g., does the ECJ justify its authority through integration-primary, subsidiarity-balance, or sovereignty-primary axioms?).',
    'If the authority structure (ECJ, Commission) grounds itself primarily in integration-primary, then the subsidiarity-balance reading is an institutional accommodation, not the true authority foundation—this would suggest the constraint is drifting toward integration-primary regardless of formal language. If sovereignty-primary, then subsidiarity-balance is a constraint imposed by member states against institutional preference. If subsidiarity-balance itself is the grounding axiom, then the reading is genuinely constitutive of the federation''s legitimacy—more stable but more contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_distinction_in_cs_kernel, conceptual, 'Which reading grounds the authority structure''s legitimacy claim in the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_treaty__subsidiarity_balance, theater_ratio, 1992, 0.22).
narrative_ontology:measurement(fede_tr_t2001, federation_membership_treaty__subsidiarity_balance, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_treaty__subsidiarity_balance, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(fede_tr_t2015, federation_membership_treaty__subsidiarity_balance, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_treaty__subsidiarity_balance, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_treaty__subsidiarity_balance, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 1992, 0.38).
narrative_ontology:measurement(fede_be_t2001, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 2001, 0.44).
narrative_ontology:measurement(fede_be_t2008, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 2008, 0.51).
narrative_ontology:measurement(fede_be_t2015, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement(fede_be_t2020, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement(fede_be_t2024, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(fede_su_t2001, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 2001, 0.41).
narrative_ontology:measurement(fede_su_t2008, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement(fede_su_t2015, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(fede_su_t2020, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(fede_su_t2024, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__subsidiarity_balance, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% The federation_membership_treaty kernel decomposes into three structurally distinct constraint stories with different epsilon values and beneficiary/victim structures: (1) integration_primary reading (Mountain or Rope, low ε, treats free movement as constitutive); (2) sovereignty_primary reading (Tangled Rope or Snare, high ε, treats free movement as conditional and member-state-constrained); (3) subsidiarity_balance reading (this file, Tangled Rope, moderate ε, treats free movement as a right bounded by proportionality-tested member-state interests). The three readings are sibling instantiations of the same contested kernel, not competing interpretations of a single constraint. Each reading has its own beneficiary set, victim set, and ε value, reflecting the reading's own structural commitments, not observer-dependent measurement. The epsilon_invariance principle applies per reading: within this reading's framework, ε=0.58 is invariant across measurement observables. A different reading's ε value reflects that reading's different structural commitments about what counts as extraction, not a different measurement of the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
