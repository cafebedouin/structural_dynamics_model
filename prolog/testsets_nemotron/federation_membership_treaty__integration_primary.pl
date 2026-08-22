% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
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
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the integration_primary reading of the
 *   contested kernel federation_membership_treaty. The reading holds that
 *   free movement of workers is constitutive of the single market — not a
 *   conditional privilege but a foundational principle — such that national
 *   restrictions are presumptively illegitimate unless narrowly justified by
 *   overriding public interest and proportionality. The constraint operates
 *   through supranational judicial enforcement (ECJ) that invalidates
 *   national measures restricting mobility, creating a structural asymmetry:
 *   mobile workers and cross-border employers benefit from guaranteed access,
 *   while national welfare systems and local labor markets bear the
 *   distributional costs without equivalent exit options. The constraint has
 *   hardened over four decades: early treaty language was permissive;
 *   jurisprudential expansion and legislative deepening have progressively
 *   narrowed the space for national derogation, raising both extraction and
 *   suppression. The sibling readings — sovereignty_primary (states retain
 *   authority to protect national labor markets/welfare) and
 *   subsidiarity_balance (proportionality bounds mobility rights) —
 *   instantiate distinct constraints with different beneficiary/victim
 *   structures and suppression profiles, linked via affects_constraints.
 *
 * KEY AGENTS:
 *   - mobile_workers: Primary beneficiary (organized/constrained) — gains guaranteed access to cross-border labor markets
 *   - cross_border_employers: Primary beneficiary (powerful/arbitrage) — gains unfettered access to labor pools across the federation
 *   - integrationist_institutions: Agenda setter (institutional/generational) — ECJ, Commission, Parliament; administers and expands the constraint
 *   - local_labor_markets: Primary victim (moderate/trapped) — bears wage pressure and displacement without political voice in supranational arena
 *   - national_welfare_systems: Primary victim (institutional/constrained) — bears fiscal externalities from mobile populations without corresponding revenue authority
 *   - displaced_domestic_workers: Primary victim (powerless/trapped) — bears concentrated costs of labor market adjustment with minimal exit
 *   - national_governments: Dual role (institutional/constrained) — both constrained by the constraint and administrators of its domestic implementation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.62).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.78).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive of the Single Market (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, 'a8b400f4-008a-444a-97a6-5b7a492a8789').
narrative_ontology:cs_kernel_codification('a8b400f4-008a-444a-97a6-5b7a492a8789', formalized).
narrative_ontology:cs_authority_grounding('a8b400f4-008a-444a-97a6-5b7a492a8789', lineage).
narrative_ontology:cs_interpretation_layer_present('a8b400f4-008a-444a-97a6-5b7a492a8789').
narrative_ontology:cs_reading_relation('a8b400f4-008a-444a-97a6-5b7a492a8789', federation_membership_treaty__sovereignty_primary, influences).
narrative_ontology:cs_reading_relation('a8b400f4-008a-444a-97a6-5b7a492a8789', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('a8b400f4-008a-444a-97a6-5b7a492a8789', foundational, free_movement_constitutive_principle).
narrative_ontology:cs_axiom_status(free_movement_constitutive_principle, holdable).
narrative_ontology:cs_axiom_grounding('a8b400f4-008a-444a-97a6-5b7a492a8789', free_movement_constitutive_principle, conventional).
narrative_ontology:cs_axiom('a8b400f4-008a-444a-97a6-5b7a492a8789', foundational, national_restrictions_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(national_restrictions_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('a8b400f4-008a-444a-97a6-5b7a492a8789', national_restrictions_presumptively_illegitimate, conventional).
narrative_ontology:cs_reference_frame('a8b400f4-008a-444a-97a6-5b7a492a8789', treaty_of_rome_mobility_mandate).
narrative_ontology:cs_drift_state('a8b400f4-008a-444a-97a6-5b7a492a8789', post_eastern_enlargement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a8b400f4-008a-444a-97a6-5b7a492a8789', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, integrationist_institutions).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, displaced_domestic_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_governments).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, single_market_constitutive_principle).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, integration_precedence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers who exercise cross-border mobility for employment. They gain guaranteed access to labor markets across the federation, portability of social security rights, and protection against nationality-based discrimination. Their exit is constrained: returning to origin country labor markets may mean wage loss, skill depreciation, or family disruption. They are organized through EURES, trade union federations, and EU citizenship rights advocacy networks.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    organized, biographical, constrained, continental).

% Firms operating across multiple member states (logistics, construction, healthcare, tech, platform economy). They gain unfettered access to a continental labor pool, wage arbitrage opportunities, and regulatory harmonization that reduces compliance costs. Their exit is arbitrage-grade: they can relocate operations, restructure supply chains, or shift to capital-intensive models if mobility rights were restricted. They lobby through BusinessEurope, sectoral federations, and national chambers of commerce.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cross_border_employers, beneficiary,
    powerful, biographical, arbitrage, continental).

% The European Court of Justice, European Commission, and European Parliament — the supranational actors that administer, interpret, and expand the free movement mandate. They set the agenda through jurisprudence (ECJ), infringement procedures (Commission), and legislative initiative (Parliament). Their authority and institutional identity are bound to the constraint's expansion. Exit is analytical: they observe the constraint from the seat that defines it.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, integrationist_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Subnational labor markets (regions, cities, sectors) that absorb the adjustment costs of mobile labor inflows: wage compression in low-skill segments, displacement of domestic workers, housing pressure, infrastructure strain. They have no supranational political voice; their representation is mediated through national governments that are themselves constrained by the treaty. Exit is trapped: a region cannot opt out of the single market's labor mobility effects without the member state leaving the federation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    moderate, generational, trapped, regional).

% National social security, healthcare, unemployment insurance, and pension systems that bear fiscal externalities from mobile populations (contribution gaps, benefit tourism, coordination costs) without corresponding revenue authority or control over eligibility. They retain some policy tools (habitual residence tests, waiting periods) but these are narrowly constrained by ECJ jurisprudence. Exit is constrained: a member state could theoretically withdraw from coordination regulations, but this would trigger systemic legal and political consequences.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Workers in exposed sectors (construction, agriculture, hospitality, care, logistics) who face direct wage competition and displacement from mobile labor. They bear concentrated costs with minimal political voice — neither organized at supranational level nor effectively represented in national politics due to sectoral fragmentation and low union density. Exit is trapped: retraining is costly, geographic mobility is limited by family/housing ties, and sectoral exit means abandoning accumulated skills.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, displaced_domestic_workers, payer,
    powerless, biographical, trapped, local).

% Member state governments in the Council of Ministers — they co-legislate the constraint's scope, implement its domestic transposition, and bear political accountability for its effects. They are both agenda-setters (shaping the constraint through intergovernmental bargaining) and payers (absorbing fiscal costs, managing political backlash from displaced workers and stressed welfare systems). Exit is constrained: treaty withdrawal (Article 50) is legally possible but politically and economically prohibitive.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, national_governments, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of fragmented national labor markets: without a binding mobility mandate, each member state would restrict inflows to protect domestic workers and welfare systems, preventing the factor mobility necessary for a genuine single market. The constraint coordinates by making mobility a right enforceable against states, not a privilege granted by them.
% TRANSFER_FUNCTION: Moves labor market access and wage arbitrage gains from national welfare systems and local labor markets to mobile workers and cross-border employers. National systems bear fiscal externalities (unfunded healthcare, education, social assistance for mobile populations) and local markets bear wage/displacement costs; mobile workers gain access to higher-wage markets; cross-border employers gain access to cheaper/more flexible labor pools.
% ABSENT_VOICES: Third-country nationals excluded from EU citizenship rights but present in the labor market; posted workers in precarious conditions who cannot enforce rights; future generations who inherit the fiscal sustainability consequences of current mobility patterns; regions experiencing demographic decline accelerated by out-mobility of working-age populations.
% DISAPPEARANCE_RATIONALE: If the integration-primary free movement mandate vanished overnight, member states would immediately reimpose national labor market protections (quotas, labor market tests, nationality preferences), welfare systems would restrict access to contributors only, cross-border employers would face fragmented hiring regimes, and mobile workers would lose enforceable rights — the single market would revert to a patchwork of national labor markets with bilateral agreements.
% FOUNDING_PROBLEM: Post-war European integration required removing barriers to factor mobility to create a genuine single market. National labor market protections and welfare chauvinism were identified as the primary obstacles to economic integration and convergence.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and ECJ attest the problem remains live (ongoing barriers, incomplete integration). National governments, trade unions, and OECD attest the core coordination problem is substantially solved (barriers removed, mobility realized) and the constraint now primarily redistributes. Independent economic analysis (e.g., European Economic Advisory Group reports) supports the shifted-function reading from outside the benefiting parties.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects the constraint's redistributive pressure: mobile workers and cross-border employers capture gains from frictionless mobility while national welfare systems and local labor markets absorb costs (fiscal externalities, wage compression, displacement) without equivalent voice or exit. Suppression (0.78) is high because the constraint's persistence depends on active supranational enforcement — ECJ jurisprudence, Commission infringement procedures, legislative harmonization — that systematically invalidates national restrictions. The theater ratio (0.22) is low-moderate: the coordination function (single market integrity) is genuine, but a growing share of enforcement activity defends the mobility mandate against national resistance rather than building market infrastructure. Accessibility collapse (0.48) and resistance (0.55) reflect the partial but real alternatives that persist (transitional arrangements, posted worker directives, sectoral carve-outs) and the sustained political resistance from member states. The measurement series runs on a shared 40-year grid (0-40, roughly 1985-2025) with 5 time points per metric, showing extraction accumulation and enforcement intensification.
 *
 * PERSPECTIVAL GAP:
 *   From the integrationist_institutions seat, the constraint is a rope: genuine coordination solving a collective action problem (fragmented national markets) with net benefits. From the local_labor_markets and displaced_domestic_workers seats, the same structure computes as snare: pure extraction with no coordination benefit, sustained by coercion (ECJ rulings) and suppressed exits. From national_governments and national_welfare_systems seats, it computes as tangled_rope: real coordination function (single market) with asymmetric extraction (fiscal/wage externalities) requiring active enforcement. The engine computes this per-seat divergence from the structural data authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   The integrationist institutions (ECJ, Commission) are structural beneficiaries: they administer the constraint, gain authority from its expansion, and face near-zero exit cost (d ~ 0.1). Mobile workers and cross-border employers are direct beneficiaries with constrained exit — they gain from the constraint but depend on it for their cross-border position (d ~ 0.2-0.3). National governments are dual-positioned: as agenda-setters in the Council they shape the constraint, but as implementers they bear enforcement costs and political backlash (d ~ 0.5). Local labor markets and displaced domestic workers are full targets with trapped exit — they bear concentrated costs, have no supranational voice, and cannot exit the federation's labor market effects (d ~ 0.85-0.95). National welfare systems are institutional targets with constrained exit — they bear fiscal externalities without revenue authority, but retain some policy tools (d ~ 0.7). The engine derives these directionalities from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — creating a single market by removing barriers to factor mobility — remains live but has shifted: the original coordination problem (transaction costs of national barriers) has been largely solved, while the extraction function (redistributive pressure on national welfare systems and local labor markets) has grown. The constraint now persists partly through institutional inertia (the EU's identity is bound to free movement) and partly because the beneficiary coalition (mobile workers, cross-border employers, integrationist institutions) is organized and the victim coalition is fragmented across 27 national political arenas. This is not pure mandatrophy — the coordination function remains — but the extraction/coordination ratio has drifted toward extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_integration,
    'Is the integration-primary reading a genuine structural feature of the treaty order, or a constructed constraint that benefits identifiable agents?',
    'Comparative analysis of treaty drafting history vs. subsequent jurisprudential expansion; empirical test of whether mobility restrictions trigger systemic market failure or merely redistributive pressure.',
    'If constructed with identifiable beneficiaries, FSM triggers reclassification toward tangled_rope/snare; if natural-law emergent, mountain classification holds for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_integration, conceptual, 'Whether the integration-primary reading reflects an irreducible treaty principle or a policy choice that creates winners and losers').

omega_variable(
    kernel_reading_committer_structure,
    'How does this reading''s structural relationship to the federation_membership_treaty kernel differ from its sibling readings?',
    'Structural comparison of beneficiary/victim sets, suppression profiles, and directionality across the three readings; the engine''s per-seat classification divergence maps the committer frame''s dispute.',
    'Clarifies whether the kernel is a single constraint with contested observables (violating ε-invariance) or a constraint family of three distinct constraints linked by affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'This constraint is the integration_primary reading of kernel federation_membership_treaty; siblings are sovereignty_primary and subsidiarity_balance. This reading places mobile workers as beneficiaries and national welfare systems as victims with high suppression of national restrictions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, enforcement machinery) or internalized (normative acceptance by national authorities)?',
    'Post-restriction suppression trajectory: if national restrictions persist after ECJ invalidation or legislative override, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint carries its own enforcement into the normative commitments of the constrained parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in federal migration governance').

omega_variable(
    coordination_extraction_separability,
    'Is the single market''s coordination function (factor mobility, regulatory harmonization) separable from the extraction function (redistributive pressure on welfare systems, wage compression in local markets)?',
    'Natural experiment from partial liberalization episodes (e.g., transitional arrangements, sectoral carve-outs): if market integration proceeds without full labor mobility, functions are separable.',
    'If separable, the mobility mandate''s extraction component is not the price of coordination but an independent distributional choice; if inseparable, measured extraction is the necessary cost of the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable in federal labor market integration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmt_integration_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(fmt_integration_tr_t0, observed).
narrative_ontology:measurement(fmt_integration_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(fmt_integration_tr_t10, observed).
narrative_ontology:measurement(fmt_integration_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(fmt_integration_tr_t20, observed).
narrative_ontology:measurement(fmt_integration_tr_t30, federation_membership_treaty__integration_primary, theater_ratio, 30, 0.19).
narrative_ontology:measurement_basis(fmt_integration_tr_t30, observed).
narrative_ontology:measurement(fmt_integration_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(fmt_integration_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fmt_integration_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(fmt_integration_be_t0, observed).
narrative_ontology:measurement(fmt_integration_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(fmt_integration_be_t10, observed).
narrative_ontology:measurement(fmt_integration_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(fmt_integration_be_t20, observed).
narrative_ontology:measurement(fmt_integration_be_t30, federation_membership_treaty__integration_primary, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(fmt_integration_be_t30, observed).
narrative_ontology:measurement(fmt_integration_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(fmt_integration_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fmt_integration_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(fmt_integration_su_t0, observed).
narrative_ontology:measurement(fmt_integration_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(fmt_integration_su_t10, observed).
narrative_ontology:measurement(fmt_integration_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(fmt_integration_su_t20, observed).
narrative_ontology:measurement(fmt_integration_su_t30, federation_membership_treaty__integration_primary, suppression_requirement, 30, 0.75).
narrative_ontology:measurement_basis(fmt_integration_su_t30, observed).
narrative_ontology:measurement(fmt_integration_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(fmt_integration_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% The federation_membership_treaty kernel decomposes into three constraint stories: integration_primary (this story), sovereignty_primary, and subsidiarity_balance. Each has distinct ε values, beneficiary/victim structures, and suppression profiles. This reading claims the single market's constitutive principle as paramount; sovereignty_primary claims member state authority as paramount; subsidiarity_balance claims proportionality as the mediating principle. They are linked via affects_constraints because the integration_primary reading's jurisprudential expansion structurally pressures the sovereign and subsidiarity readings' operating space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, powerful, 0.25).
constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, moderate, 0.5).
constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
