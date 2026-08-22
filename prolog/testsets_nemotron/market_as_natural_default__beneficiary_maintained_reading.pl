% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Naturalization as Engineered Closure
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint story instantiates the 'beneficiary_maintained_reading'
 *   of the contested kernel 'market_as_natural_default.' It treats the claim
 *   that 'markets are the natural, default form of human economic
 *   organization' not as a discovery but as an engineered closure — a
 *   narrative actively constructed and defended by identifiable beneficiary
 *   classes (finance, corporate capital, their intellectual infrastructure)
 *   to justify extraction. The constraint operates as a tangled rope: it
 *   coordinates capital allocation globally (genuine coordination function)
 *   while simultaneously extracting from labor, public alternatives, and
 *   local economies through the same structure (asymmetric extraction),
 *   requiring active enforcement via intellectual suppression, legal
 *   architecture, and crisis management. The sibling readings —
 *   lapsed_alternative_reading (forgetting) and hybrid_amnesia_reading
 *   (forgetting enabling capture) — are separate constraint stories linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - financial_sector_incumbents: Primary beneficiary (institutional/arbitrage) — collects rents from financialization
 *   - corporate_capital_holders: Primary beneficiary/agenda_setter (institutional/arbitrage) — sets policy frame, extracts via labor discipline
 *   - neoliberal_think_tank_network: Agenda setter (organized/mobile) — produces and polices the naturalization narrative
 *   - labor_market_participants: Primary payer (moderate/constrained) — bears wage stagnation, insecurity, narrativized failure
 *   - public_sector_alternative_advocates: Excluded (moderate/identity_locked) — advocate decommodification, professionally marginalized
 *   - local_economies_exposed_to_financialization: Payer (powerless/trapped) — bears extraction with no exit
 *   - heterodox_economists: Observer (moderate/mobile) — documents contingency, excluded from agenda-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization as Engineered Closure").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, 'c41191f8-19e8-4e9b-be5d-0e6846a1ad40').
narrative_ontology:cs_kernel_codification('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', distributed).
narrative_ontology:cs_authority_grounding('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', extraction).
narrative_ontology:cs_interpretation_layer_present('c41191f8-19e8-4e9b-be5d-0e6846a1ad40').
narrative_ontology:cs_reading_relation('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', foundational, market_naturalization_is_engineered_closure).
narrative_ontology:cs_axiom_status(market_naturalization_is_engineered_closure, holdable).
narrative_ontology:cs_axiom_grounding('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', market_naturalization_is_engineered_closure, empirically_contingent).
narrative_ontology:cs_axiom('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', foundational, beneficiary_classes_actively_defend_naturalization).
narrative_ontology:cs_axiom_status(beneficiary_classes_actively_defend_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', beneficiary_classes_actively_defend_naturalization, empirically_contingent).
narrative_ontology:cs_reference_frame('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', postwar_embedded_liberalism).
narrative_ontology:cs_drift_state('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', neoliberal_hegemony_peak, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c41191f8-19e8-4e9b-be5d-0e6846a1ad40', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, corporate_capital_holders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, neoliberal_think_tank_network).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, labor_market_participants).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, public_sector_alternative_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, local_economies_exposed_to_financialization).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, there_is_no_alternative_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Capture regulatory and narrative infrastructure that treats market allocation as the default efficient mechanism. Extract rents through financialization of housing, healthcare, education, and public services. Their position is secured by revolving-door personnel flows, campaign finance, and ownership of economic discourse platforms. Exit means shifting asset classes, not leaving the system.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Leverage 'market discipline' narratives to justify labor flexibility, regulatory rollback, and privatization. Fund think tanks and media that naturalize market mechanisms. Their agenda-setting operates through trade associations, business roundtables, and ownership of opinion-forming media. Can relocate capital globally; not trapped by any single jurisdiction.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, corporate_capital_holders, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, corporate_capital_holders, agenda_setter).

% Produce and disseminate the intellectual architecture that presents market allocation as natural law rather than policy choice. Funded by beneficiary classes above; their career advancement depends on maintaining the naturalization frame. Individual scholars can exit to academia or journalism, but the network's institutional logic selects for frame maintenance.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, neoliberal_think_tank_network, agenda_setter,
    organized, generational, mobile, global).

% Experience market naturalization as wage stagnation, benefit erosion, gigification, and the framing of their insecurity as personal failure rather than structural arrangement. Collective bargaining is treated as market distortion; exit from the labor market means destitution. Organized labor has been legally and narratively suppressed.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, labor_market_participants, payer,
    moderate, biographical, constrained, national).

% Advocate for decommodified housing, public banking, universal services, and democratic economic planning. Their proposals are treated as utopian or economically illiterate by the dominant discourse. Professional credibility in economics and policy often requires concession to market-natural frames. Exit from the field means losing voice entirely.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, public_sector_alternative_advocates, excluded,
    moderate, biographical, identity_locked, national).

% Face extraction through predatory lending, speculative housing markets, privatization of utilities, and austerity justified by 'market confidence.' No meaningful exit — geographic mobility is limited, and the financial logic follows them. Their immiseration is presented as market efficiency.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, local_economies_exposed_to_financialization, payer,
    powerless, generational, trapped, local).

% Document the historical contingency of market institutions, the role of state action in creating markets, and the empirical failure of naturalization claims. Marginalized in top departments and journals; their work is cited by advocates but excluded from central bank and treasury advisory circuits. Can publish and teach, but not set the agenda.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_economists, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared cognitive framework that reduces transaction costs for capital allocation across borders — a common language of 'efficiency' that lets financial actors coordinate without negotiating each institutional arrangement.
% TRANSFER_FUNCTION: Moves political legitimacy and policy space from democratic deliberation to technocratic 'market discipline'; moves economic rents from wage earners and public assets to financial asset holders; moves risk from capital to labor and local communities.
% ABSENT_VOICES: Workers in the Global South whose labor markets were restructured by structural adjustment; Indigenous economies managing commons without market allocation; mutual aid networks operating outside price mechanisms; future generations who inherit the ecological costs of market-naturalized extraction. They are absent because the frame defines them as pre-market or outside-the-economy.
% DISAPPEARANCE_RATIONALE: If the naturalization frame collapsed overnight, financial regulation would be re-politicized, housing and healthcare would be contested as rights not assets, austerity would lose its 'market confidence' justification, and the think tank network would lose its core product. The material arrangements (property law, contract enforcement, central banking) would persist but their legitimacy would require active democratic renewal.
% FOUNDING_PROBLEM: Post-1970s stagflation created a crisis of Keynesian demand-management; capital needed a framework to restore profitability and discipline labor. The 'market as natural default' narrative solved this by reframing distributional conflict as technical efficiency, making rollback of the postwar settlement appear as returning to nature rather than political choice.
% FOUNDING_PROBLEM_CORROBORATION: The original crisis (1970s profit squeeze) is historically documented by non-beneficiary sources: Glyn & Sutcliffe (1972), Brenner (2006), and the BIS's own retrospective analyses. The arrangement persists decades after the founding conditions vanished. Beneficiary-class sources (Friedman, Hayek, Mont Pelerin records) confirm the intentional construction of the narrative; no independent corroboration supports its continued necessity.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).
:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects that the naturalization frame enables trillions in financial rents, privatization proceeds, and labor share decline — but some coordination value exists (capital does need interoperable allocation frameworks). Suppression (0.62) is high because alternatives are actively marginalized: heterodox economics purged from central banking, public banking proposals treated as unserious, austerity enforced via 'market confidence' threats. Theater ratio (0.48) captures that the efficiency discourse performs coordination while increasingly serving extraction — the 2008 crisis response (bailouts for finance, austerity for publics) revealed the frame's performative core. Accessibility collapse (0.38) is moderate: alternatives exist and are practiced (Mondragon, Kerala, public banking in Germany/Brazil, indigenous commons) but are cognitively inaccessible within the dominant frame. Resistance (0.58) is significant: labor movements, alter-globalization, municipalism, and post-2008 resistance (Occupy, Sanders, Corbyn, MMT) demonstrate the frame is contested, not hegemonic.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (finance, corporate capital, think tanks), the constraint appears as rope — a genuine coordination solution they built and maintain. From the payer seats (labor, local economies), it appears as snare — extraction enforced by narrative and law. From the excluded seat (public alternative advocates), it appears as active suppression of live alternatives. From the observer seat (heterodox economists), it appears as a historically contingent arrangement presented as natural law. The engine computes these per-seat divergences from the structural data; this commentary documents the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (finance, corporate capital, think tanks) are declared in base_properties.beneficiaries — they collect rents, set agendas, and have arbitrage/mobile exit (d near 0). Victims (labor, local economies, public advocates) are declared in base_properties.victims — they bear costs, have constrained/trapped/identity_locked exit (d near 1). The think tank network is both agenda_setter and beneficiary: they produce the frame and their funding/careers depend on it. Public advocates are excluded, not victims per se — they are kept out of the conversation, not directly extracted from (though their exclusion enables extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s profit crisis) is dead — documented by non-beneficiary sources. The arrangement persists as extraction machinery. The coordination function (capital allocation interoperability) is real but has been captured: the frame now serves to block democratic reallocation of capital, not to solve coordination. This is classic mandatrophy — the mandate (solve stagflation via market discipline) expired; the apparatus (naturalization narrative, think tanks, legal architecture) remains and extracts. The engine's mandatrophy detection should flag: founding_problem_status=dead + disappearance_verdict=world_rearranges = zombie/capture flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'How much of the ''market efficiency'' discourse''s coordination value is genuine vs. a cover for extraction? Where is the structural boundary?',
    'Counterfactual: if the naturalization frame were removed but property/contract/central banking remained, would capital allocation fail or would democratic reallocation emerge? Compare jurisdictions with stronger/weaker naturalization frames (Nordic vs. Anglo-Saxon models).',
    'If coordination value is high and separable, the constraint is more rope-like; if extraction dominates and coordination is a cover, it is more snare-like. The tangled_rope claim rests on both being substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable and their relative magnitudes.').

omega_variable(
    suppression_mechanism_composition,
    'Is the suppression of alternatives primarily structural (funding, career incentives, media ownership) or internalized (economists genuinely believe alternatives are impossible)?',
    'Survey heterodox economists on career pressures vs. genuine conviction; track citation networks after cadre retirement; analyze foundation grant databases for topic steering.',
    'If internalized, suppression is deeper and more resilient — the constraint has colonized the cognitive infrastructure. If primarily structural, removal of funding/incentives could rapidly open the discourse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural vs. internalized suppression mechanism in the intellectual field.').

omega_variable(
    kernel_reading_boundary,
    'Does the beneficiary_maintained_reading foreclose the lapsed_alternative_reading, or do they coexist as competing explanations for different historical phases?',
    'Test whether a single framework can hold: ''alternatives were forgotten (1945-1970) AND then beneficiaries actively suppressed recall (1970-present).'' If yes, readings coexist; if the first phase''s forgetting is structurally necessary for the second''s engineering, this reading forecloses the lapsed reading as a complete account.',
    'If forecloses, the kernel has a logical fork — only one reading can be the primary driver. If coexists_with, the kernel hosts a genuine multi-phase dispute where both mechanisms operated sequentially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between this reading and the lapsed_alternative_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1970, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(mark_tr_t1980, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(mark_tr_t1990, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1990, 0.42).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2000, 0.46).
narrative_ontology:measurement(mark_tr_t2008, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2008, 0.52).
narrative_ontology:measurement(mark_tr_t2015, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2015, 0.49).
narrative_ontology:measurement(mark_tr_t2025, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(mark_be_t1970, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1970, 0.18).
narrative_ontology:measurement(mark_be_t1980, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(mark_be_t1990, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(mark_be_t2008, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(mark_be_t2015, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement(mark_be_t2025, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1970, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(mark_su_t1980, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(mark_su_t1990, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(mark_su_t2000, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(mark_su_t2008, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(mark_su_t2015, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(mark_su_t2025, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__beneficiary_maintained_reading, 0.12).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__hybrid_amnesia_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, neoliberal_austerity_architecture).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, financialization_of_housing).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, public_service_privatization_cascade).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_as_natural_default' kernel. The beneficiary_maintained_reading emphasizes active engineering by identifiable beneficiaries. The lapsed_alternative_reading emphasizes historical forgetting. The hybrid_amnesia_reading synthesizes: forgetting created the opening, beneficiaries captured it. All three share the referent (market naturalization as a social fact) but author different ε, beneficiaries/victims, and structural mechanisms. They form a constraint family linked by mutual affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, organized, 0.15).
constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, moderate, 0.75).
constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
