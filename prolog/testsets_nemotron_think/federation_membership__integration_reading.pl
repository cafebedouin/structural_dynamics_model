% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The integration reading of EU federation membership treats free movement
 *   as a constitutional right that cannot be restricted by member states, and
 *   supranational authority (ECJ, Commission) as the legitimate interpreter
 *   and enforcer of that right. This reading emerged from the ECJ's case law
 *   (1990s onward) transforming Treaty provisions on worker mobility into a
 *   general EU citizenship right. The constraint operates as a tangled rope:
 *   it solves a genuine coordination problem (single labor market, factor
 *   mobility) but extracts asymmetrically — mobile citizens and capital gain,
 *   while local labor markets, displaced workers, and peripheral regions bear
 *   concentrated costs. Border restriction is framed as illegitimate; the
 *   only legitimate politics is deepening integration. The claimed type is
 *   tangled_rope (coordination + extraction); the metrics describe a
 *   constraint whose extraction has accumulated over six decades as the
 *   coordination function was instrumentalized for institutional expansion.
 *
 * KEY AGENTS:
 *   - supranational_institutions: Primary agenda_setter (institutional/generational/analytical) — sets and enforces the integration acquis
 *   - mobile_citizens: Primary beneficiary (organized/biographical/arbitrage) — exercises free movement as constitutional right
 *   - capital_owners: Secondary beneficiary (powerful/biographical/arbitrage) — captures labor market flexibility gains
 *   - local_labor_markets: Primary payer (moderate/generational/constrained) — absorbs wage compression and displacement
 *   - displaced_workers: Primary payer (powerless/biographical/trapped) — bears concentrated costs without mobility offset
 *   - peripheral_regions: Secondary payer (moderate/generational/constrained) — experiences brain drain without policy agency
 *   - national_governments: Dual agenda_setter/payer (institutional/biographical/constrained) — co-legislates but bound by supranational rulings
 *   - third_country_nationals: Excluded (powerless/biographical/trapped) — mobility criminalized while citizens' mobility constitutionalized
 *   - non_mobile_populations: Excluded (moderate/biographical/constrained) — bears diffuse costs without supranational voice
 *   - academic_analysts: Observer (analytical/civilizational/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.72).
domain_priors:suppression_score(federation_membership__integration_reading, 0.68).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Integration Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, 'ae48961d-fc3e-40d9-8563-5b8ee085e85d').
narrative_ontology:cs_kernel_codification('ae48961d-fc3e-40d9-8563-5b8ee085e85d', formalized).
narrative_ontology:cs_authority_grounding('ae48961d-fc3e-40d9-8563-5b8ee085e85d', extraction).
narrative_ontology:cs_interpretation_layer_present('ae48961d-fc3e-40d9-8563-5b8ee085e85d').
narrative_ontology:cs_reading_relation('ae48961d-fc3e-40d9-8563-5b8ee085e85d', federation_membership__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('ae48961d-fc3e-40d9-8563-5b8ee085e85d', foundational, free_movement_as_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_as_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('ae48961d-fc3e-40d9-8563-5b8ee085e85d', free_movement_as_constitutional_right, deontological).
narrative_ontology:cs_axiom('ae48961d-fc3e-40d9-8563-5b8ee085e85d', foundational, irreversible_integration).
narrative_ontology:cs_axiom_status(irreversible_integration, holdable).
narrative_ontology:cs_axiom_grounding('ae48961d-fc3e-40d9-8563-5b8ee085e85d', irreversible_integration, conventional).
narrative_ontology:cs_axiom('ae48961d-fc3e-40d9-8563-5b8ee085e85d', secondary, supranational_supremacy_in_mobility).
narrative_ontology:cs_axiom_status(supranational_supremacy_in_mobility, holdable).
narrative_ontology:cs_axiom_grounding('ae48961d-fc3e-40d9-8563-5b8ee085e85d', supranational_supremacy_in_mobility, conventional).
narrative_ontology:cs_reference_frame('ae48961d-fc3e-40d9-8563-5b8ee085e85d', ever_closer_union).
narrative_ontology:cs_drift_state('ae48961d-fc3e-40d9-8563-5b8ee085e85d', contemporary_populist_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ae48961d-fc3e-40d9-8563-5b8ee085e85d', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_institutions).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, capital_owners).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, displaced_workers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, peripheral_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_governments).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, free_movement_as_constitutional_right).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, irreversible_integration_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, supranational_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European Commission, Court of Justice, and Parliament set and enforce the integration acquis. They interpret treaties as mandating ever-closer union and free movement as a non-derogable right. Their authority and budget expand with integration depth; they face no electoral exit and their legitimacy rests on the integration narrative itself.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% EU citizens who exercise free movement for work, study, or retirement. They gain access to labor markets, public services, and social protections across 27 states. Their exit option is strong — they can move again or return home — and they organize politically (e.g., Erasmus alumni networks, cross-border worker associations) to defend the acquis.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    organized, biographical, arbitrage, continental).

% Employers and investors who benefit from a continent-wide labor pool with reduced bargaining friction. They lobby for deeper market integration and against social dumping safeguards. Their capital is mobile; they can relocate production to lower-cost regions within the single market, giving them arbitrage-grade exit from any national regulatory response.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, capital_owners, beneficiary,
    powerful, biographical, arbitrage, global).

% Regional labor markets in higher-wage member states that absorb incoming workers. Wage compression and displacement effects concentrate in specific sectors (construction, logistics, care, hospitality). Exit is constrained — workers cannot easily leave their region, retraining is costly, and political representation is diluted by supranational rules that treat labor market protection as protectionism.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    moderate, generational, constrained, regional).

% Workers in exposed sectors who face direct competition from mobile labor without equivalent mobility themselves (language barriers, family ties, skill specificity). They bear the concentrated cost of the integration reading's beneficiaries. Exit options are minimal — geographic mobility is the very resource they lack, and political voice is fragmented across national party systems that do not aggregate their interest at the supranational level.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, displaced_workers, payer,
    powerless, biographical, trapped, local).

% Lower-wage member states and regions that experience outmigration of skilled workers (brain drain) and demographic hollowing. They are told free movement is a right, not a policy choice, so they cannot restrict outflow. Cohesion funds partially compensate but do not restore agency. Exit from the federation is treated as illegitimate (Brexit precedent), leaving voice as the only channel — which is structurally weak at the supranational level.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, peripheral_regions, payer,
    moderate, generational, constrained, regional).

% Member state governments in the Council co-legislate but are bound by treaty obligations and ECJ rulings they cannot unilaterally amend. They pay the political cost of integration (domestic backlash, fiscal transfers) while sharing agenda-setting power. Exit (Article 50) is legally possible but politically treated as catastrophe, making their exit option constrained rather than mobile.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, national_governments, payer).

% Non-EU migrants and asylum seekers whose mobility is criminalized while EU citizens' mobility is constitutionalized. They would challenge the distinction between 'free movement' and 'irregular migration' but have no standing in the EU polity. Their exclusion is structural — the integration reading's universalist language stops at the external border.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, third_country_nationals, excluded,
    powerless, biographical, trapped, global).

% Citizens who cannot or do not move (elderly, caregivers, low-skilled, place-attached). They experience the fiscal and social costs of integration (pressure on housing, public services, wage floors) without the offsetting benefits. Their political voice exists nationally but the constraint's key parameters are set supranationally, creating a democratic gap they cannot bridge.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, non_mobile_populations, excluded,
    moderate, biographical, constrained, national).

% Scholars of EU law, political economy, and federalism who analyze the constraint's operation across seats. They see the full structure — the coordination gains, the asymmetric extraction, the legitimacy claims — but do not collect rents or bear costs from the constraint directly.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, academic_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single labor market and civic space across 27 states: workers fill shortages where they arise, firms hire across borders without visa barriers, citizens live and retire where they choose. The coordination problem — matching labor to demand across a diverse continent — is solved by removing the border as a filtering mechanism.
% TRANSFER_FUNCTION: Moves bargaining power and wage-setting capacity from local labor markets and displaced workers to mobile citizens and capital owners. The supranational institutions extract political authority (the competence to define the border's meaning) from national governments. Fiscal costs of integration (cohesion funds, unemployment insurance portability) are socialized across member states while gains concentrate in mobile factors.
% ABSENT_VOICES: Third-country nationals excluded by the external border regime; non-mobile populations who bear diffuse costs without concentrated benefits; peripheral regions experiencing brain drain without a structural voice in the free movement framework. The integration reading treats their situation as a transitional friction rather than a structural feature.
% DISAPPEARANCE_RATIONALE: If the integration reading vanished overnight — if free movement became a negotiable policy rather than a constitutional right — national governments would reintroduce sectoral and quantitative controls within months. Labor markets would resegment; wage compression in exposed sectors would ease; peripheral regions would regain demographic agency but lose remittances and return-migration human capital. The single market would fracture into a patchwork of bilateral agreements. The supranational institutions would lose their core legitimacy anchor.
% FOUNDING_PROBLEM: Post-war Europe needed to bind German industrial capacity into a supranational framework that made war materially impossible. The ECSC and EEC treated free movement of coal, steel, and workers as the economic substrate of peace. The founding problem was preventing Franco-German conflict through irreversible economic interdependence.
% FOUNDING_PROBLEM_CORROBORATION: The Franco-German war prevention motive is attested by the Schuman Declaration (1950), the Treaty of Paris (1951), and the Treaty of Rome (1957) — all authored by the founding governments, not by the supranational institutions that later claimed the integration mantle. Contemporary historians (e.g., Milward, Moravcsik) corroborate that the peace-through-interdependence logic was the founding bargain. The integration reading's current claim — that free movement is a constitutional right owed to every citizen — is a doctrinal extension by the ECJ (e.g., Martinez Sala, Baumbast, Chen) that the founding governments did not authorize and that several (e.g., UK, Denmark, Visegrad) have explicitly contested.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint transfers bargaining power from immobile to mobile factors at continental scale — the single market's four freedoms operate as a mechanism for capital to arbitrage labor costs across borders. Suppression (0.68) is high because the constraint delegitimizes exit: national border controls are treated as treaty violations (ECJ Case C-439/16, Commission v. Hungary), and political resistance (Brexit, Visegrad) is framed as populist backsliding rather than legitimate contestation. Theater ratio (0.38) is moderate — the single market's coordination gains are real (GDP estimates +2-3% from integration) but a growing share of enforcement activity defends the citizenship-acquis expansion (e.g., social rights for jobseekers, reverse discrimination) rather than core market access. Accessibility collapse (0.65) reflects that alternatives (national labor market protection, sectoral transition arrangements) have been ruled out by ECJ jurisprudence. Resistance (0.48) is moderate — political contestation exists but is channeled into treaty reform (failed) or opt-outs (partial) rather than structural challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the supranational_institutions seat (agenda_setter, d~0), the constraint is a rope: genuine coordination solving the single market's factor-mobility problem, with negligible extraction from their position. From displaced_workers (payer, trapped, d~1), the same constraint is a snare: pure extraction enforced by a court they cannot vote against. From mobile_citizens (beneficiary, arbitrage, d~0), it is a mountain: a constitutional right that feels like natural law. The engine computes these per-seat types from the structural data; the divergence is the measurement. The integration reading's claim (tangled_rope) acknowledges the coordination function but the authored metrics show extraction dominating at the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation follows beneficiary/victim declarations plus exit modulation: supranational_institutions and mobile_citizens are declared beneficiaries with arbitrage/analytical exit → d near 0 (subsidy). capital_owners are beneficiaries with arbitrage exit → d near 0. local_labor_markets and peripheral_regions are victims with constrained exit → d near 0.8-0.9. displaced_workers are victims with trapped exit → d near 1.0. national_governments are dual-role: agenda_setter in Council but payer of political/fiscal costs, constrained exit (Article 50 treated as catastrophe) → d ~0.5-0.6. The engine will compute effective extraction χ from these structural positions — payer seats should experience high χ, beneficiary seats low or negative χ, creating the seat divergence that defines a tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Franco-German war prevention via economic interdependence) is dead — the geopolitical condition that made irreversible integration necessary no longer obtains. Yet the arrangement persists and has expanded (citizenship, services, digital, green deal) because the supranational institutions extract authority and budget from preventing revision. The coordination function (single labor market) remains live but is now instrumentalized: the ECJ uses free movement jurisprudence to expand supranational competence into social policy, criminal law, and fiscal governance — domains the founding treaties never assigned. This is classic mandatrophy: the mandate has outlived its function, but the constraint persists because the agenda_setter (supranational_institutions) benefits from its maintenance and the payers (local_labor_markets, displaced_workers, peripheral_regions) lack coalition power to force revision. The integration reading's constitutional framing ('free movement as constitutional right') is the mandatrophy cover story — it makes the arrangement unrevisable by democratic politics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_integration_reading,
    'This constraint is one reading (integration_reading) of the contested federation_membership kernel. What structural elements distinguish it from the sibling sovereignty_reading, and where is the disagreement located?',
    'Compare the two readings'' beneficiary/victim structures, claimed types, and axioms. The integration_reading places mobile_citizens in beneficiaries and local_labor_markets in victims with high ε; the sovereignty_reading would place national_governments as primary agenda_setters with border control as coordination function and mobile_citizens as conditional beneficiaries. The disagreement is located in: (1) whether free movement is a constitutional right (integration) or negotiable policy (sovereignty); (2) whether supranational authority is legitimate final interpreter (integration) or delegated agent (sovereignty); (3) whether membership is irreversible (integration) or conditional (sovereignty).',
    'If the integration_reading''s axioms are holdable and the sovereignty_reading''s are overridden, the kernel resolves toward irreversible integration. If both are holdable, the kernel remains contested and the federation operates in permanent constitutional tension. If the sovereignty_reading forecloses the integration_reading (e.g., via treaty reform), the constraint family collapses to a single reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_integration_reading, conceptual, 'Committer-frame structural delta between integration_reading and sovereignty_reading of the federation_membership kernel.').

omega_variable(
    labor_displacement_magnitude,
    'What is the actual magnitude of wage and employment displacement attributable to free movement, versus other factors (technology, globalization, domestic policy)?',
    'Natural experiments from enlargement rounds (2004, 2007, 2013) with difference-in-differences designs; sectoral panel data comparing exposed vs. non-exposed regions; ECB and OECD structural analyses controlling for concurrent shocks.',
    'If displacement is large and concentrated, the integration_reading''s extraction is structurally asymmetric (tangled_rope/snare). If displacement is small or diffuse, the coordination function dominates (rope). The integration_reading''s high ε (0.72) assumes substantial concentrated displacement — this omega tests that assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_displacement_magnitude, empirical, 'Whether the measured extractiveness reflects genuine labor displacement or is conflated with other structural trends.').

omega_variable(
    coordination_extraction_boundary,
    'Is the single market''s coordination function (factor mobility, regulatory harmonization) structurally separable from the citizenship-acquis expansion (social rights, non-economic mobility, reverse discrimination) that drives the measured extraction?',
    'Counterfactual modeling: simulate a ''thin'' single market with only economic freedoms (goods, services, capital, worker mobility) but no EU citizenship social rights, no reverse discrimination doctrine, no proportionality review of national social policies. Compare welfare and political stability outcomes.',
    'If separable, the integration_reading''s high extraction is a policy choice (doctrinal expansion by ECJ) not a coordination necessity — the constraint could be a rope with a thinner institutional layer. If inseparable, the extraction is the price of the coordination itself — the constitutional framing is structurally required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the coordination and extraction components of the integration_reading are structurally separable or jointly necessary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 1957, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_membership__integration_reading_tr_t1957, federation_membership__integration_reading, theater_ratio, 1957, 0.1).
narrative_ontology:measurement(federation_membership__integration_reading_tr_t1973, federation_membership__integration_reading, theater_ratio, 1973, 0.12).
narrative_ontology:measurement(federation_membership__integration_reading_tr_t1986, federation_membership__integration_reading, theater_ratio, 1986, 0.18).
narrative_ontology:measurement(federation_membership__integration_reading_tr_t1992, federation_membership__integration_reading, theater_ratio, 1992, 0.25).
narrative_ontology:measurement(federation_membership__integration_reading_tr_t2004, federation_membership__integration_reading, theater_ratio, 2004, 0.32).
narrative_ontology:measurement(federation_membership__integration_reading_tr_t2015, federation_membership__integration_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(federation_membership__integration_reading_tr_t2024, federation_membership__integration_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(federation_membership__integration_reading_be_t1957, federation_membership__integration_reading, base_extractiveness, 1957, 0.15).
narrative_ontology:measurement(federation_membership__integration_reading_be_t1973, federation_membership__integration_reading, base_extractiveness, 1973, 0.22).
narrative_ontology:measurement(federation_membership__integration_reading_be_t1986, federation_membership__integration_reading, base_extractiveness, 1986, 0.35).
narrative_ontology:measurement(federation_membership__integration_reading_be_t1992, federation_membership__integration_reading, base_extractiveness, 1992, 0.48).
narrative_ontology:measurement(federation_membership__integration_reading_be_t2004, federation_membership__integration_reading, base_extractiveness, 2004, 0.62).
narrative_ontology:measurement(federation_membership__integration_reading_be_t2015, federation_membership__integration_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(federation_membership__integration_reading_be_t2024, federation_membership__integration_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(federation_membership__integration_reading_su_t1957, federation_membership__integration_reading, suppression_requirement, 1957, 0.2).
narrative_ontology:measurement(federation_membership__integration_reading_su_t1973, federation_membership__integration_reading, suppression_requirement, 1973, 0.25).
narrative_ontology:measurement(federation_membership__integration_reading_su_t1986, federation_membership__integration_reading, suppression_requirement, 1986, 0.35).
narrative_ontology:measurement(federation_membership__integration_reading_su_t1992, federation_membership__integration_reading, suppression_requirement, 1992, 0.48).
narrative_ontology:measurement(federation_membership__integration_reading_su_t2004, federation_membership__integration_reading, suppression_requirement, 2004, 0.58).
narrative_ontology:measurement(federation_membership__integration_reading_su_t2015, federation_membership__integration_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(federation_membership__integration_reading_su_t2024, federation_membership__integration_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership__integration_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership__integration_reading, eu_single_market_acquis).
narrative_ontology:affects_constraint(federation_membership__integration_reading, eu_citizenship_doctrine).
narrative_ontology:affects_constraint(federation_membership__integration_reading, ecj_proportionality_review).

% DUAL FORMULATION NOTE:
% The federation_membership kernel decomposes into two constraint stories: integration_reading (this story) and sovereignty_reading. The integration_reading claims free movement as constitutional right with high ε from labor displacement; the sovereignty_reading claims border control as national competence with coordination function in democratic accountability. They share the same kernel (EU treaties) but have different beneficiary/victim structures, different ε, and different claimed types. The integration_reading influences the sovereignty_reading by setting the enforcement baseline (ECJ case law) that national resistance must overcome. Both stories link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership__integration_reading, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership__integration_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
