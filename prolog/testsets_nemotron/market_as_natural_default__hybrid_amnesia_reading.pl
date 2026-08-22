% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default: Hybrid Amnesia Reading
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_amnesia_reading of the
 *   contested kernel 'market_as_natural_default'. The reading describes a
 *   two-stage historical process: (1) 1930s-1970s genuine forgetting — the
 *   lived memory of pre-market social provisioning, planning alternatives,
 *   and institutional diversity atrophies through generational turnover, war
 *   disruption, and the marginalization of heterodox economics; (2)
 *   1980s-present defensive rationalization — as alternatives become
 *   thinkable again (post-1970s crises), incumbent beneficiaries weaponize
 *   the amnesia they inherited, actively suppressing alternatives through
 *   think-tank networks, academic gatekeeping, and the 'there is no
 *   alternative' (TINA) rhetorical apparatus. Extractiveness rises from
 *   ε≈0.20 to ε≈0.45 over the interval. The coordination function (price
 *   discovery, transaction-cost reduction) is real but increasingly performs
 *   as cover for extraction that concentrates gains among capital holders and
 *   financial intermediaries.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: Primary beneficiary (institutional/arbitrage) — captures extraction via asset appreciation, financialization, and regulatory capture
 *   - neoliberal_think_tanks: Agenda setter (organized/constrained) — produces and legitimates the naturalization narrative; funded by beneficiaries
 *   - wage_laborers: Primary victim (powerless/trapped) — bears extraction through wage suppression, precarization, and loss of public provisioning
 *   - precarious_workers: Victim (powerless/trapped) — faces intensified extraction with zero exit options
 *   - small_business_owners: Victim (moderate/constrained) — squeezed by financial-sector extraction and monopoly platforms
 *   - public_sector_constituents: Victim (organized/constrained) — loses public goods to privatization justified by market naturalness
 *   - global_south_economies: Victim (powerless/trapped) — subjected to structural adjustment enforcing the constraint externally
 *   - deregulation_era_policymakers: Beneficiary/agenda_setter hybrid (institutional/constrained) — enacts the constraint while rotating into beneficiary positions
 *   - financial_sector_incumbents: Beneficiary (institutional/arbitrage) — extracts via rent-seeking enabled by the naturalization narrative
 *   - heterodox_economists: Excluded (moderate/trapped) — would contest the constraint but are structurally marginalized in the discipline
 *   - historical_institutionalists: Observer (analytical/analytical) — documents the amnesia process without direct stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default: Hybrid Amnesia Reading").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, 'eb3e1a3c-bc2c-4683-92b2-fa566b90672b').
narrative_ontology:cs_kernel_codification('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', implicit).
narrative_ontology:cs_authority_grounding('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', extraction).
narrative_ontology:cs_interpretation_layer_present('eb3e1a3c-bc2c-4683-92b2-fa566b90672b').
narrative_ontology:cs_reading_relation('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', foundational, amnesia_precedes_capture).
narrative_ontology:cs_axiom_status(amnesia_precedes_capture, holdable).
narrative_ontology:cs_axiom_grounding('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', amnesia_precedes_capture, empirically_contingent).
narrative_ontology:cs_axiom('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', foundational, beneficiaries_weaponize_inherited_amnesia).
narrative_ontology:cs_axiom_status(beneficiaries_weaponize_inherited_amnesia, holdable).
narrative_ontology:cs_axiom_grounding('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', beneficiaries_weaponize_inherited_amnesia, empirically_contingent).
narrative_ontology:cs_reference_frame('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', postwar_keynesian_pluralism).
narrative_ontology:cs_drift_state('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', neoliberal_hegemony_consolidated, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('eb3e1a3c-bc2c-4683-92b2-fa566b90672b', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, neoliberal_think_tanks).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, deregulation_era_policymakers).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, financial_sector_incumbents).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, wage_laborers).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, precarious_workers).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, small_business_owners).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, public_sector_constituents).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, global_south_economies).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, there_is_no_alternative_narrative).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, comparative_advantage_presumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the productive and financial assets whose returns are amplified by the market-as-natural framing. Capital mobility and diversified holdings give them near-total exit from any single jurisdiction's policy regime. They fund the think-tank network that produces the naturalization narrative but do not directly administer the constraint.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Produce the intellectual infrastructure (papers, curricula, media commentary, policy briefs) that naturalizes market allocation. Funded by incumbent_capital_holders and financial_sector_incumbents. Their institutional survival depends on the constraint's persistence; they have constrained exit (other ideological networks exist but are marginally funded). They know the historical contingency but treat the naturalization as a necessary fiction.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, neoliberal_think_tanks, agenda_setter,
    organized, biographical, constrained, global).

% Bear the extraction through stagnant wages, eroded benefits, precarization, and the commodification of formerly public goods (housing, healthcare, education). Exit is trapped: labor mobility is limited by borders, skills specificity, and the universality of the constraint — there is no 'outside' to flee to. Identity-locked into the wage relation as primary social identity.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, wage_laborers, payer,
    powerless, biographical, trapped, global).

% Face the most intense extraction with zero buffers: gig economy, zero-hours contracts, algorithmic management. The market-as-natural framing justifies their classification as 'independent contractors' rather than employees. Exit is trapped — no savings, no alternative livelihoods, no collective bargaining access.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, precarious_workers, payer,
    powerless, immediate, trapped, global).

% Squeezed by financial-sector extraction (interest rates, fees), monopoly platform rents, and regulatory compliance costs that favor incumbents. They benefit marginally from market coordination (price signals, supplier access) but pay disproportionately for the financialized overlay. Exit is constrained: selling the business or closing are the only exits, both costly.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, small_business_owners, payer,
    moderate, biographical, constrained, national).

% Lose public provisioning (health, education, transport, housing) to privatization and marketization justified by the naturalness claim. Organized through unions and civic organizations but constrained by the ideological hegemony — political exit requires electing governments willing to break the consensus, which the constraint makes structurally difficult.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, public_sector_constituents, payer,
    organized, generational, constrained, national).

% Subjected to structural adjustment programs and trade rules that enforce market naturalization externally. The constraint operates as imperial discipline: debt conditionality, IP regimes, investment treaties. Exit is trapped — the global financial architecture enforces compliance. Identity-locked into 'developing' status within the market imaginary.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, global_south_economies, payer,
    powerless, generational, trapped, global).

% Enacted the 1980s-2000s deregulation, privatization, and financial liberalization that hardened the constraint. Many rotate into board positions, consultancy, and finance — directly becoming incumbent_capital_holders or financial_sector_incumbents. Their institutional power derives from the constraint; constrained exit because their reputation is bound to the 'reform' narrative.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, deregulation_era_policymakers, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, deregulation_era_policymakers, agenda_setter).

% Extract rents via intermediation fees, asset management, and the financialization of formerly non-financial domains (housing, care, infrastructure). The market-as-natural framing legitimates their centrality. Arbitrage-grade exit: capital is globally mobile, regulatory capture is portable across jurisdictions.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, financial_sector_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Produce alternatives (post-Keynesian, Marxist, feminist, ecological, institutional economics) but are structurally marginalized in hiring, publishing, funding, and policy advisory roles. The constraint's amnesia function targets them directly: their work is the memory the constraint must forget. Exit is trapped within the discipline; some exit to adjacent fields or activism.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, heterodox_economists, excluded,
    moderate, biographical, trapped, global).

% Document the historical process of amnesia and capture without direct material stake. Provide the evidentiary base for contesting the naturalization. Their analytical exit is total — they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, historical_institutionalists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates complex production and distribution across billions of agents via price signals, reducing the transaction costs of decentralized decision-making. Allocates capital to productive uses (in theory) and enables specialization at scale.
% TRANSFER_FUNCTION: Moves surplus from labor (wage suppression, benefit erosion), public sectors (privatization, austerity), small businesses (platform rents, financial extraction), and peripheral economies (unequal exchange, debt service) to capital holders and financial intermediaries. The transfer is mediated by the 'market efficiency' discourse that frames the outcome as natural rather than political.
% ABSENT_VOICES: Workers in the global south subjected to structural adjustment; future generations bearing ecological costs externalized by the market frame; heterodox economists excluded from the discipline; communal and indigenous economies whose provisioning systems are rendered invisible by the market imaginary. They are absent because the constraint operates at the level of the thinkable — their voices are not merely unheard but structurally unimaginable within the dominant framework.
% DISAPPEARANCE_RATIONALE: If the market-as-natural-default constraint vanished overnight, the ideological infrastructure justifying financialization, austerity, privatization, and labor precarization would collapse. Policy would revert to pragmatic experimentation with planning, public provisioning, cooperatives, and decommodified care. Capital would lose its primary legitimating narrative. The global trade regime would require renegotiation. The world would rearrange profoundly — not into chaos, but into a contested space where alternatives are thinkable again.
% FOUNDING_PROBLEM: The Great Depression and WWII destroyed faith in laissez-faire markets, but the postwar Keynesian consensus faced mounting crises (stagflation, profit squeeze, global competition) by the 1970s. The founding problem was: how to coordinate complex industrial economies without the rigidities of central planning AND without the instabilities of unregulated markets? The market-as-natural frame emerged as the answer: let markets coordinate, but naturalize the specific market form (financialized, globalized, shareholder-primacy) as the only viable coordination mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The beneficiaries (incumbent_capital_holders, neoliberal_think_tanks) attest the problem is live — markets are still the only viable coordinator. Historical institutionalists (Polanyi, Block, Mazzucato, Tooze) and heterodox economists attest the founding problem was specific to the 1970s conjuncture and is substantially solved by modern computational planning, democratic coordination, and ecological economics — the constraint persists as capture. Global south economists (Amin, Arrighi, Patnaik) attest the problem was never theirs — the constraint was imposed externally. No single corroboration exists outside the beneficiary set; the field is fractured.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is tangled_rope because the constraint retains a genuine coordination function (markets do coordinate complex production) while simultaneously extracting asymmetrically from labor, public sectors, and peripheral economies. The coordination function is the 'rope' component; the extraction is the 'tangled' component. Extractiveness rises over time as the amnesia deepens and defensive rationalization hardens — early period (1930s-1970s) is lower extraction because the constraint is still partially a collapsed scaffold (forgotten alternatives); late period (1980s-present) is higher extraction because beneficiaries actively defend the arrangement. Suppression increases as alternatives are actively excluded from policy discourse, academic curricula, and institutional imagination. Theater ratio rises as the 'market efficiency' discourse becomes increasingly performative — the rhetorical apparatus expands while the coordination reality contracts. Accessibility collapse is high (0.72) because the constraint operates at the level of imaginative infrastructure: once the market is framed as natural, alternatives become literally unthinkable for most agents. Resistance is moderate (0.38) — present but fragmented, lacking a unified counter-hegemonic imaginary.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (incumbent_capital_holders, financial_sector_incumbents), the constraint appears as a mountain or rope — the market order feels natural, efficient, and beneficial. From the victim seats (wage_laborers, precarious_workers, global_south_economies), it appears as a snare — extraction is visible, exit is blocked, alternatives are suppressed. From the agenda_setter seat (neoliberal_think_tanks), it appears as a scaffold they are actively maintaining — they know the history but treat the naturalization as a necessary fiction. The engine computes this divergence from the structural data: different power/exit combinations yield different effective extraction (χ) and thus different per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbent_capital_holders, neoliberal_think_tanks, deregulation_era_policymakers, financial_sector_incumbents) derive d near 0.0-0.2: they collect rents, control the narrative, and have arbitrage-grade exit (capital mobility, revolving doors). Victims (wage_laborers, precarious_workers, small_business_owners, public_sector_constituents, global_south_economies) derive d near 0.7-0.9: they bear the extraction, have trapped or constrained exit (labor mobility is limited, structural adjustment is externally imposed), and are identity-locked into the wage-labor relation. The excluded seat (heterodox_economists) derives d near 0.5: they are constrained but not directly extracted from. The observer seat (historical_institutionalists) derives d=0.5 by definition. The engine scales effective extraction χ by these directionalities and the global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating complex production without central planning) was live in the 1930s-1970s but is now contested: markets coordinate but the specific form (financialized, shareholder-primacy, globally integrated) is not the only solution. The constraint persists because beneficiaries captured the amnesia — the mandate atrophied but the arrangement was weaponized. This is not a scaffold (no sunset clause) nor a piton (beneficiaries actively maintain it, not inertia). It is a tangled_rope where the coordination function is real but the extraction component has grown to dominate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''market_as_natural_default'', and does it instantiate the hybrid_amnesia_reading specifically?',
    'Cross-file verification with sibling readings lapsed_alternative_reading and beneficiary_maintained_reading; each must declare the same kernel_id and distinct reading_id.',
    'If the kernel framing is rejected, this constraint reverts to a standalone story without committer structure; the reading_relations and axioms in cs_structure become inapplicable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the kernel/reading decomposition; this reading asserts a two-stage amnesia-capture process.').

omega_variable(
    amnesia_mechanism_ambiguity,
    'Was the 1930s-1970s forgetting genuinely endogenous (loss of lived memory, generational turnover) or was it already shaped by nascent beneficiary influence (early think-tank funding, academic capture)?',
    'Archival research on early neoliberal network funding (Volker Fund, Mont Pelerin Society archives) and curriculum histories in economics departments; oral histories of mid-century policy intellectuals.',
    'If endogenous, the constraint''s initial stage is closer to a scaffold that collapsed; if beneficiary-shaped, the extraction lineage extends further back and the hybrid reading''s ''genuine forgetting'' claim weakens toward beneficiary_maintained_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amnesia_mechanism_ambiguity, empirical, 'Whether the amnesia phase was genuinely passive or already pre-structured by emerging beneficiaries.').

omega_variable(
    defensive_rationalization_boundary,
    'Where does the shift from ''genuine forgetting'' to ''defensive rationalization'' (1980s) actually occur — is it a clean break or a gradual blending?',
    'Discourse analysis of policy rhetoric 1975-1990; think-tank publication networks; citation patterns in economics journals tracking ''market efficiency'' from contested claim to presupposition.',
    'A sharp break supports the hybrid reading''s two-stage structure; a gradual blend collapses the distinction toward lapsed_alternative_reading (if forgetting dominates) or beneficiary_maintained_reading (if rationalization dominates throughout).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_rationalization_boundary, conceptual, 'The temporal boundary between the two stages of the hybrid process.').

omega_variable(
    coordination_function_genuineness,
    'Does the market-as-natural-default constraint still perform a genuine coordination function (reducing transaction costs, enabling price discovery) for ANY stakeholder, or has the coordination cover fully collapsed into extraction?',
    'Counterfactual simulation: if the constraint vanished, would any coordination problems resurface that market institutions actually solve? Compare with historical periods of stronger alternative imaginaries.',
    'If genuine coordination persists for some stakeholders, tangled_rope is structurally correct; if coordination is wholly performative, the constraint migrates toward snare or piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_genuineness, conceptual, 'Whether the coordination component of the tangled_rope classification has residual reality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 1930, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_tr_t1930, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1930, 0.15).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_tr_t1930, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_tr_t1950, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_tr_t1950, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_tr_t1970, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_tr_t1970, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_tr_t1980, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_tr_t1980, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_tr_t1990, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_tr_t1990, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_tr_t2000, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_tr_t2000, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_tr_t2010, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_tr_t2010, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_tr_t2020, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_be_t1930, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1930, 0.2).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_be_t1930, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_be_t1950, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_be_t1950, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_be_t1970, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_be_t1970, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_be_t1980, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_be_t1980, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_be_t1990, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_be_t1990, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_be_t2000, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_be_t2000, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_be_t2010, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_be_t2010, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_be_t2020, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_su_t1930, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1930, 0.25).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_su_t1930, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_su_t1950, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_su_t1950, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_su_t1970, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_su_t1970, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_su_t1980, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_su_t1980, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_su_t1990, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_su_t1990, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_su_t2000, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_su_t2000, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_su_t2010, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_su_t2010, observed).
narrative_ontology:measurement(market_as_natural_default__hybrid_amnesia_reading_su_t2020, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement_basis(market_as_natural_default__hybrid_amnesia_reading_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__hybrid_amnesia_reading, 0.12).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, financialization_as_extraction_mechanism).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, shareholder_primacy_norm).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, global_trade_regime_as_market_enforcement).

% DUAL FORMULATION NOTE:
% Part of the market_as_natural_default kernel family. This reading (hybrid_amnesia) differs from lapsed_alternative (forgetting-only) by positing a second-stage active capture, and from beneficiary_maintained (active-from-start) by positing an initial genuine amnesia phase. The ε values differ: lapsed_alternative would show flat low extraction; beneficiary_maintained would show high extraction throughout; hybrid_amnesia shows the observed rise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__hybrid_amnesia_reading, institutional, 0.15).
constraint_indexing:directionality_override(market_as_natural_default__hybrid_amnesia_reading, powerless, 0.85).
constraint_indexing:directionality_override(market_as_natural_default__hybrid_amnesia_reading, moderate, 0.6).
constraint_indexing:directionality_override(market_as_natural_default__hybrid_amnesia_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
