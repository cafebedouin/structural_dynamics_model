% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Treaty Framework — Developmental Reading
 *   domain: international_trade_law / development_economics / political_economy
 *
 * SUMMARY:
 *   The WTO treaty framework instantiates a contested kernel: whether the
 *   framework is fundamentally a mechanism for universal market access and
 *   non-discrimination (market-access reading) or a treaty embodying
 *   equal-status commitments between states at asymmetric development levels
 *   (developmental reading). This constraint story instantiates the
 *   developmental reading: a reading under which S&D (Special and
 *   Differential) provisions are permanent structural accommodations—not
 *   temporary phase-outs—that preserve policy space for tariffs, subsidies,
 *   and compulsory licensing as legitimate tools of endogenous development.
 *   Under this reading, the WTO framework is a coordination mechanism solving
 *   the problem of enabling least-developed and Global South states to build
 *   industrial capacity without facing the competition of already-developed
 *   firms operating at scale. Extractiveness is moderate (0.31): the
 *   framework constrains multinational corporations via technology transfer
 *   obligations and compulsory licensing, and constrains developed-state
 *   governments via tariff barriers protecting infant industries, but it also
 *   extracts from developing states via dispute-settlement pressure and
 *   non-discrimination disciplines that prevent discriminatory protection.
 *   The framework's enforcement is light (suppression 0.18) because it is a
 *   treaty commitment rather than a coercive institution; enforcement rests
 *   on reciprocal reputation and selective trade retaliation for breach,
 *   which developing states carry less capacity to execute. Theater (0.22)
 *   reflects that the Secretariat often frames S&D provisions as phase-out
 *   exceptions rather than structural accommodations—a narrative drift away
 *   from the developmental reading's core claim, which has increased over the
 *   interval as developed states have pressed for 'special and differential
 *   treatment must not be special anymore' language.
 *
 * KEY AGENTS:
 *   - least_developed_countries: powerless, trapped exit — depend entirely on policy space; withdrawal is economically catastrophic
 *   - global_south_states: moderate power, constrained exit — negotiating coalition provides leverage; defection costs are coalition expulsion and loss of coordination
 *   - multinational_corporations: powerful, arbitrage exit — can litigate via investor-state dispute settlement; can move operations to friendlier jurisdictions or lobby developed governments
 *   - developed_country_governments: institutional power, mobile exit — can form preferential trade agreements excluding Global South; can veto treaty amendments; control dispute-settlement narratives
 *   - WTO secretariat: institutional power, trapped exit — administers the framework; legitimacy depends on consistent interpretation; cannot withdraw without institutional dissolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.31).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.18).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Treaty Framework — Developmental Reading").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international_trade_law / development_economics / political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, 'f1b8ebc9-d19b-46f7-87d0-7af37a8e572b').
narrative_ontology:cs_kernel_codification('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', fixed_text).
narrative_ontology:cs_authority_grounding('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', extraction).
narrative_ontology:cs_interpretation_layer_present('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b').
narrative_ontology:cs_reading_relation('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', wto_treaty_framework__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', foundational, s_and_d_permanent_structural_accommodation).
narrative_ontology:cs_axiom_status(s_and_d_permanent_structural_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', s_and_d_permanent_structural_accommodation, deontological).
narrative_ontology:cs_axiom('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', foundational, development_policy_space_equal_status_right).
narrative_ontology:cs_axiom_status(development_policy_space_equal_status_right, holdable).
narrative_ontology:cs_axiom_grounding('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', development_policy_space_equal_status_right, deontological).
narrative_ontology:cs_axiom('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', secondary, technology_transfer_compulsory_obligation).
narrative_ontology:cs_axiom_status(technology_transfer_compulsory_obligation, overridden).
narrative_ontology:cs_axiom_grounding('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', technology_transfer_compulsory_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', equal_status_development_partnership).
narrative_ontology:cs_drift_state('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', contemporary_post_2015, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f1b8ebc9-d19b-46f7-87d0-7af37a8e572b', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, global_south_states).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, domestic_infant_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_corporations).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developed_country_governments).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, equal_status_development_principle).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, asymmetric_starting_conditions_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend entirely on policy space and technology transfer access to build endogenous industrial capacity. Tariff protection and subsidy space are the mechanisms by which they can protect infant industries from immediate destruction by global competition. Compulsory licensing for medicines and agricultural inputs is essential for public health and food security in contexts where patent prices exceed local purchasing power. Withdrawal from the treaty is economically catastrophic (loss of market access, investor-state litigation, retaliation); staying requires accepting the constraint. Identity is locked in the aspiration to develop: they cannot exit without accepting permanent subordination in the global division of labor.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, least_developed_countries, beneficiary,
    powerless, generational, identity_locked, global).

% Mid-development countries (Brazil, India, Mexico, South Africa, Indonesia) that have used policy space to build industrial capacity (Brazil's aerospace, India's pharmaceuticals and IT, Mexico's automotive) and need to preserve that space for future sectors. Technology transfer obligations and compulsory licensing are valuable tools for catch-up; tariff and subsidy protection are ongoing necessities for maintaining industrial competitiveness. Constrained exit: can form regional alternatives (MERCOSUR, African Union, BRICS), can threaten non-cooperation in WTO governance (consensus requirement gives them veto power), can litigate disputes; but cannot fully withdraw without losing multilateral trade legitimacy.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_south_states, beneficiary,
    moderate, generational, constrained, global).

% Face technology transfer obligations, compulsory licensing provisions enabling generic production of patented goods in developing markets, and tariff/subsidy protection that prevents their products from fully penetrating protected markets. Patent and IP enforcement is systematically weakened in developing countries operating under the developmental reading. Can arbitrage: litigate via investor-state dispute settlement (challenging compulsory licensing or tariff barriers as unfair expropriation); lobby developed governments to pressure WTO interpretation toward market_access reading; shift operations to developed markets or countries with stronger IP enforcement; form lobbying coalitions (pharmaceutical industry, technology firms) to oppose compulsory licensing and technology transfer.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, multinational_corporations, payer,
    powerful, biographical, arbitrage, global).

% Their multinational corporations are constrained by compulsory licensing and technology transfer obligations; their export firms face tariff barriers in developing markets protected by policy space. However, they retain significant power: they can form preferential trade agreements excluding developing countries (FTAs with stronger IP enforcement and investment-chapter teeth); they control the dispute-settlement appointment process and can secure judges favoring the market_access_reading; they can lobby the Secretariat and pressure consensus on interpretive guidance; they retain veto power over treaty amendment. Mobile exit: can pressure developing states bilaterally; can bypass the WTO through bilateral deals; can threaten retaliation for use of policy space (e.g., sanctions on countries using compulsory licensing).
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_country_governments, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, developed_country_governments, observer).

% Administers and interprets the treaty framework. Legitimacy depends on interpreting the treaty faithfully to its text and respecting the consensus decision-making that governs WTO governance. Under the developmental reading, the Secretariat is obliged to enforce S&D provisions as structural commitments, defend policy space as equal-status treaty rights, and enable technology transfer and compulsory licensing. However, the Secretariat is subject to funding pressure from developed states and to narrative pressure from the multinational corporation lobby and their supporters. The Secretariat's interpretive practice has drifted toward the market_access reading (framing S&D as temporary, graduation as inevitable) despite the treaty text supporting both readings. Trapped exit: cannot rewrite the treaty without consensus; cannot withdraw its interpretations without loss of authority; cannot openly favor one reading without appearing partisan. The institutional structure locks the Secretariat into performing neutrality while delivering drift.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_secretariat, agenda_setter,
    institutional, generational, trapped, global).

% Adjudicate conflicts between policy space and market access claims. The Appellate Body's composition is decided by consensus, but appointment practice has resulted in a supermajority of judges trained in developed-country trade law and predisposed to the market_access reading. Under the developmental reading, these bodies should interpret ambiguities in the treaty to maximize policy space and preserve S&D provisions as structural accommodations. However, their actual practice (documented in case law 1995–2024) has systematically narrowed infant-industry defenses, constrained compulsory licensing, and pressed graduation logic on developing countries. Their interpretation choices are materially determining which kernel reading survives in practice.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_dispute_settlement_bodies, observer,
    institutional, generational, analytical, global).

% Labor movements, indigenous-rights advocates, environmental organizations, and public-health NGOs are not formal signatories and are excluded from treaty negotiation and interpretation. Would advocate for labor standards, indigenous land rights, environmental sustainability, and stronger public-health carve-outs in the treaty framework. Their exclusion means these concerns are not coded into the treaty text; their objections enter only through developed-state NGO filtration or as diffuse public pressure. Neither the developmental nor the market_access reading adequately addresses their concerns about whether development is compatible with labor rights, ecological limits, or indigenous sovereignty.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, civil_society_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__developmental_reading, multinational_corporations).
narrative_ontology:fixing_cost_class(wto_treaty_framework__developmental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for trade across states at asymmetric development levels. Solves the collective-action problem: if developing states were forced to compete with developed states under identical rules, the developed states' already-installed industrial capacity and technological base would destroy developing states' capacity to build domestic industries. The framework enables trade without forcing this destruction by preserving policy space (tariff flexibility, subsidy authority, compulsory licensing) for developing states to protect infant industries and build endogenous capacity while participating in global markets.
% TRANSFER_FUNCTION: Moves technology access (via compulsory licensing and technology transfer obligations) from multinational corporations to developing states; moves tariff protection and subsidy space to developing states' infant industries and governments; moves market access and IP enforcement expectations toward developed-country firms. The trade-off is asymmetric: developing states get policy flexibility and technology access; multinational corporations face constrained IP rights and market access barriers; developed-country governments see their firms face tariff protection in developing markets.
% ABSENT_VOICES: Labor movements in both developed and developing countries (excluded from the table; would demand minimum wage standards, organizing rights, worker mobility); indigenous-rights advocates (would demand recognition of land rights and resource sovereignty in development planning); environmental movements (would demand climate and biodiversity integration in development definitions); public-health advocates (would demand stronger carve-outs for medicines, vaccines, and food security beyond the nominal Doha Declaration language). Structural absence: the framework treats development as purely economic growth and industrial capacity-building, without integrating labor, environment, or indigenous-rights dimensions.
% DISAPPEARANCE_RATIONALE: If the developmental reading framework vanished and the WTO operated purely under the market_access reading, developing states would face immediate tariff reduction pressure, elimination of subsidy space, full IP enforcement on medicines and agricultural inputs, and compulsory graduation toward liberalization. Domestic industries in Global South states would collapse under competition; technology transfer incentives would evaporate. Industrial policy—the mechanism by which all currently-developed countries built capacity—would become legally indefensible for developing countries. Global inequality would accelerate as developing states lost capacity to build endogenous technological and human development.
% FOUNDING_PROBLEM: After colonialism imposed deindustrialization and structural adjustment drained state capacity, post-WWII development economics identified that all currently-developed countries (UK, US, Germany, Japan, South Korea) used tariff protection, infant-industry subsidies, and technology acquisition (via espionage, licensing, reverse-engineering) to build industrial capacity during their own development phases. These countries liberalized ONLY AFTER achieving industrial competitiveness. The founding problem is: how do states that enter the global economy with deindustrialized bases, limited technological capacity, and small domestic markets compete against already-developed firms without access to the same policy tools that enabled the developed countries' own development? How does unequal development become equal development without policy space?
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the benefiting states (Ha-Joon Chang, Erik Reinert, Gunnar Myrdal, UNCTAD research) document that the founding problem was empirically the condition that motivated the Havana Charter and the GATT, and that the problem remains live: countries that abandoned policy space (Sub-Saharan Africa under structural adjustment, Latin America under Washington Consensus) experienced deindustrialization and income divergence; countries that retained policy space (Vietnam, Bangladesh in textiles, India in pharmaceuticals, China broadly) achieved industrial capacity. Developed-state governments and multinational corporations contend the founding problem is obsolete (global supply chains allow development without tariff protection; market access substitutes for policy space); this contention is corroborated ONLY by the benefiting parties themselves and is contradicted by observable development outcomes (Africa's industrial share of GDP has contracted since liberalization; Asia's industrial economies grew under retained policy space). UN UNCTAD and Global South government bodies corroborate that the founding problem persists.
narrative_ontology:disappearance_verdict(wto_treaty_framework__developmental_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__developmental_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__developmental_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(wto_treaty_framework__developmental_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).
:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The developmental reading produces moderate extractiveness (0.31) because the constraint does real coordination work (enabling trade across asymmetric partners without destroying developing-state capacity) but simultaneously constrains multiple parties asymmetrically. From a least-developed country's position, the constraint is near-beneficiary (enables policy space, provides compulsory-licensing authority, coordinates access to technology and markets). From a multinational corporation's position, it is extractive (forces technology transfer, enables compulsory licensing, faces tariff barriers). From a developed-country government's position, it is moderately extractive (markets in Global South are protected from full penetration by infant-industry tariffs; IP rights are weakened; their firms face compulsory-licensing pressure). Suppression is light (0.18) because the framework depends on voluntary treaty membership, reciprocal negotiation, and consensus decision-making—not on coercive enforcement machinery. A state unhappy with the treaty can withdraw (though at severe cost). Theater has risen over the interval (1995: 0.15 → 2024: 0.22) because the Secretariat and dispute-settlement bodies have increasingly interpreted S&D provisions as temporary phase-outs and 'graduation' requirements, rather than as permanent structural accommodations; this narrows the developmental reading's operational space while maintaining nominal agreement that development is a legitimate goal. The measurement series show extractiveness stable (0.28–0.32) despite theater rising—the real constraints on policy space have persisted despite the interpretive drift, but the performance of defending those constraints has grown more elaborate and less honest. Suppression has fallen slightly (0.12 → 0.18, then 0.18 toward 2024) as developing states have gained capacity to challenge disputes and have formed regional alternatives (African Union, BRICS, regional trade agreements) that reduce the WTO's monopoly power.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap sits between least-developed countries and multinational corporations. From the LDC seat: the constraint enables endogenous development by protecting infant industries, preserving subsidy space for human development, and extracting technology transfer as a condition of market access. The trade-off (facing tariff barriers and facing dispute settlement pressure when they use policy space) is acceptable because the alternative (unfettered competition with developed-country firms) is catastrophic. From the multinational corporation seat: the constraint imposes uncompensated obligations (technology transfer without compensation, compulsory licensing below market rates, tariff barriers blocking market access). The Secretariat occupies a third perspective: its role is to interpret the framework faithfully to the treaty text and to respect the consensus decision-making that governs WTO governance. This should, under the developmental reading, mean defending policy space and S&D provisions as structural commitments. However, the Secretariat's interpretive practice has drifted toward the market-access reading—framing S&D provisions as temporary and graduation-based—because developed states, which control the dispute-settlement funding and narrative, have pressed that framing. The gap between what the developmental reading requires (permanent structural accommodation) and what the Secretariat delivers (temporary exception language) is widening over the interval; this is captured in the rising theater_ratio.
 *
 * DIRECTIONALITY LOGIC:
 *   The developmental reading derives directionality from the structural beneficiary/victim positions and exit constraints. Least-developed countries are the primary beneficiaries (benefit from policy space, technology transfer authority, compulsory licensing) and are identity-locked via their dependence on development pathways and on the legitimacy of their development aspirations—exit would mean accepting permanent subordination in the global division of labor. Global South states are secondary beneficiaries with stronger coalition power and constrained (not identity-locked) exit. Multinational corporations and developed-country governments are the structural targets (face constraints on IP rights, market access, subsidy discipline) but have high exit options: multinationals can arbitrage via investor-state dispute settlement or relocate; developed governments can form preferential agreements or pressure unilaterally. The WTO Secretariat occupies a dual position: it administers the framework (agenda-setter role) but is trapped in its interpretation (cannot rewrite the treaty; must respect consensus governance; legitimacy depends on consistent application). From the WTO's institutional seat, the developmental reading is beneficial (it legitimates the organization as serving development goals and keeps developing states engaged); from a multinational corporation's seat, it is extractive (compulsory licensing, technology transfer). The engine will compute different types from each seat because the structural asymmetry is real: the framework coordinates development and restricts extraction in a way that is beneficiary-serving from developing-state positions and target-serving from multinational and developed-government positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The developmental reading avoids false mandatrophy classification by clearly establishing that the founding problem (enabling developing states to build industrial capacity without facing destruction from mature competitors) remains contested but LIVE. The Global South coalition, Global South government bodies, and external economic historians corroborate that the problem persists: development trajectories in countries that abandoned policy space have degraded (Sub-Saharan Africa's industrial capacity contracted post-liberalization), whereas countries that retained policy space (Vietnam, Bangladesh in textiles; India in pharmaceuticals) developed endogenously. The mandate is contested because developed states and the multinational corporation constituency argue the problem is obsolete (supply chains and market access are sufficient for development), but that contention is not corroborated by observable development outcomes. The classification as a Rope (genuine coordination with asymmetric constraints, not pure extraction) is defensible because the framework does solve a real coordination problem—it enables trade across asymmetric partners without destroying the capacity of weaker partners to build endogenous capacity. The Rope classification remains robust even as theater rises, because the real constraint on policy space persists: a developing state cannot legally impose tariffs beyond the bound rate, cannot subsidize domestic industries beyond the subsidy cap (ASCM limits), and faces compulsory licensing challenges through the TRIPS Agreement dispute process, regardless of narrative framing in Secretariat documents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    s_d_provision_permanence_ambiguity,
    'Are S&D (Special and Differential) provisions structurally permanent accommodations for asymmetric starting conditions, or are they temporary phase-out exceptions for countries in transition toward full liberalization?',
    'Examine WTO treaty text (Articles 1, 10, XXVIII GATT; differential and more favorable treatment clauses in TRIPS, ASCM, SPS) and dispute-settlement interpretations over the interval. If the Secretariat and Appellate Body consistently interpret S&D as phase-out exceptions and apply ''graduation'' logic, the developmental reading loses operative force. If they interpret S&D as structural accommodations recognizing permanent asymmetry, the developmental reading gains strength. The resolution is textual-hermeneutic, not empirical.',
    'If S&D provisions are permanent: the developmental reading is defensible, policy space is preserved, technology transfer obligations remain enforceable. If S&D provisions are temporary: the developmental reading collapses into the market_access reading, policy space erodes over time, developing states face mounting liberalization pressure. This is the central axis differentiating the two kernel readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(s_d_provision_permanence_ambiguity, conceptual, 'Whether S&D provisions instantiate permanent structural accommodation or temporary transition exception.').

omega_variable(
    technology_transfer_enforceability,
    'Can technology transfer obligations (TRIPS Article 66.2, capacity-building clauses) be enforced against multinational corporations as binding commitments, or are they treated as best-efforts aspirations without dispute-settlement teeth?',
    'Document actual dispute-settlement cases brought by developing states alleging failure to transfer technology; examine whether the Secretariat and Appellate Body treat technology transfer obligations as justiciable contract language or as policy recommendations. Historical data (1995–2024): zero successful disputes brought for technology transfer violations (as of 2024). This near-zero enforceability is the resolution.',
    'If technology transfer is enforceable, the developmental reading produces real extraction of IP rights and obligatory knowledge flows; if it is not enforceable, the developmental reading''s core mechanism (coercive technology diffusion) is absent and the constraint is a weaker rope than claimed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_transfer_enforceability, empirical, 'Whether technology transfer obligations are enforced or treated as voluntary.').

omega_variable(
    infant_industry_defense_collapse,
    'Can developing states use tariff protection and infant-industry exemptions (GATT Article XVIII, Safeguards Agreement) to defend growing industries against mature-market competition, or does the dispute-settlement body systematically restrict these exemptions as protectionist rather than developmental?',
    'Examine disputes where developing states invoked infant-industry or development grounds for tariffs (India''s pharmaceutical tariffs, Brazil''s automotive tariffs, etc.); document whether the Appellate Body upheld or rejected these defenses. Historical pattern (1995–2024): the Appellate Body has systematically narrowed infant-industry defenses, requiring faster tariff phase-outs and denying ''indefinite'' development periods. This represents a collapse of the structural accommodation the developmental reading depends on.',
    'Collapse of infant-industry defense operationalizes the market_access reading (tariff protection becomes indefensible on development grounds; liberalization becomes mandatory). Preservation of the defense preserves the developmental reading''s operational space. The trend is toward collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infant_industry_defense_collapse, empirical, 'Whether infant-industry tariff protection remains available as a development tool or has been systematically narrowed.').

omega_variable(
    compulsory_licensing_scope,
    'Can developing states invoke public-health emergencies (HIV/AIDS medicines, pandemic vaccines) or agricultural necessity to license production of patented goods without IP infringement liability, or does the TRIPS enforcement regime (especially post-TRIPs flexibilities interpretation) constrain compulsory licensing to narrow carve-outs?',
    'Document compulsory-licensing cases (India''s cancer-drug licensing, South Africa''s ARV production, Bangladesh''s generic manufacturing); examine whether developed states and patent holders challenge these via bilateral pressure, investor-state dispute settlement, or WTO dispute panels. Historical pattern (1995–2024): significant pressure against compulsory licensing; developed states have used FTA investment chapters to pressure against licensing; the legal space has narrowed despite Doha Declaration language nominally supporting public health. This represents a reduction of the developmental reading''s core mechanism (access to essential knowledge).',
    'Narrowed compulsory-licensing scope reduces the developmental reading''s enforceability for public-health and food-security development (IP rights are preserved for patent holders; technology remains enclosed). Preserved scope preserves the developmental reading''s extraction mechanism (forcing technology diffusion). The trend is toward narrowing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compulsory_licensing_scope, empirical, 'Whether compulsory licensing remains available for public health and development, or has been constrained by IP enforcement.').

omega_variable(
    reading_kernel_coexistence_mechanism,
    'By what institutional mechanism do these two incompatible kernel readings (permanent-structure vs. temporary-phase-out) coexist in the same treaty system without one being formally foreclosed or one triumphing definitively?',
    'Examine WTO governance: the treaty text contains language supporting both readings (S&D provisions exist, but so does non-discrimination and market-access language); disputes are resolved by an Appellate Body composed of judges appointed by consensus (which includes developing states'' veto); the Secretariat is neutral on interpretation. The mechanism is textual ambiguity + consensus governance. As long as treaty amendment requires consensus, neither reading can be formally eliminated. However, interpretive practice (dispute settlement, Secretariat guidance) has been drifting toward the market_access reading because developed states'' appointment power in the Appellate Body has secured a supermajority of judges favoring that reading. The coexistence is unstable and is being resolved through interpretive drift, not through explicit legal change.',
    'Understanding this mechanism reveals that the developmental reading''s operational space is being eroded not through treaty amendment (which developing states could block) but through dispute-settlement interpretation (which they can challenge but not fully control). This is the primary extraction mechanism: the apparent coexistence masks a gradual institutionalization of the market_access reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_coexistence_mechanism, conceptual, 'How two incompatible kernel readings coexist in the same treaty system without formal resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__developmental_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement_basis(wto__tr_t1995, observed).
narrative_ontology:measurement(wto__tr_t2001, wto_treaty_framework__developmental_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement_basis(wto__tr_t2001, observed).
narrative_ontology:measurement(wto__tr_t2008, wto_treaty_framework__developmental_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement_basis(wto__tr_t2008, observed).
narrative_ontology:measurement(wto__tr_t2015, wto_treaty_framework__developmental_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(wto__tr_t2015, observed).
narrative_ontology:measurement(wto__tr_t2020, wto_treaty_framework__developmental_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement_basis(wto__tr_t2020, observed).
narrative_ontology:measurement(wto__tr_t2024, wto_treaty_framework__developmental_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(wto__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__developmental_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement_basis(wto__be_t1995, observed).
narrative_ontology:measurement(wto__be_t2001, wto_treaty_framework__developmental_reading, base_extractiveness, 2001, 0.29).
narrative_ontology:measurement_basis(wto__be_t2001, observed).
narrative_ontology:measurement(wto__be_t2008, wto_treaty_framework__developmental_reading, base_extractiveness, 2008, 0.31).
narrative_ontology:measurement_basis(wto__be_t2008, observed).
narrative_ontology:measurement(wto__be_t2015, wto_treaty_framework__developmental_reading, base_extractiveness, 2015, 0.32).
narrative_ontology:measurement_basis(wto__be_t2015, observed).
narrative_ontology:measurement(wto__be_t2020, wto_treaty_framework__developmental_reading, base_extractiveness, 2020, 0.3).
narrative_ontology:measurement_basis(wto__be_t2020, observed).
narrative_ontology:measurement(wto__be_t2024, wto_treaty_framework__developmental_reading, base_extractiveness, 2024, 0.31).
narrative_ontology:measurement_basis(wto__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__developmental_reading, suppression_requirement, 1995, 0.12).
narrative_ontology:measurement_basis(wto__su_t1995, observed).
narrative_ontology:measurement(wto__su_t2001, wto_treaty_framework__developmental_reading, suppression_requirement, 2001, 0.16).
narrative_ontology:measurement_basis(wto__su_t2001, observed).
narrative_ontology:measurement(wto__su_t2008, wto_treaty_framework__developmental_reading, suppression_requirement, 2008, 0.19).
narrative_ontology:measurement_basis(wto__su_t2008, observed).
narrative_ontology:measurement(wto__su_t2015, wto_treaty_framework__developmental_reading, suppression_requirement, 2015, 0.21).
narrative_ontology:measurement_basis(wto__su_t2015, observed).
narrative_ontology:measurement(wto__su_t2020, wto_treaty_framework__developmental_reading, suppression_requirement, 2020, 0.2).
narrative_ontology:measurement_basis(wto__su_t2020, observed).
narrative_ontology:measurement(wto__su_t2024, wto_treaty_framework__developmental_reading, suppression_requirement, 2024, 0.18).
narrative_ontology:measurement_basis(wto__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__developmental_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__developmental_reading, 0.18).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, wto_treaty_framework__market_access_reading).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, trips_agreement_enforcement).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, global_supply_chain_governance).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, bilateral_trade_agreement_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel wto_treaty_framework. The sibling reading is wto_treaty_framework__market_access_reading, which instantiates the same treaty text under a different interpretive lens: S&D provisions as temporary phase-outs rather than permanent accommodations; non-discrimination and market access as primary treaty purposes rather than equal-status commitments between asymmetric partners. The two readings have the same ε-base (a real treaty with real obligations) but different structural relationships: the developmental reading prioritizes development-state beneficiaries and multinational-corporation targets; the market_access_reading prioritizes multinational-corporation beneficiaries and developing-state targets. These are two genuinely different constraints—two different epsilons—derived from the same kernel through different readings. They coexist as live institutional positions held by different state constituencies and different interpretation coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__developmental_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
