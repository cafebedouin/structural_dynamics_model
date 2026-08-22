% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default: Hybrid Amnesia Reading (Lapsed Closure Enabling Beneficiary Capture)
 *   domain: political_economy/economic_history/ideology_studies
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the kernel 'market as natural
 *   default': the hybrid amnesia reading, which partitions the 1930-present
 *   record into two stages. Stage one (roughly 1930-1978) is lapsed closure:
 *   the interwar and wartime repertoire of economic alternatives — indicative
 *   planning, capital controls, cooperative credit, guild and commons
 *   traditions — genuinely dropped out of institutional memory as the
 *   generations that ran them retired and the planning apparatuses were
 *   quietly dismantled inside otherwise prosperous welfare states. No one had
 *   to suppress what no one remembered. Stage two (1979-present) is
 *   beneficiary capture: incumbent asset holders, finding the field cleared,
 *   built an active rationalization apparatus — funded networks, curricula,
 *   editorial framings — that converted inherited amnesia into explicit
 *   doctrine ('there is no alternative') and shielded a growing rent
 *   structure behind it. The ε referent throughout is the standing
 *   arrangement under contest — the naturalized market order as it actually
 *   operates — assessed by this reading's own lights; the readings' endorsed
 *   alternatives are never the referent. Sibling readings (pure lapse; pure
 *   active maintenance) are separate constraints in separate files; this
 *   story hedges nothing across them. KEY AGENTS (by structural
 *   relationship): - incumbent_capital_holders: Primary beneficiary
 *   (powerful/arbitrage) — asset returns shielded by the naturalized frame -
 *   financial_services_industry: Secondary beneficiary and co-agenda-setter
 *   (institutional/arbitrage) - large_multinational_corporations: Beneficiary
 *   (institutional/arbitrage) - market_liberal_think_tanks: Rationalization
 *   producer (organized/mobile) - mainstream_economics_departments: Epistemic
 *   gatekeeper (institutional/identity_locked) - organized_labor: Primary
 *   payer (organized/constrained) - austerity_exposed_populations: Payer
 *   (moderate/trapped) - global_south_debtor_states: Payer (moderate/trapped)
 *   - heterodox_economists: Excluded objectors holding the documentary memory
 *   (moderate/constrained) - democratic_electorates: Dual-positioned
 *   payer/beneficiary (organized/constrained) - economic_historians:
 *   Analytical observer (moderate/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.65).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default: Hybrid Amnesia Reading (Lapsed Closure Enabling Beneficiary Capture)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/economic_history/ideology_studies").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '9556ca55-35f8-4f4d-ab0c-6695c9febe09').
narrative_ontology:cs_kernel_codification('9556ca55-35f8-4f4d-ab0c-6695c9febe09', implicit).
narrative_ontology:cs_authority_grounding('9556ca55-35f8-4f4d-ab0c-6695c9febe09', expertise).
narrative_ontology:cs_interpretation_layer_present('9556ca55-35f8-4f4d-ab0c-6695c9febe09').
narrative_ontology:cs_reading_relation('9556ca55-35f8-4f4d-ab0c-6695c9febe09', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('9556ca55-35f8-4f4d-ab0c-6695c9febe09', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('9556ca55-35f8-4f4d-ab0c-6695c9febe09', foundational, lapsed_closure_precedes_capture).
narrative_ontology:cs_axiom_status(lapsed_closure_precedes_capture, holdable).
narrative_ontology:cs_axiom_grounding('9556ca55-35f8-4f4d-ab0c-6695c9febe09', lapsed_closure_precedes_capture, empirically_contingent).
narrative_ontology:cs_axiom('9556ca55-35f8-4f4d-ab0c-6695c9febe09', foundational, amnesia_functions_as_extractive_resource).
narrative_ontology:cs_axiom_status(amnesia_functions_as_extractive_resource, holdable).
narrative_ontology:cs_axiom_grounding('9556ca55-35f8-4f4d-ab0c-6695c9febe09', amnesia_functions_as_extractive_resource, empirically_contingent).
narrative_ontology:cs_axiom('9556ca55-35f8-4f4d-ab0c-6695c9febe09', secondary, rationalization_phase_is_defensive_not_originary).
narrative_ontology:cs_axiom_status(rationalization_phase_is_defensive_not_originary, holdable).
narrative_ontology:cs_axiom_grounding('9556ca55-35f8-4f4d-ab0c-6695c9febe09', rationalization_phase_is_defensive_not_originary, empirically_contingent).
narrative_ontology:cs_reference_frame('9556ca55-35f8-4f4d-ab0c-6695c9febe09', amnesiac_market_commonsense).
narrative_ontology:cs_drift_state('9556ca55-35f8-4f4d-ab0c-6695c9febe09', contemporary_post_crisis_pluralism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9556ca55-35f8-4f4d-ab0c-6695c9febe09', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, financial_services_industry).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, large_multinational_corporations).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, market_liberal_think_tanks).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, organized_labor).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, austerity_exposed_populations).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, global_south_debtor_states).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, heterodox_economists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, democratic_electorates).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, democratic_electorates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the asset base whose returns depend on existing property, contract, and capital-flow arrangements. After 1980 they inherited a policy field in which the pre-war repertoire of alternatives — planning organs, capital controls, cooperative credit — had already dropped out of institutional memory, and they fund research, media, and campaign infrastructure that presents the resulting order as the natural form of economic life. Capital moves across jurisdictions, so no single government's re-regulation threatens their position.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Intermediates the asset economy and collects fees and spreads that expand as financial arrangements multiply. Through lobbying, revolving-door personnel, and sponsorship of policy research it shapes which interventions count as technically respectable. Its balance sheets are internationally mobile.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, financial_services_industry, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, financial_services_industry, agenda_setter).

% Operate supply chains and tax positions premised on the existing trade and investment regime. When the regime is described as natural rather than chosen, proposals to renegotiate it read as interference with nature rather than as policy choices with historical precedents. They retain the option of relocating production or incorporation.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, large_multinational_corporations, beneficiary,
    institutional, biographical, arbitrage, global).

% Produce the op-eds, policy briefs, and curricula that translate the inherited assumption into explicit doctrine. Staffed by career intellectuals whose advancement depends on the network that funds them; their output supplies legislators and journalists with ready-made framings. Individuals move between such organizations, governments, and universities.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, market_liberal_think_tanks, agenda_setter,
    organized, generational, mobile, continental).

% Train the economists who staff central banks, ministries, and international agencies, and control journal publication and hiring. The discipline's self-understanding fused with the naturalized framework over the decades in which rival approaches lost departmental footholds; senior careers were built inside it. Individual departure carries the cost of leaving the profession one was formed by.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, mainstream_economics_departments, agenda_setter,
    institutional, generational, identity_locked, global).

% Represents workers whose bargaining position deteriorated as the frame foreclosed industrial policy, sectoral bargaining support, and public employment as ordinary tools. Union density and strike capacity vary by country, but the repertoire of demands shrank everywhere as proposals outside the frame came to look naive rather than radical. Members cannot individually exit the labor market.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, organized_labor, payer,
    organized, biographical, constrained, national).

% Live in regions dependent on public employment and services that spending caps cut. Cuts arrive described as arithmetic necessity rather than as choices among remembered alternatives, which removes the addressable decision-maker. Moving away means leaving housing networks, family care, and local work.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, austerity_exposed_populations, payer,
    moderate, immediate, trapped, regional).

% Govern countries whose access to credit was made conditional on liberalization packages presented as technical requirements rather than negotiable terms. Planning ministries and development banks were dismantled as conditions; rebuilding them invites capital flight and rating downgrades. Exit from the creditor framework has few historical examples.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, global_south_debtor_states, payer,
    moderate, generational, trapped, continental).

% Maintain research programs on planning, post-Keynesian, institutional, and dependency economics — the documentary memory of the forgotten alternatives. They publish in lower-ranked journals, hold minority departmental positions, and are largely absent from the committees and editorial boards where policy-relevant economics is certified. Leaving the fringe means abandoning the archive they keep.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, heterodox_economists, excluded,
    moderate, generational, constrained, global).

% Vote within a menu of parties that mostly share the naturalized premise, so the choice of economic constitution is rarely on the ballot. They receive the consumer benefits of market provision while bearing the costs of arrangements — housing scarcity, precarious work, privatized risk — that no electoral channel currently offers to renegotiate.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, democratic_electorates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, democratic_electorates, beneficiary).

% Document the sequence by which interwar debates, wartime planning capacity, and early postwar experiments dropped from professional and public memory, and date the subsequent rationalization. They hold no lever over current policy and write for audiences that include the disciplines they study.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Market allocation genuinely solves the economic calculation and information problem: prices aggregate dispersed knowledge and coordinate production and consumption decisions across millions of actors without central command. The naturalization rides on this real function — the frame stabilizes expectations and lowers contestation over basic economic arrangements.
% TRANSFER_FUNCTION: Moves bargaining power, surplus, and agenda-setting authority from labor, debtor states, and democratic institutions toward asset holders and financial intermediaries; and moves the memory of alternative arrangements out of public reach — from everyone, to the archive.
% ABSENT_VOICES: Heterodox economists, economic historians of the planning-era experiments, labor representatives, and Global South policymakers are structurally absent from the venues where 'economic necessity' is adjudicated — central banks, finance ministries, editorial boards, leading departments. Present, they would testify that the naturalized frame is one historically contingent arrangement among several, with a documented replacement repertoire.
% DISAPPEARANCE_RATIONALE: If the naturalization vanished overnight, economic policy debate would reopen: capital controls, industrial policy, public options, and cooperative structures would re-enter the feasible set as policies with precedents rather than as nostalgia; asset prices carrying rent protection would adjust; the discipline's monopoly over economic common sense would break. Arrangements across finance, trade, welfare, and academia currently depend on the frame staying closed.
% FOUNDING_PROBLEM: Restoring policy coherence and investment confidence after the breakdown of the postwar Keynesian settlement amid 1970s stagflation — accomplished by presenting the restored market order as nature rather than as one reconstructible choice, with the already-lapsed memory of alternatives supplying the raw material.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians working outside the beneficiary network corroborate the stagflation-legitimation genealogy through archival studies of the 1970s policy turn; retrospective memoirs by former central bankers and finance ministers, including self-described architects, attest the problem-framing contemporaneously. Attestation that the founding problem remains live today comes only from the arrangement's own beneficiaries; no outside source corroborates continued liveness.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction ends at 0.45: substantial rents are shielded by the frame (financial spreads, land and asset appreciation, monopoly pricing read as market outcomes), but the genuine price-coordination function beneath the naturalization caps the ceiling — this is not a pure tollbooth. Suppression is 0.65 and epistemic-institutional in kind: journal gatekeeping, funding asymmetries, career risk, and capital-flight discipline, not physical coercion. Theater is 0.47 at interval end — approaching half of maintenance activity is defensive performance (natural-law metaphors, inevitability rhetoric, post-2008 'austerity is arithmetic' framing) while the textbook-and-journal machinery does real gatekeeping work alongside it. Accessibility_collapse is 0.55: the alternatives collapsed from living memory but are recoverable — the archives survive, heterodox programs persist, and Nordic, French, and Malaysian variants ran into recent decades — so the collapse is deep but not total. Resistance is 0.55: Occupy, anti-austerity movements, post-2008 pluralist economics, and the industrial-policy revival are recurring and real, but have so far failed to displace the default. The measurement series share one grid (t=0..95, roughly 1930..2025) and show the reading's signature shape: a flat, low plateau through t≈48 (genuine-forgetting era, ε 0.12→0.24), a knee at t≈56 (capture onset, Thatcher/Reagan years), then sustained climb with visible deceleration after t≈80 (the 2008 crack partially re-sealed, slowing but not reversing accumulation). The claimed type (tangled_rope) is authored from structure — real coordination function, asymmetric extraction through the same frame, active enforcement — independently of these metric values.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the asset-holder seat the frame is experienced as nature and freedom — the arrangement asks nothing of them and explains everything to them. From the labor and debtor seats the same frame operates as closure: a shrinking menu presented as the only menu. From the mainstream-economics seat it is experienced as science — the identity-lock means the frame is not perceived as an arrangement at all but as the discipline's own competence. The historian seat sees contingency and dating. Same-level lateral divergence is sharpest between market_liberal_think_tanks and heterodox_economists: two epistemic actors of comparable nominal power, differentiated by funding flow, venue access, and exit — the former mobile and resourced, the latter constrained and archiving what the former profits from forgetting.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidy end of d, amplified by arbitrage-grade exit: capital holders, financial intermediaries, and multinationals can relocate away from any single jurisdiction's re-regulation, placing them nearest the beneficiary pole. Declared victims sit near the target end, pushed further by trapped or constrained exit: debtor states face capital-flight discipline, austerity-exposed populations face immobile housing and care networks, labor cannot exit the labor market, and heterodox economists cannot exit their archive without losing it. Democratic electorates derive near-symmetric treatment — genuine consumer benefits against foreclosed constitutional choice. The two agenda-setting epistemic seats (think tanks, departments) appear in neither declaration array, so the engine's fallback governs them; qualitatively they are captured maintainers — think tanks via funding dependence, departments via identity fusion — and the commentary flags them for override review if the fallback misplaces them. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope (global scope modestly amplifies effective extraction on the trapped victim seats).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-stagflation legitimation of a restored market order — was substantially addressed by the mid-1990s: inflation was broken, the Washington Consensus was installed, and the frame hardened into common sense. What persists past that point increasingly serves rent protection rather than the original stabilization purpose, but the status is honestly contested because inflation and state-capacity failures recur and beneficiaries cite them as proof of continuing liveness. The hybrid reading is precisely what prevents mandatrophy misclassification in both directions: a pure-snare reading would erase the genuine price-coordination function and the genuine first-stage lapse (nothing was being enforced while nobody remembered); a pure-rope reading would erase the post-1980 capture and the enforcement machinery that now holds the frame shut. Tangled_rope holds both facts. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): no dead-but-persistent zombie flag fires, matching the honest state — the mandate is disputed, not dead, and the world genuinely depends on the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition,
    'Does the two-stage hybrid decomposition (genuine lapse 1930-1978, then beneficiary capture 1979-present) correctly partition the history, versus the pure active-maintenance account or the pure-lapse account?',
    'Archival study of 1970s-80s funder strategy documents, Mont Pelerin network records, and planning-office closure files, distinguishing engineered memory loss from organic attrition; cross-checked against the timing of enforcement-machinery buildout.',
    'A pure-maintenance verdict shifts the ε rise earlier and steeper and collapses this reading toward the sibling maintenance account; a pure-lapse verdict makes the post-1980 rise diffusion rather than capture, lowering effective extraction estimates across all victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_partition, empirical, 'Partition of the amnesia history between engineered and organic components — the load-bearing uncertainty of this reading.').

omega_variable(
    initial_forgetting_genuineness,
    'Was the 1930s-1970s disappearance of alternatives from institutional memory genuinely unlapsed transmission, or already partly managed from the start?',
    'History of economics curricula and hiring, closure records of wartime and postwar planning bodies, and oral-history projects with retired planners and civil servants.',
    'If managed from the start, stage-one ε rises well above 0.20 and the reading''s two-stage axiom loses its foundation; if genuine, the low early plateau in the measurement series stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(initial_forgetting_genuineness, empirical, 'Genuineness of the first-stage forgetting versus covert early engineering.').

omega_variable(
    counterfactual_alternative_viability,
    'Could the forgotten alternatives — indicative planning, capital controls, cooperative credit — have remained viable at scale under postwar conditions, or were they obsolete independent of anyone''s memory?',
    'Comparative institutional analysis of economies that retained variants into recent decades: Nordic wage-earner funds, French indicative planning to the 1980s, Malaysian capital controls in 1998, and their performance against matched liberalizing peers.',
    'If the alternatives were structurally obsolete, part of the measured extraction is accurate description rather than rent-shielding and ε falls toward rope territory; if they were viable, the amnesia foreclosed live options and ε rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_alternative_viability, conceptual, 'Whether the foreclosed menu contained workable items — determines how much of the frame''s closure counts as loss versus correction.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the current suppression of alternatives carried by external machinery (funding, gatekeeping, career risk) or by internalized belief (economists and publics sincerely holding the frame)?',
    'Post-barrier trajectory: track heterodox argument uptake where gatekeeping barriers lifted — open-access journals, post-2008 pluralist curricula, student movements for curriculum reform — and measure whether traction persists without external scaffolding.',
    'A high internalized share means dismantling the external machinery alone leaves the frame intact, shifting the enforcement picture from organizational to cultural persistence and raising the true cost of fixing the constraint above the structural estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized component of the frame''s current suppression of alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mark_tr_t16, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(mark_tr_t32, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(mark_tr_t48, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 48, 0.2).
narrative_ontology:measurement(mark_tr_t56, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 56, 0.3).
narrative_ontology:measurement(mark_tr_t64, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 64, 0.36).
narrative_ontology:measurement(mark_tr_t72, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 72, 0.4).
narrative_ontology:measurement(mark_tr_t80, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement(mark_tr_t88, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 88, 0.46).
narrative_ontology:measurement(mark_tr_t95, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 95, 0.47).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(mark_be_t16, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement(mark_be_t32, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 32, 0.2).
narrative_ontology:measurement(mark_be_t48, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 48, 0.24).
narrative_ontology:measurement(mark_be_t56, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 56, 0.31).
narrative_ontology:measurement(mark_be_t64, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 64, 0.36).
narrative_ontology:measurement(mark_be_t72, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 72, 0.39).
narrative_ontology:measurement(mark_be_t80, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement(mark_be_t88, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 88, 0.44).
narrative_ontology:measurement(mark_be_t95, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 95, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(mark_su_t16, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 16, 0.26).
narrative_ontology:measurement(mark_su_t32, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 32, 0.32).
narrative_ontology:measurement(mark_su_t48, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 48, 0.38).
narrative_ontology:measurement(mark_su_t56, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 56, 0.48).
narrative_ontology:measurement(mark_su_t64, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 64, 0.54).
narrative_ontology:measurement(mark_su_t72, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 72, 0.58).
narrative_ontology:measurement(mark_su_t80, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 80, 0.61).
narrative_ontology:measurement(mark_su_t88, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 88, 0.63).
narrative_ontology:measurement(mark_su_t95, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 95, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the market is natural' decomposes per the ε-invariance principle into three structurally distinct causal accounts, each with its own ε, beneficiaries, and enforcement profile. This file (hybrid_amnesia_reading, ε 0.45) asserts lapse-then-capture; lapsed_alternative_reading (lower ε, no capture stage, weaker enforcement) asserts forgetting alone; beneficiary_maintained_reading (higher ε, enforcement throughout) asserts active maintenance from the outset. The upstream/downstream structure runs from the lapse account to the hybrid account (the hybrid inherits and extends the lapse claim) and from the hybrid account to the maintenance account (the hybrid's stage-two evidence is the maintenance account's core mechanism, dated later). Each story links the other two via affects_constraints; the disagreement is a partition of one historical record, not a difference of observables, so no further decomposition is warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
