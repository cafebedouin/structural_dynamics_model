% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Norms as Hegemonic Extraction: Frozen Formal Revisability, Selective Enforcement
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the hegemonic-extraction reading of contested
 *   RBIO (Responsibility to Protect, Bretton Woods institutions, UN Charter
 *   sovereignty norms) as a frozen hegemonic project: formally revisable
 *   through amendment procedures, but practically un-amendable because P5
 *   veto power and institutional path-dependency prevent any change that
 *   would reduce hegemonic advantage. Selective enforcement reveals
 *   extractive intent—humanitarian interventions proceed when they serve
 *   Western interests, humanitarian crises go unaddressed when they do not.
 *   Conditionality imposed on Global South states is framed as development
 *   partnership but operates as coerced contract, extracting regulatory
 *   autonomy and financial value. This reading competes with a
 *   liberal-institutional reading (norms are universal and legitimately
 *   revisable) and a sovereignty-maximalist reading (RBIO norms are only
 *   legitimate when they protect state sovereignty). The three readings share
 *   a referent (the standing arrangement of international law and
 *   multilateral institutions) but assess its legitimacy very differently.
 *   This story author the hegemonic reading's structural claim: that formal
 *   universal principles mask hegemonic rent extraction, maintained through
 *   selective enforcement and institutional design that freezes the status
 *   quo.
 *
 * KEY AGENTS:
 *   - us_european_capital_interests: Primary beneficiary (institutional power, arbitrage exit, global scope) — extracts economic value through conditionality and debt servicing
 *   - permanent_security_council_states: Agenda-setters (institutional power, mobile exit, global scope) — hold veto over amendment, choose selective enforcement
 *   - global_south_states: Primary targets (moderate power, identity-locked exit, global scope) — cannot renegotiate because sovereignty is defined within the system they want to exit
 *   - populations_under_structural_adjustment: Secondary victims (powerless, trapped exit, global scope) — bear real costs of privatization and labor deregulation, no formal voice
 *   - international_legal_scholarly_community: Observers (analytical power) — produce competing readings; hegemonic reading is marginalized as 'non-mainstream'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.79).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.71).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Norms as Hegemonic Extraction: Frozen Formal Revisability, Selective Enforcement").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, 'ac6e4f43-c5c7-4269-b5d4-c7c1519efe89').
narrative_ontology:cs_kernel_codification('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', formalized).
narrative_ontology:cs_authority_grounding('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', extraction).
narrative_ontology:cs_interpretation_layer_present('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89').
narrative_ontology:cs_reading_relation('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', foundational, selective_enforcement_reveals_hegemonic_extraction).
narrative_ontology:cs_axiom_status(selective_enforcement_reveals_hegemonic_extraction, holdable).
narrative_ontology:cs_axiom_grounding('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', selective_enforcement_reveals_hegemonic_extraction, empirically_contingent).
narrative_ontology:cs_axiom('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', foundational, formal_universality_masks_extractive_selectivity).
narrative_ontology:cs_axiom_status(formal_universality_masks_extractive_selectivity, holdable).
narrative_ontology:cs_axiom_grounding('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', formal_universality_masks_extractive_selectivity, deontological).
narrative_ontology:cs_reference_frame('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', universal_international_law_and_sovereign_equality).
narrative_ontology:cs_drift_state('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', contemporary_selective_enforcement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ac6e4f43-c5c7-4269-b5d4-c7c1519efe89', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, permanent_security_council_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, populations_under_structural_adjustment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, aligned_global_south_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Capital flows (multinational corporations, financial institutions, development agencies) headquartered in and controlled by U.S. and European states benefit from RBIO norms that lock Global South states into predictable neoliberal frameworks, enable debt servicing, and create investment-friendly regulatory environments. They shape RBIO interpretation through formal and informal channels and benefit when enforcement is selective — applied to states that resist conditionality, withheld from aligned states. Exit would mean abandonment of extractive advantage.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests, agenda_setter).

% P5 states hold formal veto over Security Council action and substantive seat at every major RBIO institution. They set the rules and decide when they apply. They have capacity to amend the framework but choose not to (path-dependency, institutional lock-in, preference for the current arrangement as enabling selective enforcement). They benefit from the arrangement's ambiguity: formal revisability creates legitimacy cover; practical un-amenability preserves their discretion.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, permanent_security_council_states, agenda_setter,
    institutional, civilizational, mobile, global).

% Face RBIO-mandated structural adjustment conditions as the price of IMF/World Bank access, security guarantees, and international legitimacy. Cannot exit because sovereignty itself is defined within the RBIO framework they are locked into. Cannot renegotiate because formal amendment requires consensus or supermajority that P5 states control. Exit would mean international isolation, loss of credit access, delegitimization within the system. They resist through non-compliance, but enforcement is selective — those that comply get limited benefits; those that resist get punished.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    moderate, generational, identity_locked, global).

% Bear the costs of structural adjustment (reduced public spending, privatization, labor deregulation, subsidy removal) mandated by RBIO institutions as condition of sovereign state participation. They have no formal seat in RBIO governance. Their governments negotiated under duress (capital flight, currency crisis, debt service). Their alternatives are internal displacement (migration) or organized resistance that governments repress.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, populations_under_structural_adjustment, payer,
    powerless, biographical, trapped, global).

% Comply with RBIO conditionality and receive preferential access to capital, security support, and institutional positions within the framework. They extract rents by managing compliance and playing multiple patrons. Their populations still bear adjustment costs, but regime elites capture side benefits. They maintain power through alliance with hegemonic interests.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, aligned_global_south_regimes, beneficiary,
    moderate, generational, constrained, global).

% Formally seated but operationally constrained: secondary P5 members (Russia, China) can veto some actions but lack agenda-setting power for institutional amendment; rising powers (India, Brazil) lack formal P5 status and cannot block consensus. They would challenge RBIO legitimacy and the selective enforcement pattern if they had structural capacity. Their dissent is documented but structurally overridden.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, dissenting_p5_or_rising_powers, excluded,
    institutional, civilizational, constrained, global).

% Produces competing readings of RBIO norms and legitimacy. This reading (hegemonic extraction) competes with liberal-institutional readings (norms are universal, enforceable, revisable through legitimate process) and sovereignty-maximalist readings. Scholarly consensus is fractured; consensus is weaponized selectively to legitimate enforcement choices already made by power-holders.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, international_legal_scholarly_community, observer,
    analytical, generational, analytical, global).

% Documented evidence of RBIO enforcement selectivity and harms; have no formal RBIO seat. They advocate for amendment, transparency, and accountability but lack structural capacity to force change. Their advocacy is incorporated as theater (humanitarian language covers extractive enforcement) or marginalized as non-state interference. Exit would mean abandonment of advocacy channel.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, ngos_and_civil_society, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes formal rules for interstate conduct (sovereignty, non-intervention, humanitarian limits, debt relief, development partnership) that enable predictable international order and make bilateral negotiations possible without constant recourse to force.
% TRANSFER_FUNCTION: Moves economic value and regulatory capacity from Global South states to U.S./European capital through mechanisms nominally framed as development assistance: debt servicing obligations, conditionality-imposed privatization, labor market deregulation, and tax regimes that favor foreign investors. Moves legitimacy from states to international institutions, then concentrated governance within those institutions to P5 and Western-aligned states.
% ABSENT_VOICES: Populations subjected to structural adjustment have no formal RBIO seat. Dissenting or rising powers are structurally overridden despite formal participation. Alternative frameworks for interstate order (non-aligned movement, ALBA, Chinese Belt and Road) are systematically excluded from RBIO revision proposals. Scholars documenting the hegemonic reading are treated as 'non-mainstream' in formal RBIO discourse.
% DISAPPEARANCE_RATIONALE: If selective RBIO enforcement and conditionality architecture vanished, capital flows would reorganize around different terms; Global South debt burdens would renegotiate or default; alternative interstate institutions would gain legitimacy; U.S./European financial dominance would face direct competition; Global South political autonomy would expand radically. The world would undergo substantial institutional reorganization.
% FOUNDING_PROBLEM: Post-WWII interstate order needed rules to prevent total war, manage decolonization, and establish legitimate authority for international intervention. RBIO norms codified this as universal principles: sovereignty, non-intervention, territorial integrity, collective security, and later development cooperation.
% FOUNDING_PROBLEM_CORROBORATION: The liberal-institutional reading attests the founding problem remains live: state fragility, humanitarian crisis, terrorism, climate change require coordinated RBIO response. The hegemonic reading (this one) and scholarship from Global South states attest the founding problem was substantially solved (decolonization is complete, major wars have not recurred) and the RBIO framework persists as a vehicle for hegemonic rent extraction, not legitimate coordination. Academic work documenting conditionality harms (Stiglitz on structural adjustment, Easterly on development aid ineffectiveness) and enforcement selectivity (selective humanitarian intervention) supports the shifted-function diagnosis from outside the benefiting parties.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.79 at interval end) reflects sustained asymmetry: high extractiveness from Global South states and populations, with selective enforcement amplifying extraction—those who comply get minor benefits, those who resist face isolation or intervention. Suppression (0.71) reflects active enforcement machinery (sanctions regimes, conditional lending, selective humanitarian intervention) plus internalized compliance through institutional lock-in and identity-fusion: Global South elites have internalized RBIO framing as 'natural' or 'inevitable.' Theater ratio (0.62, rising from 0.41) indicates performative maintenance: formal amendment procedures exist but never amend anything; humanitarian language covers extractive enforcement; development partnerships are branded as aid rather than conditionality. Accessibility collapse (0.48, lower than for mountains/snares) reflects genuine alternatives available to wealthier states (exit through alternative institutions like BRICS, ASEAN+) but not to poorest states (locked in). Resistance (0.73) reflects documented opposition from Global South states, rising powers, and civil society—resistance is substantial but structurally overridden by institutional design. Measurement series show extractiveness and theater rising over the interval: the constraint has ratcheted as enforcement technology improved and alternative exit pathways were foreclosed (2008 financial crisis increased dependence; post-Cold War unipolarity eliminated countervailing power blocs).
 *
 * PERSPECTIVAL GAP:
 *   From the U.S./European capital and P5 seat: RBIO norms are legitimate universal principles that coordinate interstate order and enable development cooperation. From the Global South target seat: the same framework operates as hegemonic extraction disguised as universal law. From the analytical (scholarly) seat: both readings are structurally correct—the framework IS universal in form and hegemonic in effect. The engine computes per-seat directionality: beneficiary seats (U.S./European capital, aligned Global South regimes) get low d, near-zero effective extraction; payer seats (Global South states, populations) get high d, high effective extraction. The constraint looks like rope to the beneficiary; snare to the target. This perspectival gap is the entire point—the same institution produces opposite classifications from different positions, which is exactly how hegemonic extraction operates: as shared coordination that benefits some and extracts from others asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (d ≈ 0.15 for U.S./European capital): collects extraction without bearing costs; arbitrage-level exit capacity (could reorganize capital flows but chooses the current arrangement). Payer directionality (d ≈ 0.88 for Global South states): bears conditionality costs, cannot exit because sovereignty is defined within the system, trapped by institutional lock-in and identity-fusion (national sovereignty IS their seat at RBIO, so exit from RBIO is exit from recognized statehood). Secondary victims (populations) have even higher d ≈ 0.95 because they are powerless with trapped exit. This asymmetry in d-values is what drives the per-seat type divergence: the constraint computes as coordination or low-extraction rope from the beneficiary seats; tangled rope or snare from payer seats. The directionality overrides are not needed here because the structural derivation captures the asymmetry correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has resolved its founding mandate: post-WWII major-power war has not recurred; decolonization succeeded; state fragility and humanitarian crises are managed (imperfectly, selectively) rather than ignored. The founding problem was 'establish legitimate authority for interstate order and prevent hegemonic war.' That problem is solved. The constraint now persists primarily as a vehicle for hegemonic rent extraction—it is not maintained because coordination still requires it, but because the arrangement benefits powerful states. Mandatrophy is declared. However, the framework's legitimacy still rests on the founding problem (universal principles, development cooperation, humanitarian protection); the constraint cannot be explicitly reorganized as 'hegemonic rent extraction' without collapsing its legitimacy. It persists through theatrical maintenance: formal amendment procedures that never amend, humanitarian rhetoric that covers extractive enforcement, claims of universal principles that are selectively applied. The theater ratio rising from 0.41 to 0.62 models this drift: as the founding problem genuinely recedes, more enforcement activity goes to maintaining the system itself (performative universalism) rather than solving the coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_enforcement_as_extraction_or_capacity,
    'Is enforcement selectivity evidence of hegemonic extractive intent, or does it reflect capacity constraints and evolving normative judgment about which interventions are legitimate?',
    'Counterfactual analysis: compare enforcement patterns when Western interests diverge (e.g., Rwanda non-intervention when capacity existed). If interventions correlate with Western geopolitical interest rather than humanitarian need or capacity, extractive intent is supported. If enforcement follows stated humanitarian criteria despite capacity, capacity explanation is supported.',
    'If extractive intent is confirmed, the constraint classifies as snare or tangled_rope from payer seats (currently tangled_rope claim). If capacity explanation holds, the constraint moves toward rope classification and mandatrophy is not declared. This omega directly determines whether theater ratio accurately models extractive performativity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_as_extraction_or_capacity, empirical, 'Whether RBIO enforcement selectivity reveals hegemonic extraction or reflects legitimate capacity/judgment constraints.').

omega_variable(
    formal_revisability_vs_practical_amendment,
    'Is the formal revisability of RBIO norms genuine, or is P5 veto a practical barrier to any amendment that reduces hegemonic advantage?',
    'Examine historical amendment attempts: identify amendments blocked by P5 veto and assess whether they would have reduced Western advantage. If amendments reducing Western advantage are systematically blocked while others pass, the practical barrier is confirmed. If blocked amendments were also opposed on non-hegemonic grounds (sovereignty, cost), the barrier is neutral.',
    'If P5 veto systematically prevents hegemonic-reducing amendments, the constraint''s ''frozen'' character is confirmed and mandatrophy is supported (founding problem solved but amendment blocked). If blocks are content-neutral, the constraint retains rope-status flexibility. This omega determines whether theater includes false-revision procedures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_revisability_vs_practical_amendment, empirical, 'Whether RBIO norms are formally revisable or practically frozen through hegemonic P5 control.').

omega_variable(
    alternative_rbio_frameworks_legitimacy,
    'Are alternative international law frameworks (non-aligned movement norms, ALBA, Chinese Belt and Road governance structures, etc.) structurally viable as replacements for RBIO, or are they fundamentally constrained by power asymmetries that make RBIO inescapable?',
    'Track institutional success of alternatives: BRICS institutions, ASEAN+3, Shanghai Cooperation Organization. If they acquire governance capacity for dispute resolution, collective action, and enforcement comparable to RBIO, viability is increasing. If they remain subordinate or geographically limited, RBIO lock-in is confirmed.',
    'If alternatives are genuinely viable, the accessibility_collapse should be lower and exit options for Global South states upgrade from identity_locked toward constrained or mobile. If RBIO remains inescapable, accessibility_collapse and identity_locked stay high. This omega determines whether the constraint is trap or merely asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_rbio_frameworks_legitimacy, empirical, 'Whether genuine exit alternatives to RBIO exist for Global South states or whether RBIO lock-in is structural.').

omega_variable(
    liberal_vs_hegemonic_reading_kernel_foreclosure,
    'Do the liberal-institutional reading and the hegemonic-extraction reading logically foreclose each other, or can both coexist as live positions within different actors'' frameworks?',
    'Examine policy coherence: do Western states'' RBIO rhetoric (we believe in universal principles) align with their voting/veto patterns (we systematically block amendments that reduce our advantage)? If the contradiction persists stably over decades without resolution, coexistence is the structural reality. If pressure accumulates toward explicit choice, foreclosure may eventually occur.',
    'If the readings logically foreclose each other, one must be abandoned and the other becomes canonical (the winning reading''s hegemonic class wins the narrative). If they coexist, the constraint persists in contested state, with performative maintenance of both readings. This omega determines long-term trajectory: collapse toward one reading or permanent contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liberal_vs_hegemonic_reading_kernel_foreclosure, conceptual, 'Whether liberal-institutional and hegemonic-extraction readings of RBIO norms logically foreclose each other or can coexist indefinitely.').

omega_variable(
    identity_locked_vs_contingent_sovereignty,
    'Is Global South state exit-optionality genuinely identity-locked (sovereignty defined within RBIO framework such that exit = loss of recognized statehood), or is the lock contingent on current power asymmetries and could be broken through coalition-building with rising powers?',
    'Monitor coalition dynamics: if BRICS or Global South states increasingly exit RBIO instruments (reject IMF conditionality, establish alternative development banks) without losing recognized statehood or international standing, identity-lock is contested. If exit remains unthinkable and costs are applied (isolation, sanctions, legitimacy loss), identity-lock is confirmed.',
    'If identity-lock is genuine, Global South states remain trapped (high d, high extraction). If contingent, exit options upgrade and effective extraction declines as alternatives become viable. This omega determines whether the constraint is truly snare-like (inescapable) or tangled-rope-like (escape possible but difficult).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_contingent_sovereignty, empirical, 'Whether Global South state sovereignty is identity-locked to RBIO framework or contingently dependent on current power structure.').

omega_variable(
    founding_problem_obsolescence_contested,
    'Has the founding problem (legitimate interstate order, prevention of hegemonic war) been solved, or does it remain live, and what evidence settles this?',
    'Track incidence of major-power war, state fragility leading to humanitarian crisis, and institutional response effectiveness. If no major-power war since 1945, state fragility is managed without collapse into warfare, and humanitarian crises are addressed (even if selectively), the founding problem is substantially solved. If major-power conflict risk is rising or humanitarian crisis management is failing, the problem is live.',
    'If founding problem is dead, mandatrophy is strongly supported and theater-increase models performative maintenance. If still live, mandatrophy may be premature and some of the constraint''s persistence is justified by actual coordination need. This omega determines whether the constraint is extractive overlay (dead mandate) or genuine coordination with extractive overlay (live mandate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_contested, empirical, 'Whether RBIO norms'' founding problem (legitimate international order, prevention of hegemonic war) has been solved or remains live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0, 0.41).
narrative_ontology:measurement(rbio_tr_t5, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rbio_tr_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(rbio_tr_t20, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 20, 0.57).
narrative_ontology:measurement(rbio_tr_t25, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 25, 0.61).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(rbio_tr_t35, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 35, 0.62).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 0, 0.61).
narrative_ontology:measurement(rbio_be_t5, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(rbio_be_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(rbio_be_t20, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(rbio_be_t25, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement(rbio_be_t35, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 35, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(rbio_su_t5, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(rbio_su_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(rbio_su_t20, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(rbio_su_t25, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(rbio_su_t35, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 35, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(rbio_grid_01, rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(rbio_grid_02, rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse(class), 35, 0.45).
narrative_ontology:measurement(rbio_grid_03, rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse(individual), 0, 0.31).
narrative_ontology:measurement(rbio_grid_04, rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse(individual), 35, 0.29).
narrative_ontology:measurement(rbio_grid_05, rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse(organizational), 0, 0.61).
narrative_ontology:measurement(rbio_grid_06, rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse(organizational), 35, 0.58).
narrative_ontology:measurement(rbio_grid_07, rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse(structural), 0, 0.85).
narrative_ontology:measurement(rbio_grid_08, rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse(structural), 35, 0.82).
narrative_ontology:measurement(rbio_grid_09, rbio_practice_norm_complex__hegemonic_extraction_reading, resistance(class), 0, 0.78).
narrative_ontology:measurement(rbio_grid_10, rbio_practice_norm_complex__hegemonic_extraction_reading, resistance(class), 35, 0.76).
narrative_ontology:measurement(rbio_grid_11, rbio_practice_norm_complex__hegemonic_extraction_reading, resistance(individual), 0, 0.52).
narrative_ontology:measurement(rbio_grid_12, rbio_practice_norm_complex__hegemonic_extraction_reading, resistance(individual), 35, 0.51).
narrative_ontology:measurement(rbio_grid_13, rbio_practice_norm_complex__hegemonic_extraction_reading, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(rbio_grid_14, rbio_practice_norm_complex__hegemonic_extraction_reading, resistance(organizational), 35, 0.73).
narrative_ontology:measurement(rbio_grid_15, rbio_practice_norm_complex__hegemonic_extraction_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(rbio_grid_16, rbio_practice_norm_complex__hegemonic_extraction_reading, resistance(structural), 35, 0.58).
narrative_ontology:measurement(rbio_grid_17, rbio_practice_norm_complex__hegemonic_extraction_reading, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(rbio_grid_18, rbio_practice_norm_complex__hegemonic_extraction_reading, stakes_inflation(class), 35, 0.65).
narrative_ontology:measurement(rbio_grid_19, rbio_practice_norm_complex__hegemonic_extraction_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(rbio_grid_20, rbio_practice_norm_complex__hegemonic_extraction_reading, stakes_inflation(individual), 35, 0.48).
narrative_ontology:measurement(rbio_grid_21, rbio_practice_norm_complex__hegemonic_extraction_reading, stakes_inflation(organizational), 0, 0.64).
narrative_ontology:measurement(rbio_grid_22, rbio_practice_norm_complex__hegemonic_extraction_reading, stakes_inflation(organizational), 35, 0.71).
narrative_ontology:measurement(rbio_grid_23, rbio_practice_norm_complex__hegemonic_extraction_reading, stakes_inflation(structural), 0, 0.72).
narrative_ontology:measurement(rbio_grid_24, rbio_practice_norm_complex__hegemonic_extraction_reading, stakes_inflation(structural), 35, 0.78).
narrative_ontology:measurement(rbio_grid_25, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression(class), 0, 0.54).
narrative_ontology:measurement(rbio_grid_26, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression(class), 35, 0.61).
narrative_ontology:measurement(rbio_grid_27, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression(individual), 0, 0.38).
narrative_ontology:measurement(rbio_grid_28, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression(individual), 35, 0.44).
narrative_ontology:measurement(rbio_grid_29, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(rbio_grid_30, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression(organizational), 35, 0.68).
narrative_ontology:measurement(rbio_grid_31, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression(structural), 0, 0.68).
narrative_ontology:measurement(rbio_grid_32, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression(structural), 35, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.18).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, bretton_woods_structural_adjustment_conditionality).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_veto_structure_and_amendment_paralysis).

% DUAL FORMULATION NOTE:
% The RBIO norms kernel decomposes into three structurally distinct constraint stories instantiating three readings: (1) hegemonic-extraction reading (this story): norms are frozen hegemonic project, maintained through selective enforcement and path-dependency. (2) liberal-institutional reading: norms are universal and legitimately revisable through multilateral process. (3) sovereignty-maximalist reading: norms legitimate only when protecting state sovereignty. Each reading produces a different ε-referent assessment: hegemonic extraction focuses on enforcement selectivity and conditionality; liberal institutionalism focuses on capacity constraints; sovereignty maximalism focuses on consent. The three constraints are linked through network.affects_constraints to enable contention modeling and comparative analysis of the same kernel under different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
