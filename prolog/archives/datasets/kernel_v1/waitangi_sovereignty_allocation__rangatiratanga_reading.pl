% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Waitangi Treaty Article II: Rangatiratanga (Tino Authority) Reading
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The rangatiratanga reading of Article II of the Treaty of Waitangi claims
 *   that Māori signatories retained 'tino rangatiratanga' (full authority,
 *   unqualified chieftainship) over their lands, forests, fisheries, and all
 *   taonga (treasured things), while the Crown gained only 'kāwanatanga'
 *   (governorship) over European settlers — a limited jurisdiction over the
 *   settler population, not dominion over Māori or their territories. This
 *   reading directly forecloses the dominant crown_sovereignty_reading (which
 *   treats the Crown as acquiring full territorial and jurisdictional
 *   authority) and coexists with the partnership_reading (which frames the
 *   arrangement as intended co-equal partnership subject to negotiation and
 *   evolution). The rangatiratanga reading is instantiated as a constraint on
 *   contemporary governance: it establishes a claim-structure where Māori
 *   authorities (iwi, hapū) possess inherent decision-making power over
 *   resource management, cultural transmission, and territorial governance,
 *   while Crown authority is residual and conditional on Māori consent in
 *   Māori-designated territories. The constraint exhibits all six DR types
 *   depending on observer position: landless iwi see pure extraction (Snare)
 *   because they cannot exercise rangatiratanga while Crown disputes their
 *   authority; organized iwi with recognized territories see coordination
 *   mixed with extraction (Tangled Rope); recognized iwi authorities see
 *   functional coordination (Rope); Crown sees administrative disruption and
 *   lost jurisdictional clarity (Tangled Rope); settlers in transition zones
 *   see temporary uncertainty (Scaffold); and a universal legal principle
 *   view naturalizes Crown sovereignty as inevitable (Mountain, false
 *   summit). The measurement trajectory shows declining suppression and
 *   extractiveness over 185 years (1840–2026) as settlement acts, Waitangi
 *   Tribunal findings, and co-governance arrangements have operationalized
 *   increasing aspects of the reading, but theater ratio rising indicates
 *   growing performative content as the gap widens between treaty commitment
 *   and full implementation.
 *
 * KEY AGENTS:
 *   - Iwi collectives and hapū: Primary beneficiary (organized/constrained, regional scope) — collective entities claiming and exercising rangatiratanga authority over rohe; structural beneficiaries from reading's legal force
 *   - Landless or title-disputed iwi: Primary victim (powerless/trapped, national scope) — communities whose territory claims are contested; bears suppression without capacity to exercise counter-authority
 *   - Crown administrative apparatus: Secondary beneficiary and victim (institutional/constrained, national scope) — benefits from kāwanatanga clarity but loses unilateral authority over Māori territories; mixed structural relationship
 *   - Non-Māori settler population: Tertiary affected (powerful/mobile, regional scope in Māori territories, national scope in Crown-designated areas) — positioned as subjects of kāwanatanga (Crown's authority) rather than Māori authority; mobile exit via relocation or adaptation
 *   - Waitangi Tribunal and courts: Mediating institutional actors (institutional/arbitrage, national scope) — interpret the reading's scope and enforce/deny specific claims; arbitrage position via interpretive authority
 *   - Analytical observer: Civilizational perspective (analytical/analytical, universal scope) — risks naturalizing Crown sovereignty as legal necessity rather than contingent Westphalian framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.58).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.68).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Waitangi Treaty Article II: Rangatiratanga (Tino Authority) Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, 'c29e4b71-8c09-4619-885a-32984129e5e3').
narrative_ontology:cs_kernel_codification('c29e4b71-8c09-4619-885a-32984129e5e3', fixed_text).
narrative_ontology:cs_authority_grounding('c29e4b71-8c09-4619-885a-32984129e5e3', lineage).
narrative_ontology:cs_interpretation_layer_present('c29e4b71-8c09-4619-885a-32984129e5e3').
narrative_ontology:cs_reading_relation('c29e4b71-8c09-4619-885a-32984129e5e3', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('c29e4b71-8c09-4619-885a-32984129e5e3', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('c29e4b71-8c09-4619-885a-32984129e5e3', foundational, maori_retain_tino_rangatiratanga_1840).
narrative_ontology:cs_axiom_status(maori_retain_tino_rangatiratanga_1840, holdable).
narrative_ontology:cs_axiom_grounding('c29e4b71-8c09-4619-885a-32984129e5e3', maori_retain_tino_rangatiratanga_1840, deontological).
narrative_ontology:cs_axiom('c29e4b71-8c09-4619-885a-32984129e5e3', foundational, kawanatanga_limited_to_pakeha_jurisdiction).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_pakeha_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('c29e4b71-8c09-4619-885a-32984129e5e3', kawanatanga_limited_to_pakeha_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('c29e4b71-8c09-4619-885a-32984129e5e3', maori_tino_rangatiratanga_baseline).
narrative_ontology:cs_drift_state('c29e4b71-8c09-4619-885a-32984129e5e3', contemporary_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c29e4b71-8c09-4619-885a-32984129e5e3', '2026-02-26T14:32:17Z').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, iwi_collectives).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, hapu_governance_structures).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_administrative_jurisdiction).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_political_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LANDLESS IWI (SNARE) — Iwi without confirmed land title under rangatiratanga reading face full extraction: they remain subject to Crown jurisdictional assertion while unable to exercise tino authority. Exit is impossible — the dispute over which authority framework applies means Māori communities bear the suppression of dual-claim uncertainty without capacity to resolve it unilaterally. Maximum structural extraction.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__rangatiratanga_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RECOGNIZED IWI WITH ROHE (TANGLED ROPE) — Iwi with established tribal territories under Te Ture Whenua Act and settlement acts exercise genuine coordination function (managing whenua, fisheries, cultural taonga) AND face significant extraction: residual Crown control over resource consents, conservation decisions, and veto over certain economic developments. Constrained exit — can organize collectively but face high costs to challenge Crown jurisdiction in each domain.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RANGATIRATANGA-EXERCISING IWI AUTHORITY (ROPE) — Iwi bodies (Iwi Authorities, Mandated Iwi Organizations) that have achieved Crown recognition under this reading exercise genuine tino authority: they coordinate resource allocation, cultural transmission, and governance through their own whakapapa (genealogical frameworks) and kaupapa (values). Arbitrage exit — can allocate across domains. Net beneficiary from this reading's institutionalization.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__rangatiratanga_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CROWN ADMINISTRATIVE APPARATUS (TANGLED ROPE) — The Crown legitimately coordinates settler polity (rule of law, infrastructure, public goods) AND faces extraction from the rangatiratanga reading: jurisdictional fragmentation, loss of unified legal authority, requirement for co-governance or consent in Māori territories creates administrative complexity and reduces Crown's unilateral policy capacity. Constrained exit — can renegotiate arrangements but cannot unilaterally override treaty obligations without international reputational cost.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NON-MĀORI SETTLER POPULATION (SCAFFOLD) — Settler communities in Māori territories experience the rangatiratanga reading as temporary jurisdictional uncertainty with a sunset: as Crown co-governance and Māori resource control mature, settlers' legal position becomes clarified (as resident subjects of dual authority or as guests in Māori-governed spaces, depending on land status). Low effective extraction because settlement in these areas is structurally temporary pending clarification — settlers can relocate or adapt. Theater is moderate: the actual governance transition is functional, not purely performative.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__rangatiratanga_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: UNIVERSAL LEGAL SOVEREIGNTY PRINCIPLE (MOUNTAIN, FALSE SUMMIT CANDIDATE) — From a civilizational analytical view, sovereignty is inherently indivisible: one authority must hold ultimate decision-making power or chaos results. This perspective treats the rangatiratanga reading as attempting to create an impossible legal structure — you cannot have two sovereign authorities over the same territory. However, the beneficiary declarations and structural data contradict this: the arrangement functions through explicit power-sharing agreements, and the 'indivisibility' principle naturalizes what is actually a contingent Westphalian framework. Engine will classify this as a false summit.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__rangatiratanga_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__rangatiratanga_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__rangatiratanga_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, declining over time. The reading establishes a claim-structure where Māori retain inherent authority and Crown authority is derived/limited. Under this reading, the historical actual state (1840–1900) was maximum Crown extraction — Crown asserted full sovereignty despite treaty language limiting it to kāwanatanga over settlers, and systematically confiscated land and suppressed Māori authority. Current (2020) extractiveness is lower because settlement acts have operationalized parts of the reading (returning land, establishing co-governance, recognizing Māori authority). However, it remains tangled_rope level because significant Crown veto power persists in resource consents, conservation, and infrastructure decisions. Suppression (0.68): High, declining. Historical suppression (0.92) was near-total: Māori were physically displaced, legally prohibited from exercising authority, and isolated from decision-making through language barriers and institutional exclusion. Contemporary suppression reflects residual Crown veto capacity and jurisdictional ambiguity — Māori can exercise increasing ranges of authority but face institutional friction and legal challenges at boundaries. Theater ratio (0.62): Moderate-high and rising. Early period (0.15): the Crown's actions were functionally extractive (actual land confiscation, actual authority assertion) with minimal performative content. Current period (0.62): more theater has accumulated because settled land transfers, co-governance arrangements, and Tribunal findings create formal recognition of rangatiratanga while substantial Crown control persists in practice. The rising theater indicates that the gap between formal recognition and actual authority devolution is widening — more ritual acknowledgment, less substantive power shift.
 *
 * PERSPECTIVAL GAP:
 *   The rangatiratanga reading produces maximal perspectival divergence. Landless iwi see Snare (no exit from Crown dispute while unable to exercise authority). Recognized iwi see Tangled Rope (real authority exercised but surrounded by Crown veto points). Iwi authorities exercising full rohe see Rope (coordination function clear, no extraction). Crown sees Tangled Rope from the opposite direction — it coordinates settlers but loses jurisdictional clarity and must negotiate every major decision. Settlers see Scaffold (temporary uncertainty as new governance arrangement matures). The civilizational observer risks Mountain (treating unified Crown sovereignty as natural law). These gaps are not measurement artifacts — they reflect real structural differences in how agents experience the constraint. The engine's false-summit detection will flag the Mountain classification as naturalization, revealing that 'indivisible sovereignty' is a Westphalian doctrinal choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position within the constraint. Iwi are declared beneficiaries: they retain or recover tino rangatiratanga under this reading, placing them at d ≈ 0.15–0.25 (beneficiary with constrained/mobile exit depending on land status). Crown is declared victim: it loses unilateral authority and must negotiate, placing it at d ≈ 0.55–0.65 (victim with constrained exit — cannot simply override treaty obligations). The sigmoid f(d) applies the canonical power atom's transformation: powerless iwi with trapped exit get d → 0.95 → f(d) ≈ 1.42 (maximum extraction experienced); institutional Crown with constrained exit gets d → 0.55 → f(d) ≈ 0.75 (moderate extraction experienced). Scope modifiers apply: national scope σ(1.0) for base calculation. The perspectival gap between Rope (recognized iwi exercising authority) and Snare (landless iwi unable to exercise it) reflects the same underlying constraint structure experienced through different exit options: mobile/arbitrage (recognized) versus trapped (landless).
 *
 * MANDATROPHY ANALYSIS:
 *   The rangatiratanga reading resolves the mandatrophy by declaring unambiguously that the constraint is tangled_rope at the structural level: it coordinates resource governance AND extracts Crown authority through historical violation and ongoing veto. The reading is not trying to hide coordination under extraction (pure snare), nor is it coordination that accidentally developed coercive properties (rope-to-snare drift). It is a hybrid with both functions: genuine governance coordination over Māori territories (iwi manage whenua, fisheries, taonga) AND genuine extraction (Crown reserves veto over resource consents, infrastructure, conservation). The perspectives show that depending on structural position, agents experience this differently: organized iwi see more coordination, landless iwi see more extraction, Crown sees coordination-that-limits-its-authority, analytical observer risks seeing Mountain. The mandatrophy is not resolved by choosing one type, but by recognizing that tangled_rope is the reading's canonical state precisely because it combines coordination (the reading's claimed function) and extraction (the reading's historical reality and residual structural feature). The rising theater ratio indicates potential piton drift: as formal recognition increases without proportional power devolution, the arrangement could degrade toward performative co-governance (Piton) if suppression continues to decline while theater stays high.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tino_rangatiratanga_definition_scope,
    'Does tino rangatiratanga mean absolute independent authority over all domains (internal sovereignty), or does it mean co-equal authority with Crown jurisdiction depending on the domain (shared sovereignty)?',
    'Historical Māori language analysis of ''tino'' (unqualified/absolute) and ''rangatiratanga'' (chieftainship/authority); settlement act reading practices; comparative indigenous governance models (Sámi co-management, First Nations in Canada); iwi exercise of authority patterns 2015–2026',
    'If absolute: Crown''s kāwanatanga is limited to settler governance only (eliminates Crown veto over resource management, environmental protection, economic development in Māori territories). If shared-domain: Crown retains veto in specified domains (national security, infrastructure, conservation) — reduces extractiveness to 0.35, reclassifies Tangled Rope perspectives toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tino_rangatiratanga_definition_scope, empirical, 'Scope of tino rangatiratanga authority: absolute or domain-dependent').

omega_variable(
    kawanatanga_settler_jurisdiction_boundary,
    'Does Crown kāwanatanga over settlers include authority over settlers'' conduct in Māori territories, or does it mean Crown jurisdiction exists only in Crown-designated (urban, state-administered) areas?',
    'Treaty text analysis of ''kāwanatanga over the pākehā''; settlement acts'' jurisdictional clauses; case law interpreting dual jurisdiction boundaries; practical governance friction points 2015–2026 (resource consents, criminal jurisdiction, planning)',
    'If territorial: settlers in Māori territories fall under Māori authority (increases Iwi beneficiary power, reclassifies settler perspective as Snare). If functional: Crown authority follows settler populations regardless of location (reduces Iwi authority scope, keeps current Tangled Rope for Crown perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_settler_jurisdiction_boundary, conceptual, 'Whether Crown jurisdiction over settlers is territorial or functional').

omega_variable(
    historical_crown_bad_faith_attribution,
    'To what extent does the rangatiratanga reading rest on attribution of historical Crown bad faith (knowing misrepresentation of kāwanatanga to settlers while intending full sovereignty takeover), versus pragmatic working assumption that Crown gradually expanded its actual authority beyond what the treaty allowed?',
    'Documentary evidence of Crown intent 1840–1900; Waitangi Tribunal findings; comparative post-colonial restitution cases; hypothetical: if Crown had exercised only kāwanatanga (governance over settlers) and not confiscated land or superseded Māori authority, would the reading require different remediation?',
    'If intentional deception: reading justifies maximal restitution and independent Māori authority (supports current Snare classification for dispossessed iwi). If gradualism: reading permits co-governance compromise without full restoration of pre-contact authority (supports Tangled Rope as terminal state).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_crown_bad_faith_attribution, empirical, 'Whether Crown bad faith was intentional or emergent gradual expansion').

omega_variable(
    post_settlement_mandate_authority_gap,
    'When Crown settles historical claims through settlement acts (returning land, establishing co-governance), do those acts instantiate the rangatiratanga reading or do they merely compensate for its violation while Crown retains underlying sovereignty?',
    'Settlement act text analysis (whose authority is transferred — fee simple title, governance mandate, or both?); Tribunal interpretation of settlement acts as Treaty implementation versus alternative dispute resolution; iwi and Crown joint governance outcomes 2008–2026',
    'If instantiation: settlements ARE the reading made real (reclassifies current arrangement as Rope — coordination achieved). If compensation: settlements are residual acknowledgment but Crown retains ultimate authority (confirms Tangled Rope as stable terminal state).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_settlement_mandate_authority_gap, conceptual, 'Whether post-settlement arrangements instantiate or merely compensate for rangatiratanga').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waitangi_rangatira_theater_1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(waitangi_rangatira_theater_1890, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(waitangi_rangatira_theater_1940, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(waitangi_rangatira_theater_1990, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 150, 0.58).
narrative_ontology:measurement(waitangi_rangatira_theater_2020, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 180, 0.62).

% Extraction over time
narrative_ontology:measurement(waitangi_rangatira_extractiveness_1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(waitangi_rangatira_extractiveness_1890, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(waitangi_rangatira_extractiveness_1940, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 100, 0.82).
narrative_ontology:measurement(waitangi_rangatira_extractiveness_1990, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 150, 0.72).
narrative_ontology:measurement(waitangi_rangatira_extractiveness_2020, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 180, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(waitangi_rangatira_suppression_1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(waitangi_rangatira_suppression_1890, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(waitangi_rangatira_suppression_1940, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 100, 0.88).
narrative_ontology:measurement(waitangi_rangatira_suppression_1990, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 150, 0.75).
narrative_ontology:measurement(waitangi_rangatira_suppression_2020, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 180, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.18).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_title_recognition).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, iwi_co_governance_frameworks).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, conservation_authority_disputes_aotearoa).

% DUAL FORMULATION NOTE:
% This story represents one reading of the Waitangi sovereignty kernel. The crown_sovereignty_reading and partnership_reading are sibling stories — each instantiates the same kernel with different reference frames, axiom sets, and axiom statuses. All three should be present in the corpus; no single story represents 'the' constraint. The network edges indicate structural influence: this reading creates pressure on co-governance frameworks (affects_constraints: iwi_co_governance_frameworks) because the reading's force determines how much authority coexists; it influences settler political adaptation (affects: conservation_authority_disputes) because settlers must navigate uncertainty about whether they are subjects of Crown kāwanatanga only or also Māori authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
