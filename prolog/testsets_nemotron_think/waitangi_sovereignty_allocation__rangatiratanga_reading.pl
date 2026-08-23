% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Waitangi Article II — Tino Rangatiratanga Retained (Māori Text Reading)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Māori text of Te Tiriti o Waitangi Article II guarantees 'te tino
 *   rangatiratanga' (full authority/chieftainship) over 'ō rātou wenua o
 *   rātou kāinga me ō rātou taonga katoa' (their lands, villages, and all
 *   their treasures). The Crown gained only 'kāwanatanga' (governorship) — a
 *   delegated authority to govern settlers. This reading, grounded in the
 *   Māori text signed by the vast majority of rangatira, treats tino
 *   rangatiratanga as a pre-existing, inherent authority that cannot be
 *   ceded. The standing arrangement — Crown parliamentary sovereignty over
 *   all territory and people — is assessed from this reading's lights as a
 *   high-extraction, actively enforced constraint that suppresses Māori
 *   authority and extracts resources, land, and decision-making power. The
 *   claimed_type is Mountain: tino rangatiratanga is asserted as an
 *   irreducible structural fact of Māori constitutional order, not a
 *   negotiable concession.
 *
 * KEY AGENTS:
 *   - maori_iwi_hapu: Primary target (powerless/identity_locked) — bears extraction of authority, land, resources
 *   - crown_institutions: Primary beneficiary (institutional/arbitrage) — collects sovereignty rents, controls governance
 *   - settler_population: Beneficiary (organized/constrained) — benefits from Crown governance and resource access
 *   - nz_government: Agenda setter (institutional/arbitrage) — administers and enforces the constraint
 *   - waitangi_tribunal: Observer (institutional/analytical) — investigates but lacks enforcement power
 *   - corporate_resource_interests: Beneficiary (powerful/arbitrage) — extracts resource value under Crown license
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.82).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, mountain).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Waitangi Article II — Tino Rangatiratanga Retained (Māori Text Reading)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).
domain_priors:emerges_naturally(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, 'deb6e77c-a5a9-4c66-86f0-46adf3105309').
narrative_ontology:cs_kernel_codification('deb6e77c-a5a9-4c66-86f0-46adf3105309', fixed_text).
narrative_ontology:cs_authority_grounding('deb6e77c-a5a9-4c66-86f0-46adf3105309', lineage).
narrative_ontology:cs_interpretation_layer_present('deb6e77c-a5a9-4c66-86f0-46adf3105309').
narrative_ontology:cs_reading_relation('deb6e77c-a5a9-4c66-86f0-46adf3105309', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('deb6e77c-a5a9-4c66-86f0-46adf3105309', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('deb6e77c-a5a9-4c66-86f0-46adf3105309', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('deb6e77c-a5a9-4c66-86f0-46adf3105309', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_axiom('deb6e77c-a5a9-4c66-86f0-46adf3105309', foundational, kawanatanga_limited_to_settlers).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_settlers, holdable).
narrative_ontology:cs_axiom_grounding('deb6e77c-a5a9-4c66-86f0-46adf3105309', kawanatanga_limited_to_settlers, conventional).
narrative_ontology:cs_reference_frame('deb6e77c-a5a9-4c66-86f0-46adf3105309', id_1840_tiriti_relationship).
narrative_ontology:cs_drift_state('deb6e77c-a5a9-4c66-86f0-46adf3105309', contemporary_crown_sovereignty, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('deb6e77c-a5a9-4c66-86f0-46adf3105309', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_institutions).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_population).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, nz_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, corporate_resource_interests).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_owners).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_resource_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Iwi and hapū are the primary bearers of tino rangatiratanga. They experience the Crown's sovereignty as active extraction: land alienated through Native Land Court and confiscation; resources (fisheries, forests, water, minerals) regulated and licensed by Crown agencies; decision-making authority over their own territories superseded by Crown law. Exit is identity_locked — tino rangatiratanga is constitutive of Māori political identity; to exit the constraint would be to cease asserting Māori authority, which is experienced as cultural and political death. Resistance is continuous: petitions, courts, occupations, constitutional transformation movements.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, payer,
    powerless, generational, identity_locked, national).

% Māori land owners (often within iwi/hapū structures) hold title under Te Ture Whenua Māori Act, which imposes Crown restrictions on alienation, partition, and development. They bear extraction through forced sales, rating burdens, and inability to develop land according to tikanga. Exit is constrained — they can sell to Crown or other Māori but cannot exercise full rangatiratanga over land use. Some access Treaty settlements as partial redress.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_owners, payer,
    moderate, biographical, constrained, local).

% Māori customary users of fisheries, forests, freshwater, and geothermal resources. Crown quota management systems (QMS), Resource Management Act, and conservation legislation regulate and restrict customary access. They bear extraction through lost customary harvest, commercial quota allocation to corporations, and pollution/degradation permitted by Crown agencies. Exit is constrained — customary rights are recognised in law but subordinated to Crown management regimes.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_resource_users, payer,
    moderate, biographical, constrained, regional).

% Parliament, executive, judiciary, and state agencies (TPK, DOC, MPI, LINZ, etc.) administer and enforce Crown sovereignty. They collect the gains: legislative monopoly, resource licensing revenue, territorial jurisdiction, regulatory authority over Māori affairs. They justify this as democratic governance and Treaty partnership. Exit is arbitrage-grade — they could devolve authority (and have in limited co-governance) but structurally benefit from retaining ultimate control. The Waitangi Tribunal sits within this structure but lacks binding enforcement power.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_institutions, beneficiary).

% The elected government of the day wields Crown sovereignty. It negotiates Treaty settlements, designs co-governance arrangements, and legislates over Māori affairs (e.g., Three Waters, Fast-track Approvals). It benefits from the sovereignty allocation through governing capacity and resource control. Exit is arbitrage — governments could pursue constitutional transformation but face electoral and institutional incentives to maintain the status quo.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, nz_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, nz_government, beneficiary).

% Non-Māori New Zealanders benefit from Crown governance: secure land title, public services, infrastructure, rule of law. The kāwanatanga guarantee was originally for their governance. They bear diffuse costs (Treaty settlement fiscal transfers, perceived privilege) but net benefit. Exit is constrained — they are born into the system; emigration is possible but costly. Their political voice dominates the parliamentary system that upholds the constraint.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_population, beneficiary,
    organized, biographical, constrained, national).

% Forestry, fishing, mining, energy, and agribusiness corporations extract resource value under Crown-granted licenses and consents. They benefit from Crown sovereignty's allocation of resource rights and regulatory certainty. They lobby to maintain Crown authority over resource allocation (rather than Māori authority). Exit is arbitrage-grade — capital is mobile; they would operate under any regime that secures their returns.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, corporate_resource_interests, beneficiary,
    powerful, biographical, arbitrage, global).

% A standing commission of inquiry established 1975. It investigates Treaty claims, produces reports recommending redress, and shapes public and policy understanding. It has no enforcement power; the Crown accepts or rejects its recommendations. It occupies an analytical seat: it sees the full structure but cannot alter it. Its existence is itself a product of Māori resistance (the 1975 land march).
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% UN Special Rapporteurs, UNPFII, human rights bodies, and comparative constitutional scholars. They observe and critique New Zealand's compliance with UNDRIP and international law. They have no domestic enforcement power but contribute to legitimacy pressure on the Crown.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Treaty's original coordination function (from rangatiratanga perspective): providing Crown with lawful authority to govern its own settlers (kāwanatanga) while Māori retained full authority over their own affairs (tino rangatiratanga). This solved the problem of orderly British settlement without conflict.
% TRANSFER_FUNCTION: The standing arrangement transfers: legislative authority over Māori territories from Māori to Parliament; resource allocation rights from Māori to Crown agencies and corporate licensees; land title from Māori to settlers/Crown; regulatory control over taonga from Māori to Crown ministries. The flow is unidirectional — from Māori to Crown/settlers/capital.
% ABSENT_VOICES: Māori who reject any Crown authority over Māori (independence movements) are structurally excluded from the Treaty settlement process, which requires accepting Crown sovereignty. Future generations of Māori who will inherit diminished authority. Non-Māori who would support constitutional transformation but are not organised. The voices of taonga themselves (rivers, forests, mountains) recognised as ancestors in tikanga but not in Crown law.
% DISAPPEARANCE_RATIONALE: If Crown sovereignty over Māori vanished overnight, Māori would reassert tino rangatiratanga over their territories: iwi/hapū would resume full governance, resource management, and decision-making. Crown would retain governance over settlers only. Land titles would revert to Māori customary title. Resource management would shift to tikanga-based systems. The NZ state would reorganise as a binational or multinational constitutional order. The world would fundamentally rearrange.
% FOUNDING_PROBLEM: The Crown needed lawful authority to govern British settlers in New Zealand (kāwanatanga) without undermining Māori authority (tino rangatiratanga). Māori needed protection from lawless settlers and a framework for ongoing relationship with the Crown.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Crown need for governorship over settlers) is attested as dead by: Waitangi Tribunal reports (e.g. Te Urewera, Whanganui River) noting Crown sovereignty has expanded far beyond kāwanatanga; Matike Mai Aotearoa (Independent Working Group on Constitutional Transformation) concluding the original Treaty relationship has been unilaterally overridden; Crown's own 2019 Cabinet paper acknowledging 'the Treaty partnership has not been honoured'. No corroboration from outside the benefiting parties supports the claim that the founding problem remains live — Crown institutions assert ongoing relevance but this is self-serving.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, ExtMetricName, E),
    domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(waitangi_sovereignty_allocation__rangatiratanga_reading),
    narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because Crown sovereignty operates as a transfer mechanism: resource rents, land value, regulatory authority, and jurisdictional control flow from Māori to Crown institutions and settlers. Suppression (0.78) is high because the constraint's persistence depends on active legal, police, and military enforcement — Native Land Court, confiscation, Resource Management Act overrides, Foreshore and Seabed Act. Theater ratio (0.45) reflects genuine Treaty settlement processes and co-governance arrangements that perform recognition while Crown ultimate authority remains intact. Accessibility collapse (0.72) is high: independent Māori governance structures were systematically dismantled; re-establishing them requires overcoming entrenched legal and institutional barriers. Resistance (0.68) is substantial and sustained: from 1840s armed resistance to 1970s land marches to contemporary constitutional transformation movements.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat (agenda_setter, institutional), the constraint appears as legitimate sovereignty — a Mountain of constitutional law. From the Māori seat (payer, powerless/identity_locked), the same structure operates as a Snare: active suppression of inherent authority for resource extraction. The partnership_reading seat would compute as Tangled Rope: genuine coordination function (Treaty partnership) with asymmetric extraction (Crown retains ultimate authority). The engine computes this divergence from the structural data — the rangatiratanga_reading's claim of Mountain is its own structural self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori (iwi/hapū) are structural targets: they bear the extraction (lost authority, alienated land, regulated resources) with identity_locked exit — tino rangatiratanga is constitutive of Māori identity; exit means ceasing to be Māori politically. Crown institutions are structural beneficiaries: they collect the sovereignty rents (legislative monopoly, resource licensing, territorial control) with arbitrage-grade exit (they could devolve power but choose not to). Settlers benefit indirectly (secure title, governance services) with constrained exit. The Waitangi Tribunal sits at analytical: it observes and recommends but cannot bind. Corporate interests are powerful beneficiaries with arbitrage exit (capital mobility).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Crown need for lawful settlement authority) is dead — the settler population is now self-governing and no longer requires Crown protection from lawlessness. Yet the constraint (Crown sovereignty over Māori) persists and has expanded. The mandate has atrophied into extraction: the original coordination function (orderly settlement) is complete; what remains is the extraction of Māori authority and resources. This is not a Piton (inertial remnant) because the constraint is actively enforced and expanded (e.g., Three Waters reform centralisation, Fast-track Approvals Act). The mandatrophy is unresolved: the arrangement persists because beneficiaries (Crown, settlers, capital) capture the gains, while the cost to fix (constitutional transformation) is prohibitive for the payer (Māori) alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is tino rangatiratanga a genuine natural law / inherent authority (Mountain) or a constructed constitutional claim that benefits identifiable agents (false summit)?',
    'Historical-linguistic analysis of 1840 Māori text vs English text; comparative analysis of pre-1840 Māori governance structures; assessment of whether Crown beneficiaries extract rents from the sovereignty allocation.',
    'If Mountain: Māori authority is irreducible and Crown governance over Māori is structurally illegitimate extraction. If false summit: the ''natural law'' framing conceals a constitutional arrangement that benefits Crown institutions and settlers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Natural-law vs constructed-status of tino rangatiratanga claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Māori authority primarily structural (legal barriers, resource alienation, institutional exclusion) or internalized (colonised governance imaginaries, legitimated Crown frameworks)?',
    'Post-settlement trajectory analysis: if suppression persists after Treaty settlements and co-governance arrangements, internalized component is significant.',
    'If substantially internalized, effective suppression exceeds structural measure — the constraint travels with Māori even in spaces of formal recognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of Māori authority').

omega_variable(
    coextensive_authority_claim,
    'Does tino rangatiratanga authorize fully independent Māori governance (foreclosing Crown authority over Māori), or a sphere of Māori authority coexisting with Crown kāwanatanga over shared territory?',
    'Waitangi Tribunal findings; Māori constitutional thought (e.g. Matike Mai); Crown response to independence movements.',
    'If independent: crown_sovereignty_reading is foreclosed in any single framework. If coexisting: partnership_reading becomes the structural mediation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coextensive_authority_claim, conceptual, 'Scope of tino rangatiratanga — independence vs coexistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(wait_tr_t1860, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1860, 0.15).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.15).
narrative_ontology:measurement(wait_be_t1860, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1860, 0.45).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1900, 0.75).
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1975, 0.85).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.1).
narrative_ontology:measurement(wait_su_t1860, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1860, 0.65).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, nz_resource_management_act).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, nz_local_government_act).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, foreshore_seabed_act).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the waitangi_sovereignty_allocation kernel. The crown_sovereignty_reading (ε≈0.15, Mountain from Crown seat) and partnership_reading (ε≈0.45, Tangled Rope) are separate stories with distinct ε, stakeholders, and classifications. They are linked via network.affects_constraints. The rangatiratanga_reading has the highest ε because from its lights the standing Crown arrangement is maximally extractive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, institutional, 0.15).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
