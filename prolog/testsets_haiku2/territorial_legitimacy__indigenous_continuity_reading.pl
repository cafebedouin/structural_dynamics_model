% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy via Indigenous Continuity and Anti-Colonial Self-Determination
 *   domain: political/territorial/international
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested
 *   territorial legitimacy kernel. Under the indigenous continuity reading,
 *   the Israeli state is delegitimized as a settler-colonial arrangement;
 *   Palestinian territorial right derives from continuous indigenous
 *   habitation, not international partition; 1948 is coded as Nakba
 *   (catastrophe), not partition-mandated migration; and the right of return
 *   for 1948 refugees is structurally central to any legitimate settlement,
 *   not a negotiable concession. This reading opposes the partition reading
 *   (which grounds legitimacy in UN 181 and international legal recognition)
 *   and the security reading (which grounds legitimacy in defensive
 *   territorial control and strategic necessity). The engine will compute
 *   per-seat classifications from the structural data provided here; the
 *   authored claim (snare) reflects what this reading frames as the
 *   constraint's true character. Do NOT reconcile the claim with an
 *   alternative reading's frame — each reading instantiates its own
 *   constraint with its own ε and its own beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - Palestinian refugee diaspora (dispossessed 1948, denied return, powerless, trapped)
 *   - Palestinian residents West Bank/Gaza (under occupation, identity-locked to territorial claim)
 *   - Palestinian citizens of Israel 1948 (institutionally discriminated, constrained exit)
 *   - Israeli state institutional (agenda-setter, enforces exclusion, identity-locked)
 *   - International legal system (maintains contradictory commitments, observer seat)
 *   - Diaspora Palestinian movements (excluded from negotiation frameworks)
 *   - Israeli Jewish diaspora and settlement constituency (direct beneficiary)
 *   - Western security alliance states (beneficiary and secondary agenda-setter)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.89).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.92).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy via Indigenous Continuity and Anti-Colonial Self-Determination").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political/territorial/international").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, 'ed5f10b6-fa4d-42a5-81fd-af0f9381eea0').
narrative_ontology:cs_kernel_codification('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', distributed).
narrative_ontology:cs_authority_grounding('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', extraction).
narrative_ontology:cs_reading_relation('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', territorial_legitimacy__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', foundational, continuous_indigenous_habitation_as_legitimacy_ground).
narrative_ontology:cs_axiom_status(continuous_indigenous_habitation_as_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', continuous_indigenous_habitation_as_legitimacy_ground, deontological).
narrative_ontology:cs_axiom('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', foundational, settler_colonialism_structural_illegitimacy).
narrative_ontology:cs_axiom_status(settler_colonialism_structural_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', settler_colonialism_structural_illegitimacy, deontological).
narrative_ontology:cs_axiom('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', secondary, right_of_return_non_negotiable).
narrative_ontology:cs_axiom_status(right_of_return_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', right_of_return_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', pre_partition_palestinian_self_determination).
narrative_ontology:cs_drift_state('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', contemporary_2024, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('ed5f10b6-fa4d-42a5-81fd-af0f9381eea0', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_residents_west_bank_gaza).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_israel_1948).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, israeli_jewish_diaspora_and_settlement_constituency).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, western_security_alliance_states).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, indigenous_territorial_primacy).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, right_of_return_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispossessed in 1948 (Nakba narrative frame: forced displacement, not partition-mandated migration). Denied right of return by Israeli state; distributed across refugee camps in Lebanon, Syria, Jordan, West Bank, and diaspora. Bear the cost of territorial non-recognition and statelessness — legal status ambiguous, property claims unenforceable under international law, intergenerational trauma normalized. Exit from this situation requires either Israeli state reversal of return denial or absorption by host states; both routes are politically closed.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, global).

% Live under territorial arrangements they did not accept (PA governance in West Bank under occupation framework; Gaza under blockade). Subjected to military administration, settlement expansion, resource extraction (water, land), and restricted movement. Identity-locked to the territorial claim itself — departure would require abandonment of the claim to the territory they inhabit. The constraint extracts by denying them sovereignty over their inhabited land while enforcing recognition of an alternative sovereignty claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_residents_west_bank_gaza, payer,
    powerless, biographical, identity_locked, local).

% Citizens of the Israeli state by law but subject to institutional discrimination in land access, housing allocation, and resource distribution. Excluded from the state's founding narrative and security logic (the state is defined as a Jewish state, not a civic state of all inhabitants). Politically constrained: organized at below-proportional voting power; land acquisition restricted by legacy laws of property registration. Bear suppression costs through institutional exclusion, not direct violence, but structural marginalization is severe.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_israel_1948, payer,
    moderate, generational, constrained, regional).

% Sets territorial boundaries, enforces exclusion of refugee return, administers settlement policy, controls military apparatus that maintains the constraint. Legitimates itself through alternative territorial narratives (historical Jewish connection, security necessity, international legal partition). Collects from the arrangement through territorial control, resource extraction, demographic advantage, and geopolitical status. Identity-locked to the territorial claim as the Israeli nation-state — reversal would dissolve the state identity itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_institutional, agenda_setter,
    institutional, generational, identity_locked, regional).

% Maintains parallel commitments to (1) partition as legitimate (UN 181, 1948 resolution), (2) anti-colonial self-determination as principle (UN Charter), and (3) right of return as law (UN 194). The three readings of the territorial legitimacy kernel each marshal different parts of international law as their authority. The system does not resolve the contradiction; instead it accommodates all three readings simultaneously, producing a kind of institutional paralysis.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_system, observer,
    institutional, civilizational, analytical, global).

% Advocate for right of return and one-state solutions; politically excluded from direct negotiation frameworks that assume two-state partition. Their voices are present in civil society and some UN forums but structurally sidelined from the official territorial settlement process. Exclusion from negotiation is itself part of the constraint's enforcement mechanism — return advocates are kept out of the table where territorial legitimacy is adjudicated.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, diaspora_palestinian_political_movements, excluded,
    organized, generational, constrained, global).

% Benefit from the territorial arrangement through right of return to the Israeli state (Law of Return, 1950) and through settlement colonization framework (rapid path to citizenship and property acquisition for Jewish immigrants while Palestinians are denied the same). Exit is available through relocation, but ideological and kinship ties to the state create strategic stickiness. Direct beneficiary of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_jewish_diaspora_and_settlement_constituency, beneficiary,
    organized, biographical, mobile, global).

% Benefit from Israeli state as strategic ally in Middle East region; provide military, economic, and diplomatic support that underwrites the constraint's enforcement. Shape territorial narratives through veto power on UN resolutions and diplomatic pressure on negotiation frameworks. Benefit indirectly through regional geopolitical positioning. Could exit through policy reversal; exit costs are political rather than material.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, western_security_alliance_states, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, western_security_alliance_states, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, israeli_state_institutional).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does not recognize a coordination function. Under the indigenous continuity reading, what the beneficiaries frame as 'solving a security problem through partition' is reframed as 'engineering dispossession and encoding it in law.' The arrangement does coordinate Israeli state interests and western security interests; it does NOT coordinate Palestinian interests, who are dispossessed by the same mechanism. Since one party is excluded by design, it is not coordination of a genuine collective-action problem — it is coordination of extractors against victims.
% TRANSFER_FUNCTION: Transfers sovereignty, territory, water resources, settlement rights, demographic advantage, and recognition from Palestinians (victims) to the Israeli state (beneficiary) and its supporting constituencies (Israeli diaspora, western allies). Palestinians pay through loss of territory, refugee status, denial of return, military administration, resource extraction, and institutional discrimination. The transfer is encoded in law (Law of Return for Jews, property seizures for Palestinians, settlement expansion) and enforced militarily.
% ABSENT_VOICES: Palestinian refugee diaspora have no seat at the negotiation table — they are the largest set of dispossessed parties and have no formal voice in territorial settlement. Right-of-return advocates within Palestinian movements are excluded from frameworks (Oslo, Camp David, two-state negotiations) that assume partition rather than return. Israeli voices questioning the legitimacy of the ethno-national state framing are marginalized by security logic and national narrative.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared (territorial non-recognition of Palestinian sovereignty, refugee exclusion, settlement expansion, military occupation), the world would reorganize severely: Palestinian refugee return would reshape Levantine demography and politics; the Israeli state would require reconstitution as a civic rather than ethno-national state; western geopolitical positioning in the Middle East would shift; regional security logics would invert. The persistence of the constraint despite these high reorganization costs is evidence of how much benefit the agenda-setter and allies extract from it.
% FOUNDING_PROBLEM: Post-World War II need for a territorial refuge for Jewish populations displaced by European antisemitism and Holocaust; British League of Nations mandate requiring eventual self-determination for the Palestinian population; collision between Jewish immigration to Palestine and Palestinian resistance to displacement.
% FOUNDING_PROBLEM_CORROBORATION: Israeli institutional and western security allies attest the founding problem (need for Jewish refugee) remains live, though at lower intensity than 1945. Historians and international human-rights scholars outside the benefiting parties document that the founding problem was substantially addressed by 1950 (Jewish state established, migration routes open); the constraint's persistence and intensification after 1967 reflects not response to the founding problem but extraction layered onto a solved problem. The evidence is from historical scholarship, UN documentation, and analyses by scholars without institutional tie to the Israeli state.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under this reading, extractiveness is very high (0.89) because the constraint transfers sovereignty, land, and demographic advantage from Palestinians to Israel without Palestinian consent or compensation — the transfer is extraction, not negotiated exchange. Suppression is exceptionally high (0.92) because the constraint's persistence depends on active military administration (occupation), legal denial of rights (refugee exclusion, Law of Return asymmetry), and international diplomatic suppression of competing readings (veto power on UN resolutions). Theater ratio is low at interval start (0.05, year 0 = 1948) and rises to moderate (0.22 at present): early enforcement was primarily direct (military dispossession, refugee expulsion); as decades pass and the arrangement stabilizes, more of the enforcement machinery becomes theatrical (peace-process performance, recognition of Palestinian rights in principle while denying them in practice, two-state framing that preserves the status quo). The series is authored on one shared time grid, with all three metrics valued at every time point. Time point 0 = 1948 (Nakba), endpoint = ~2024.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Israeli state institutional) experiences this constraint very differently from the payer seats. From the Israeli institutional position, the arrangement is security necessity and legal partition — the alternative is existential threat. From the Palestinian payer positions, the same structure is dispossession and ongoing extraction without consent. The engine computes these divergences from the structural data: the Israeli state has high exit options (state power, international backing, ideological commitment to the territorial claim), while Palestinians have trapped or identity-locked exit (denial of return, statelessness, rootedness to the contested land). The directionality derivation should produce near-zero d for the agenda-setter (beneficiary, high power, high exit = low-to-negative effective extraction on that seat) and near-unity d for the payer seats (victims, low power, trapped/identity-locked exit = high effective extraction on those seats). This seat divergence is the engine's job to compute; I am merely authoring the structural conditions that drive it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state is the structural beneficiary: it collects sovereignty, territory, demographic advantage, international recognition (at least from Western powers), and security positioning. Palestinian refugees are the primary victims: dispossessed property, denied return, stateless status, legal disability. Palestinians in the West Bank and Gaza bear extraction through military administration, resource extraction (water, land), and denial of self-determination. Palestinians within Israel are institutionally discriminated against but retain some civic protections — they are payers but at a lower extraction rate than the diaspora and occupied populations. The western security alliance states are secondary beneficiaries: they acquire strategic positioning, alliance commitments, and a dependent state in a critical region. The international legal system is an observer: it maintains doctrinal commitments (partition, self-determination, right of return) that contradict each other, producing institutional paralysis that benefits the status-quo maintainer (the Israeli state) by preventing enforcement of any alternative reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames the mandate as historically dead but institutionally undead. The founding mandate — provide a territorial refuge for persecuted Jewish populations — is, under this reading, substantially satisfied by 1967 at the latest. Yet the constraint persists and intensifies (theater rising from 0.05 to 0.22 over the interval) because the beneficiaries have transformed the mandate: the constraint now operates not to solve a coordination problem (providing refuge) but to sustain an extraction arrangement (territorial expansion, resource control, regional hegemony). The constraint is a mandatrophy case: the original problem is dead, but the solution has become self-perpetuating through institutional inertia and beneficiary interest in maintaining extraction. Mandatrophy resolution would require acknowledging the founding problem is satisfied and reversing the constraint — the Israeli institutional seat cannot do this without dissolving its own identity as an ethno-national state. Hence mandatrophy persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nakba_vs_partition_framing,
    'Is 1948 best understood as the execution of an international legal partition (UN 181) or as the dispossession of an indigenous population (Nakba)?',
    'Archival study of intent: did partition supporters (UN framers, British withdrawal, Zionist leadership) intend or foresee Palestinian displacement? Did they accept it as an unavoidable consequence, or did they deliberately engineer it? Contemporaneous statements and planning documents would show intent.',
    'If partition was the intended framework and displacement was foreseen/accepted, the indigenous continuity reading''s core claim (settler colonialism, not partition) weakens slightly — displacement would be a known cost of the chosen solution, not a hidden extraction. If displacement was engineered deliberately and partition was a cover story, the indigenous reading is vindicated structurally. If displacement was contingent on subsequent events (not predetermined), the reading''s necessity claim is weaker.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nakba_vs_partition_framing, empirical, 'Whether 1948 displacement was intended by partition framers or engineered post-hoc.').

omega_variable(
    partition_inevitability,
    'Given the historical conditions of 1947 (European antisemitism, Jewish migration to Palestine, Palestinian resistance, British mandate withdrawal), was partition the only viable solution, or did alternatives exist that would have achieved Jewish territorial security without full Palestinian displacement?',
    'Counterfactual historical analysis: what were the actual alternatives presented and rejected? What was the reasoning for rejection (security, feasibility, political will)? Comparative analysis of other partition scenarios (1947 Peel Commission proposals, later two-state frameworks) shows whether more equitable distributions were structurally possible.',
    'If partition was inevitable given the conditions, the indigenous reading overstates the voluntarism in the choice and the reading''s extraction framing becomes weaker (inevitable outcome from collision of movements, not engineered extraction). If alternatives were available but rejected, the reading''s snare framing is strengthened — the dispossession was chosen, not forced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_inevitability, conceptual, 'Whether displacement was an inevitable consequence of decolonization or a chosen solution among alternatives.').

omega_variable(
    right_of_return_feasibility,
    'Is the right of return (UN 194, Palestinian law) structurally compatible with the Israeli state''s survival as a political entity, or does return materially dissolve the state through demographic change?',
    'Demographic modeling: what population flows and settlement patterns would result from full right of return? What are the scenarios under which the Israeli state remains politically viable with majority Palestinian population or significant demographic rebalancing? Legal analysis: is a one-state solution with minority Jewish rights protection structurally stable, or does it require the demographic maintenance of Jewish majority or near-majority?',
    'If return is compatible with Israeli state survival (through one-state restructuring, power-sharing frameworks, or managed return flows), the indigenous reading''s insistence on return as ''structurally central'' is vindicated. If return dissolves the state, the reading must clarify whether it accepts state dissolution as the cost of justice, which reframes the constraint from snare (extraction with enforcement) to revolutionary (change-of-state demand). The distinction matters for classification: a snare claims extraction within an existing framework; a revolutionary constraint claims the framework itself is illegitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_feasibility, empirical, 'Whether right of return is compatible with Israeli state survival or requires state dissolution.').

omega_variable(
    settler_colonial_classification,
    'Does the settler-colonial label (applied by this reading to the Israeli state) map onto the definitional criteria used in postcolonial scholarship and international law, or is it a polemical frame that obscures structural differences from historical settler colonialism (e.g., Australia, South Africa, Algeria)?',
    'Comparative structural analysis: what are the definitional features of settler colonialism? Does the Israeli case match the criteria (external settler population displacing indigenous population, permanent settlement intent, racial/ethnic governance structure)? Or does it differ in ways that require a distinct category (refugee-driven migration, religious-national rather than racial settler ideology, indigenous population not exterminated but subordinated)?',
    'If the Israeli case matches settler-colonial criteria, the indigenous reading''s framing is vindicated structurally. If the Israeli case is structurally distinct, the reading''s rhetorical leverage is weakened — it must reframe the claim to fit the actual structure rather than claiming identity with historical precedent. The classification choice affects both moral standing (is the arrangement comparable to South African apartheid, or is that analogy misleading?) and remedial pathways (decolonization models drawn from historical cases may not fit if the structure is different).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_classification, conceptual, 'Whether the Israeli state fits the structural definition of settler colonialism or is a distinct category.').

omega_variable(
    identity_lock_asymmetry,
    'Why is the Israeli state identity-locked to the territorial claim while Palestinians are identity-locked to the same territory, yet these locks produce opposed exit constraints?',
    'Structural analysis of identity constitution: for the Israeli state, what dissolves if the territorial claim is reversed? (State identity as ethno-national entity, security logic, demographic majority, founding narrative.) For Palestinians, what dissolves if they accept non-return? (Historical justice, dispossession reversal, claim to belonging in the land of origin, intergenerational restoration.) Are these dissolution paths symmetrical? Can one party exit the identity-lock without the other party dissolving?',
    'If the dissolution paths are symmetrical (both parties lose foundational identity claims if they abandon the territorial claim), the constraint is a genuine stalemate and no resolution preserves both identities. If the paths are asymmetrical (one party could restructure identity without dissolution while the other cannot), the constraint is not symmetric — one party bears asymmetric dissolution cost if they concede. The asymmetry would strengthen the snare framing by showing structural asymmetry in the ''lock,'' not just in initial dispossession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_asymmetry, conceptual, 'Whether the identity-locks holding both parties to the constraint are symmetrical or asymmetrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t10, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(terr_tr_t10, observed).
narrative_ontology:measurement(terr_tr_t20, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(terr_tr_t20, observed).
narrative_ontology:measurement(terr_tr_t30, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(terr_tr_t30, observed).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(terr_tr_t40, observed).
narrative_ontology:measurement(terr_tr_t50, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement_basis(terr_tr_t50, observed).
narrative_ontology:measurement(terr_tr_t60, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(terr_tr_t60, observed).
narrative_ontology:measurement(terr_tr_t76, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 76, 0.22).
narrative_ontology:measurement_basis(terr_tr_t76, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 0, 0.87).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t10, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 10, 0.88).
narrative_ontology:measurement_basis(terr_be_t10, observed).
narrative_ontology:measurement(terr_be_t20, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 20, 0.89).
narrative_ontology:measurement_basis(terr_be_t20, observed).
narrative_ontology:measurement(terr_be_t30, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(terr_be_t30, observed).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(terr_be_t40, observed).
narrative_ontology:measurement(terr_be_t50, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 50, 0.9).
narrative_ontology:measurement_basis(terr_be_t50, observed).
narrative_ontology:measurement(terr_be_t60, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 60, 0.9).
narrative_ontology:measurement_basis(terr_be_t60, observed).
narrative_ontology:measurement(terr_be_t76, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 76, 0.89).
narrative_ontology:measurement_basis(terr_be_t76, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t10, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement_basis(terr_su_t10, observed).
narrative_ontology:measurement(terr_su_t20, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement_basis(terr_su_t20, observed).
narrative_ontology:measurement(terr_su_t30, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 30, 0.92).
narrative_ontology:measurement_basis(terr_su_t30, observed).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement_basis(terr_su_t40, observed).
narrative_ontology:measurement(terr_su_t50, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 50, 0.93).
narrative_ontology:measurement_basis(terr_su_t50, observed).
narrative_ontology:measurement(terr_su_t60, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 60, 0.92).
narrative_ontology:measurement_basis(terr_su_t60, observed).
narrative_ontology:measurement(terr_su_t76, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 76, 0.92).
narrative_ontology:measurement_basis(terr_su_t76, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=76
narrative_ontology:measurement(terr_grid_01, territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse(class), 0, 0.88).
narrative_ontology:measurement(terr_grid_02, territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse(class), 76, 0.81).
narrative_ontology:measurement(terr_grid_03, territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse(individual), 0, 0.95).
narrative_ontology:measurement(terr_grid_04, territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse(individual), 76, 0.92).
narrative_ontology:measurement(terr_grid_05, territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(terr_grid_06, territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse(organizational), 76, 0.68).
narrative_ontology:measurement(terr_grid_07, territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse(structural), 0, 0.82).
narrative_ontology:measurement(terr_grid_08, territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse(structural), 76, 0.75).
narrative_ontology:measurement(terr_grid_09, territorial_legitimacy__indigenous_continuity_reading, resistance(class), 0, 0.8).
narrative_ontology:measurement(terr_grid_10, territorial_legitimacy__indigenous_continuity_reading, resistance(class), 76, 0.82).
narrative_ontology:measurement(terr_grid_11, territorial_legitimacy__indigenous_continuity_reading, resistance(individual), 0, 0.75).
narrative_ontology:measurement(terr_grid_12, territorial_legitimacy__indigenous_continuity_reading, resistance(individual), 76, 0.78).
narrative_ontology:measurement(terr_grid_13, territorial_legitimacy__indigenous_continuity_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(terr_grid_14, territorial_legitimacy__indigenous_continuity_reading, resistance(organizational), 76, 0.72).
narrative_ontology:measurement(terr_grid_15, territorial_legitimacy__indigenous_continuity_reading, resistance(structural), 0, 0.72).
narrative_ontology:measurement(terr_grid_16, territorial_legitimacy__indigenous_continuity_reading, resistance(structural), 76, 0.75).
narrative_ontology:measurement(terr_grid_17, territorial_legitimacy__indigenous_continuity_reading, stakes_inflation(class), 0, 0.85).
narrative_ontology:measurement(terr_grid_18, territorial_legitimacy__indigenous_continuity_reading, stakes_inflation(class), 76, 0.84).
narrative_ontology:measurement(terr_grid_19, territorial_legitimacy__indigenous_continuity_reading, stakes_inflation(individual), 0, 0.9).
narrative_ontology:measurement(terr_grid_20, territorial_legitimacy__indigenous_continuity_reading, stakes_inflation(individual), 76, 0.88).
narrative_ontology:measurement(terr_grid_21, territorial_legitimacy__indigenous_continuity_reading, stakes_inflation(organizational), 0, 0.75).
narrative_ontology:measurement(terr_grid_22, territorial_legitimacy__indigenous_continuity_reading, stakes_inflation(organizational), 76, 0.78).
narrative_ontology:measurement(terr_grid_23, territorial_legitimacy__indigenous_continuity_reading, stakes_inflation(structural), 0, 0.8).
narrative_ontology:measurement(terr_grid_24, territorial_legitimacy__indigenous_continuity_reading, stakes_inflation(structural), 76, 0.82).
narrative_ontology:measurement(terr_grid_25, territorial_legitimacy__indigenous_continuity_reading, suppression(class), 0, 0.84).
narrative_ontology:measurement(terr_grid_26, territorial_legitimacy__indigenous_continuity_reading, suppression(class), 76, 0.87).
narrative_ontology:measurement(terr_grid_27, territorial_legitimacy__indigenous_continuity_reading, suppression(individual), 0, 0.88).
narrative_ontology:measurement(terr_grid_28, territorial_legitimacy__indigenous_continuity_reading, suppression(individual), 76, 0.91).
narrative_ontology:measurement(terr_grid_29, territorial_legitimacy__indigenous_continuity_reading, suppression(organizational), 0, 0.82).
narrative_ontology:measurement(terr_grid_30, territorial_legitimacy__indigenous_continuity_reading, suppression(organizational), 76, 0.85).
narrative_ontology:measurement(terr_grid_31, territorial_legitimacy__indigenous_continuity_reading, suppression(structural), 0, 0.8).
narrative_ontology:measurement(terr_grid_32, territorial_legitimacy__indigenous_continuity_reading, suppression(structural), 76, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__indigenous_continuity_reading, 0.25).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE READING of the territorial_legitimacy kernel. The indigenous_continuity_reading (this file) frames 1948 as Nakba and Palestinian dispossession. The partition_reading frames 1948 as international legal partition and state recognition. The security_necessity_reading frames territorial legitimacy as derived from defensive control and strategic depth. All three are held simultaneously by different parties; they coexist rather than logically foreclose each other. Each story has its own ε (this reading: 0.89; partition reading: lower, coordination-focused; security reading: lower, coordination-focused). The network links allow the corpus to track the constraint family as a whole while keeping each reading's structural data separate and ε-invariant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
