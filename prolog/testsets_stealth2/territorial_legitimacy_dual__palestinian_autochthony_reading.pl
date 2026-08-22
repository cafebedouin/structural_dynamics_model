% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Standing Territorial Arrangement — Palestinian Autochthony Reading
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   The standing arrangement under contest: a state established in the 1948
 *   war atop the displacement of most of the country's Arab inhabitants,
 *   which has since administered a citizenship, land, and entry regime
 *   keeping the displaced and their descendants outside for three
 *   generations, and since 1967 has administered the remaining Palestinian
 *   territories under military government while transferring land to the
 *   national group's institutions. This story authors that arrangement from
 *   the palestinian_autochthony_reading: continuous habitation grounds the
 *   primary claim to the land, the displacement is an open wrong transmitted
 *   across generations and still requiring remedy, and the right of return is
 *   the remedy's non-negotiable core. The epsilon referent is the standing
 *   arrangement itself, assessed by this reading's own lights — hence high
 *   extractiveness and suppression. Claim and metrics are authored
 *   independently; from this seat they converge, but the engine's per-seat
 *   computation may diverge for the beneficiary seats, and that divergence is
 *   the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - palestinian_refugees_1948_descendants: Primary target (powerless/identity_locked) — barred from return for three generations; the claim is constitutive of their identity
 *   - west_bank_palestinians and gaza_palestinians: Secondary targets (moderate and powerless/trapped) — occupied and blockaded populations bearing enforcement directly
 *   - palestinian_citizens_of_israel: Secondary target with formal membership (moderate/constrained) — citizens bearing the dispossession legacy and exclusion from the self-determination clause
 *   - israeli_state: Agenda-setter and primary beneficiary (institutional/arbitrage) — sets the citizenship, land, and entry rules and collects the arrangement's gains
 *   - settler_movement_institutions and jewish_national_land_institutions: Beneficiaries (organized–institutional, mobile–arbitrage) — collect land, subsidy, and leasehold under the arrangement
 *   - refugee_camp_popular_committees: Excluded voice (powerless/trapped) — object to compensation-only formulas, structurally sidelined from final-status structures
 *   - international_legal_institutions: Analytical observer (institutional/analytical) — adjudicate the legal surface without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.88).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.9).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Standing Territorial Arrangement — Palestinian Autochthony Reading").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, 'd2713255-1177-4e8a-8140-81a207d81b6e').
narrative_ontology:cs_kernel_codification('d2713255-1177-4e8a-8140-81a207d81b6e', distributed).
narrative_ontology:cs_authority_grounding('d2713255-1177-4e8a-8140-81a207d81b6e', lineage).
narrative_ontology:cs_interpretation_layer_present('d2713255-1177-4e8a-8140-81a207d81b6e').
narrative_ontology:cs_reading_relation('d2713255-1177-4e8a-8140-81a207d81b6e', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2713255-1177-4e8a-8140-81a207d81b6e', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('d2713255-1177-4e8a-8140-81a207d81b6e', foundational, unremedied_displacement_voids_title).
narrative_ontology:cs_axiom_status(unremedied_displacement_voids_title, holdable).
narrative_ontology:cs_axiom_grounding('d2713255-1177-4e8a-8140-81a207d81b6e', unremedied_displacement_voids_title, deontological).
narrative_ontology:cs_axiom('d2713255-1177-4e8a-8140-81a207d81b6e', foundational, return_right_non_negotiable).
narrative_ontology:cs_axiom_status(return_right_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('d2713255-1177-4e8a-8140-81a207d81b6e', return_right_non_negotiable, deontological).
narrative_ontology:cs_axiom('d2713255-1177-4e8a-8140-81a207d81b6e', secondary, continuous_habitation_confers_priority).
narrative_ontology:cs_axiom_status(continuous_habitation_confers_priority, holdable).
narrative_ontology:cs_axiom_grounding('d2713255-1177-4e8a-8140-81a207d81b6e', continuous_habitation_confers_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('d2713255-1177-4e8a-8140-81a207d81b6e', pre_displacement_habitation_baseline).
narrative_ontology:cs_drift_state('d2713255-1177-4e8a-8140-81a207d81b6e', contemporary, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d2713255-1177-4e8a-8140-81a207d81b6e', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, settler_movement_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_national_land_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1948_descendants).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Were displaced in the 1948 war and again in part in 1967; registered across generations with UNRWA in Lebanon, Syria, Jordan, the West Bank, and Gaza, and in a wider diaspora. The state's citizenship and entry regime bars their return to the localities they left, and absentee-property and state-land administration converted most of their homes, farmland, and villages to other hands. Family memory, registration documents, and camp institutions transmit the claim across generations. Resettlement-and-compensation offers have been declined across three generations at heavy material cost, because accepting them is experienced as dissolving the family's standing and the collective claim. Exit in the practical sense — building a life elsewhere — has happened for many individuals; exit in the sense of relinquishing the return claim has not.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1948_descendants, payer,
    powerless, generational, identity_locked, global).

% Live under military administration in the West Bank: movement between towns and to Jerusalem runs through a permit system, land has been taken for settlements and the separation barrier through military orders and state-land declarations, and residency can be revoked. They have no sovereign of their own; the Palestinian Authority administers civil affairs in limited areas under interim-agreement terms. Leaving means emigration under a permit regime that makes re-entry precarious.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_palestinians, payer,
    moderate, biographical, trapped, regional).

% A majority are 1948 refugees and their descendants concentrated in one of the densest territories in the world. Movement in and out has been controlled by Israel and Egypt since 2007, with air, sea, and most land access closed and goods and utilities rationed. Nearly all are barred from returning to the localities inside present-day Israel their families left in 1948. Emigration is possible only through rare permits and crossings.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_palestinians, payer,
    powerless, biographical, trapped, regional).

% About a fifth of the state's citizens. They vote, hold citizenship, and use state services, but carry the legacy of wartime dispossession inside Israel: internally displaced communities barred from returning to their own villages, absentee-property transfers, unrecognized villages denied planning recognition, and land and planning administration that has historically favored the national group. The state's basic laws define self-determination as the national group's alone. Emigration is open to them but severs them from the land and community the claim runs through.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel, beneficiary).

% Administers sovereignty inside its borders and military government in the West Bank; sets citizenship, entry, land-registration, and planning rules that maintain the post-1948 exclusion of the displaced; collects and administers absentee and state land; frames the enforcement apparatus as security. It can reshape the ground through settlement construction and legislation faster than external actors can respond, and faces no internal institutional check on the exclusion rules themselves. It bears the costs of enforcement — military expenditure, international censure, and the manpower of occupation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, beneficiary).

% Organized bodies that build and populate communities in the West Bank and East Jerusalem under state sponsorship: land allocations, housing subsidies, infrastructure, and military protection. Their expansion is the visible edge of the territorial regime, and their institutions lobby for annexation. Relocating their populations back inside the state's pre-1967 lines is the demand directed at them; individually, members are mobile within the state.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, settler_movement_institutions, beneficiary,
    organized, generational, mobile, regional).

% Quasi-state bodies in the Jewish National Fund and Israel Lands Administration lineage that received and administer land purchased, confiscated, or transferred from displaced owners; lease it on terms restricted to the national group; and administer the absentee-property estate. Their charters tie them to holding land for the national group in perpetuity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_national_land_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Grassroots bodies organizing camp residents in Lebanon, Syria, Jordan, and Palestine. They have rejected compensation-only and host-state-normalization formulas, demanded direct representation in any negotiation over the return question, and organized marches and documentation projects. Negotiations run through state-level intermediaries; the committees have no seat.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, refugee_camp_popular_committees, excluded,
    powerless, generational, trapped, regional).

% The International Court of Justice, UN General Assembly and Human Rights Council bodies, and treaty committees issue advisory opinions, resolutions, and findings on the occupation, annexation, the settlement enterprise, and the refugee right of return. They adjudicate the legal surface of the arrangement and keep the return question on the international record, but hold no enforcement power over the parties.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Administers the territory's land registration, planning, and security for the population incorporated as citizens under a single sovereign legal order; allocates land, housing, and infrastructure for that population, secures it, and runs the occupied territories' day-to-day civil functions under military government.
% TRANSFER_FUNCTION: Moves land, confiscated property, leasehold rights, political standing, and freedom of movement from the displaced and occupied population to the state and the national group's institutions; moves the costs of enforcing the exclusion — military, administrative, and diplomatic — onto the state's own budget and onto the occupied population's daily life.
% ABSENT_VOICES: The displaced themselves: camp populations and their popular committees in Lebanon, Syria, Jordan, and the diaspora are not seated in final-status structures; consent is presumed by state-level intermediaries, and compensation-only formulas have been negotiated over their recorded objection. Host-state governments, which bear the camp populations' costs, also sit outside the bilateral frame.
% DISAPPEARANCE_RATIONALE: If the exclusion regime vanished overnight — borders opened to return, military government lifted, absentee and state land restored to claimants — the territorial order would rearrange around repatriation and restitution: the state's demographic and land regime would transform within years, settlement administration would unwind, regional refugee populations would move, and every institution built on the exclusion (registration, permits, the barrier, land charters) would lose its object. Nothing in the arrangement is self-sustaining absent its enforcement.
% FOUNDING_PROBLEM: In this reading's genealogy: consolidating sovereign control of the territory for one national group after the 1948 war, in a country whose Arab majority had been displaced — securing the state's demographic and territorial character against the displaced population's return.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the state's own declassified archives and Israeli 'new historian' scholarship (on Plan Dalet and the absentee-property administration) attest that displacing the Arab population and preventing its return were deliberate policy; UN General Assembly Resolution 194 (1948) and UNRWA's continuous registration attest the displacement as an unresolved international problem; ICJ advisory proceedings attest the occupation's continuing character. The 'live' status is attested by the enforcement record itself — entry denials, residency revocations, settlement approvals — not by any beneficiary's self-description.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88 at interval end) because the arrangement transfers land, property, political standing, and movement freedom from the displaced and occupied population to the state and the national group's institutions, and the transfer has compounded for three generations through absentee-property administration, state-land declarations, and settlement expansion. Suppression is higher still (0.90) because persistence depends on active enforcement — entry denial, the permit regime, the blockade, military government — not on participant preference; the return alternative is not unchosen but barred. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope. Theater (0.38) tracks a substantial performative layer — autonomy arrangements and peace-process sovereignty that simulated remedy while control deepened; the ratio peaked around the Oslo decade and has decayed as the pretense is openly abandoned. The base_extractiveness dip at t=45 reflects the Oslo autonomy interlude, when formal Palestinian institutions were created, before settlement expansion under negotiation cover restored the trajectory. Accessibility_collapse (0.68) sits below natural-law grade: the return alternative is legally barred and physically engineered against, but international legal avenues, refugee institutions, and demographic persistence keep it live. Resistance (0.78) has been sustained across generations — uprisings, legal campaigns, boycott movements, and armed resistance — and the victim class, though numerous, is jurisdictionally fragmented across host states, which suppresses coalition power despite shared position.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute opposite types from the same structure. From the state's position the arrangement is sovereign self-administration: security, land allocation for its citizens, continuity of the national home. From the refugee and occupied seats the same structure is enforced dispossession: the permit that secures one population is the wall against the other's return. Same-level dynamics differentiate the payer seats: west_bank and gaza Palestinians hold the same nominal position (occupied, stateless) but different enforcement regimes — a permit-and-settlement regime versus a total blockade — giving them different exit profiles and different daily exposure. The refugees' identity lock is relational and institutional: self-concept and family standing are constituted through displacement and return, reinforced by UNRWA's matrilineal registration; if that frame broke — a mass-accepted compensation-plus-settlement formula — the victim structure itself would dissolve into a negotiated transfer, and the arrangement's classification from the remaining seats would shift accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the beneficiary end: the state sets and collects; settler and land institutions collect land, subsidy, and leasehold without running the enforcement core. The refugee seat sits at the full-target end: identity-locked, no arbitrage exit, three generations of accumulated transfer. The occupied and blockaded seats sit near-full-target: trapped by permit and blockade regimes. Palestinian citizens of Israel sit high-target despite formal citizenship — the citizenship does not damp the burden they bear (internal-displacement legacies, unrecognized villages, exclusion from the self-determination clause), which is why no directionality override is needed: the victim declaration carries their position. The excluded camp-committee seat would sit at full target if seated; its exclusion is part of how the arrangement keeps the remedy question off the table.
 *
 * MANDATROPHY ANALYSIS:
 *   From this seat there is no mandatrophy to resolve: the arrangement's operative function — maintaining the exclusion — is live and actively enforced, so no degraded-shell or zombie reading applies. The divergence is between the legitimizing story (refuge, security, national home) and the operative function (perpetuating the displacement), and it runs through enforcement, not inertia. The temporal series shows the theater layer rising through the Oslo decade and falling as the pretense is abandoned — the opposite of a mandate outliving its function; the function never died. The snare classification from this seat prevents the security-administration story from laundering the transfer as coordination cost, while the separability omega keeps the honest version of that challenge alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story is one reading (palestinian_autochthony_reading) of the territorial_legitimacy_dual kernel: does unremedied 1948 displacement void or merely qualify sovereign title to the territory? The sibling readings answer differently — the zionist_refuge_reading treats the standing arrangement as the remedy to persecution (legitimacy conferred by partition acceptance), and the two_state_coexistence_reading locates the wrong in the post-1967 occupation only, accepting the 1948 frame.',
    'No empirical resolution: the disagreement is located in the adopted theory of territorial legitimacy (habitation-and-remedy vs. recognition-and-control vs. dual recognition). Resolution would require the parties to converge on one legitimacy theory, not new data.',
    'Under the sibling readings'' seats, the same standing arrangement computes with far lower reading-indexed extractiveness and different types; this file''s claim is valid only within this reading''s framework. Cross-reading comparison of epsilon is meaningless — the readings share the referent, not the values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: which theory of territorial legitimacy governs the standing arrangement''s assessment.').

omega_variable(
    return_right_scope,
    'What does the non-negotiable right of return concretely require: individual return to original localities (many now inside Israel proper, some destroyed), collective return to a sovereign Palestinian state, or a return-plus-compensation menu chosen by each refugee family?',
    'Refugee-community referenda and representative polling on remedy formulas; analysis of which formulas camp institutions have accepted or rejected in past negotiation rounds.',
    'The victim set and the remedy''s land arithmetic differ materially across scopes: individual return implies transformation of the state''s core; return-to-a-state is compatible with a two-state frame; a compensation menu matches the arrangements the camp committees have rejected. The reading''s claim that the standing arrangement requires remedy holds under all scopes, but what counts as remedy does not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(return_right_scope, conceptual, 'Scope ambiguity in the return right — the reading''s central remedy is underspecified between individual, collective, and menu forms.').

omega_variable(
    remedy_factual_viability,
    'Has continued settlement expansion and demographic change made the remedy this reading demands factually unachievable — converting a live wrong into a formally live but practically foreclosed one?',
    'Settlement-footprint and built-area trajectory analysis against the territory a return would require; demographic projections of the registered refugee population; precedent analysis of large-scale repatriations elsewhere.',
    'If the remedy is factually foreclosed, the reading''s claim shifts from ''the arrangement must be remedied'' to ''the arrangement is an accomplished wrong'' — changing what persistence means (entrenchment vs. ongoing extraction) and strengthening the case that the operative question has moved from remedy to compensation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_factual_viability, empirical, 'Whether the reading''s remedy remains factually achievable on the ground.').

omega_variable(
    coordination_extraction_separability,
    'Is the arrangement''s security-administration function separable from its return-denial function? This reading holds they are inseparable — the same permit, registration, and military apparatus that administers security for the beneficiary population is the apparatus that bars return — but a framework that separated them would attribute part of the measured burden to genuine coordination cost.',
    'Counterfactual institutional-design analysis: could a security administration serving the current population operate without the entry, land, and planning rules that maintain the exclusion? Compare enforcement activity directed at security proper versus activity directed at maintaining the exclusion.',
    'If separable, part of the authored extractiveness is coordination cost and the tangled_rope reading strengthens even within this framework; if inseparable, the full measure stands and the coordination story is cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the arrangement''s coordination and dispossession functions are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t19, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 19, 0.18).
narrative_ontology:measurement_basis(terr_tr_t19, observed).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement_basis(terr_tr_t32, observed).
narrative_ontology:measurement(terr_tr_t45, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement_basis(terr_tr_t45, observed).
narrative_ontology:measurement(terr_tr_t57, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 57, 0.5).
narrative_ontology:measurement_basis(terr_tr_t57, observed).
narrative_ontology:measurement(terr_tr_t67, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 67, 0.44).
narrative_ontology:measurement_basis(terr_tr_t67, observed).
narrative_ontology:measurement(terr_tr_t77, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 77, 0.38).
narrative_ontology:measurement_basis(terr_tr_t77, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t19, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 19, 0.83).
narrative_ontology:measurement_basis(terr_be_t19, observed).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 32, 0.84).
narrative_ontology:measurement_basis(terr_be_t32, observed).
narrative_ontology:measurement(terr_be_t45, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 45, 0.8).
narrative_ontology:measurement_basis(terr_be_t45, observed).
narrative_ontology:measurement(terr_be_t57, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 57, 0.84).
narrative_ontology:measurement_basis(terr_be_t57, observed).
narrative_ontology:measurement(terr_be_t67, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 67, 0.86).
narrative_ontology:measurement_basis(terr_be_t67, observed).
narrative_ontology:measurement(terr_be_t77, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 77, 0.88).
narrative_ontology:measurement_basis(terr_be_t77, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t19, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 19, 0.73).
narrative_ontology:measurement_basis(terr_su_t19, observed).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement_basis(terr_su_t32, observed).
narrative_ontology:measurement(terr_su_t45, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 45, 0.78).
narrative_ontology:measurement_basis(terr_su_t45, observed).
narrative_ontology:measurement(terr_su_t57, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 57, 0.84).
narrative_ontology:measurement_basis(terr_su_t57, observed).
narrative_ontology:measurement(terr_su_t67, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 67, 0.87).
narrative_ontology:measurement_basis(terr_su_t67, observed).
narrative_ontology:measurement(terr_su_t77, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 77, 0.9).
narrative_ontology:measurement_basis(terr_su_t77, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the legitimacy of the territorial arrangement' decomposes, per the epsilon-invariance principle, into three reading-indexed constraint stories of one kernel: this file (palestinian_autochthony_reading, claimed snare, high reading-indexed extractiveness), zionist_refuge_reading (the standing arrangement as remedy to persecution, low extractiveness from its seat), and two_state_coexistence_reading (post-1967 occupation as the wrong, intermediate extractiveness). The readings share the referent and differ in values; they are linked here so contamination and cross-reading analysis can run. This reading influences the coexistence reading (the non-negotiable return claim sets the terms the compromise frame must accommodate) and coexists with the refuge reading (rival legitimacy theories held by different parties; a remedial-title framework could hold elements of both, so neither strictly forecloses the other).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
