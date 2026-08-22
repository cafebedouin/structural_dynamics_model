% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Standing Displacement Arrangement under the Palestinian Autochthony Reading
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   Under the Palestinian autochthony reading, the standing arrangement under
 *   contest is the post-1948 order that displaced the majority of the
 *   Palestinian population, barred its return, transferred its land and
 *   property to state custody and allocative control, and layered occupation,
 *   settlement expansion, and blockade onto the remainder of the territory.
 *   The reading holds that continuous habitation grounds title, that the
 *   displacement is an open injustice rather than a settled founding cost,
 *   and that return is non-negotiable. Epsilon's referent is that standing
 *   arrangement as this reading assesses it — never the return-implementing
 *   order the reading endorses. The claimed type (snare) and the metrics are
 *   authored independently: the claim states what this seat believes is
 *   structurally true; the metrics state what is descriptively true of the
 *   arrangement's operation; the engine computes per-seat classifications
 *   from the structural data, and divergence between claim and computation is
 *   signal, not error. This story is one member of a three-story constraint
 *   family decomposing the territorial-legitimacy kernel; the siblings are
 *   separate files with their own epsilon and victim sets.
 *
 * KEY AGENTS:
 *   - palestinian_refugees_diaspora: primary target (powerless/identity_locked) — bears the arrangement's core deprivation; the return claim is their constitutive intergenerational identity
 *   - west_bank_palestinians: primary target (moderate/constrained) — bears the occupation's daily costs under permit and planning regimes
 *   - gaza_residents: primary target (powerless/trapped) — bears blockade and recurrent large-scale operations with no exit
 *   - israeli_state_apparatus: agenda setter (institutional/arbitrage) — administers and enforces the arrangement and receives its principal gains
 *   - israeli_civic_polity: broad beneficiary (powerful/mobile) — inherits the redistributed space as ordinary citizenship
 *   - west_bank_settler_movement: concentrated beneficiary (organized/constrained) — holds the frontier gains under state subsidy and protection
 *   - palestinian_authority: dual-positioned intermediary (organized/constrained) — administers civil affairs while its budget and standing depend on the framework continuing
 *   - arab_host_state_governments: mixed seat (institutional/mobile) — bear hosting costs while some regimes bank political utility from unresolved status
 *   - palestinian_citizens_of_israel: target with partial membership (moderate/constrained) — votes and litigates inside the state that holds their community's confiscated property
 *   - un_mandate_bodies: analytical observer (institutional/analytical) — records, services, and affirms norms without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.9).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.93).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.93).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Standing Displacement Arrangement under the Palestinian Autochthony Reading").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '839e540f-7d17-46a6-a9a0-2366bddc669a').
narrative_ontology:cs_kernel_codification('839e540f-7d17-46a6-a9a0-2366bddc669a', distributed).
narrative_ontology:cs_authority_grounding('839e540f-7d17-46a6-a9a0-2366bddc669a', lineage).
narrative_ontology:cs_interpretation_layer_present('839e540f-7d17-46a6-a9a0-2366bddc669a').
narrative_ontology:cs_reading_relation('839e540f-7d17-46a6-a9a0-2366bddc669a', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('839e540f-7d17-46a6-a9a0-2366bddc669a', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('839e540f-7d17-46a6-a9a0-2366bddc669a', foundational, continuous_habitation_grounds_title).
narrative_ontology:cs_axiom_status(continuous_habitation_grounds_title, holdable).
narrative_ontology:cs_axiom_grounding('839e540f-7d17-46a6-a9a0-2366bddc669a', continuous_habitation_grounds_title, deontological).
narrative_ontology:cs_axiom('839e540f-7d17-46a6-a9a0-2366bddc669a', foundational, displacement_claim_not_extinguished_by_time).
narrative_ontology:cs_axiom_status(displacement_claim_not_extinguished_by_time, holdable).
narrative_ontology:cs_axiom_grounding('839e540f-7d17-46a6-a9a0-2366bddc669a', displacement_claim_not_extinguished_by_time, deontological).
narrative_ontology:cs_axiom('839e540f-7d17-46a6-a9a0-2366bddc669a', secondary, recognition_conditioned_on_remedy).
narrative_ontology:cs_axiom_status(recognition_conditioned_on_remedy, holdable).
narrative_ontology:cs_axiom_grounding('839e540f-7d17-46a6-a9a0-2366bddc669a', recognition_conditioned_on_remedy, conventional).
narrative_ontology:cs_reference_frame('839e540f-7d17-46a6-a9a0-2366bddc669a', pre_displacement_continuous_habitation_order).
narrative_ontology:cs_drift_state('839e540f-7d17-46a6-a9a0-2366bddc669a', contemporary_entrenchment_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('839e540f-7d17-46a6-a9a0-2366bddc669a', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_civic_polity).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_settler_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_authority).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_host_state_governments).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_residents).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_host_state_governments).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, effective_control_supersedes_legal_title).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, demographic_majority_security_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Families displaced in 1948 and 1967 and their descendants, registered for relief across Lebanon, Syria, Jordan, the occupied territories, and further afield. Many hold deeds and keys to homes now occupied by others. In several host countries they face barred professions and blocked naturalization; nowhere can they exercise the return they claim. They are absent from every forum that has negotiated their status — the interim accords deferred the question, and the final-status talks never convened them. Political identity across generations centers on return; accepting permanent resettlement elsewhere is widely experienced as abandoning the claim and the dead who kept it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_diaspora, payer,
    powerless, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_diaspora, excluded).

% Residents of the West Bank living under military administration: permit regimes govern work, building, and movement; most of the land is zoned unavailable to them while adjacent settlement grows. Some work inside Israel under revocable permits; emigration is possible but costly and reads locally as desertion. Their own ministries run day-to-day civil affairs in the towns, but borders, water, planning, and security remain externally controlled.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_palestinians, payer,
    moderate, biographical, constrained, regional).

% Residents of the Gaza Strip living behind a land, air, and sea blockade punctuated by large-scale military operations. Movement in and out is tightly restricted; unemployment and reconstruction cycles dominate daily economics. There is effectively no exit: the borders close, and third-country resettlement paths are negligible. Most are themselves refugees or their descendants, holding the return claim under the tightest conditions of any seat.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_residents, payer,
    powerless, immediate, trapped, regional).

% The governing institutions of Israel: they administer the territory's security, population registries, planning, and borders; they execute the laws that bar refugee return and hold the displaced's confiscated property in state custody for allocation. They set the terms under which every other party here operates, and they can adjust enforcement intensity — checkpoint policy, permit quotas, settlement approvals, revenue transfers — without structural change to their own position.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Israel's Jewish citizenry, for whom the state provides housing, a conscription-based social contract, and a national home built substantially on land and property lost by the displaced. Individual exit exists — emigration, dual nationality — but the polity as such is bound to the state's continuation. Most members experience the arrangement as ordinary citizenship rather than as a position in a dispute; the costs they carry are the security burden and international censure.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_civic_polity, beneficiary,
    powerful, generational, mobile, national).

% Organized communities and their institutions building and living in settlements across the West Bank, backed by state subsidies, army protection, and planning priority. Their presence is the arrangement's most visible physical fact and its fastest-compounding component. Removal would require uprooting hundreds of thousands of people and confronting a settlement commitment many hold as religious duty; the 2005 Gaza evacuation is the template they organize against.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_settler_movement, beneficiary,
    organized, generational, constrained, regional).

% The governing body administering Palestinian civil affairs in parts of the West Bank under interim accords now three decades old. It collects taxes transferred at the counterparty's discretion, pays salaries, and coordinates security with Israel; its officials' positions, budgets, and international standing depend on the interim framework continuing. Elections have been indefinitely postponed since 2006, and its mandate's representativeness — particularly over the refugee constituency — is disputed among the people it administers.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_authority, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_authority, agenda_setter).

% Governments of the states hosting large refugee populations. They bear service costs, fiscal strain, and political friction; their policies diverge sharply — citizenship extended in Jordan, profession and property bans in Lebanon, residency rights in Syria. Several regimes have at times drawn political utility from the unresolved status, championing the cause externally while restricting the refugees domestically, and each retains the option to re-price its hosting relationship as regional politics shift.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_host_state_governments, payer,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_host_state_governments, beneficiary).

% Palestinians holding Israeli citizenship, roughly a fifth of the state's population. They vote, litigate, and serve in the state's institutions while carrying family histories of internal displacement and village destruction; land laws transferred much of their community's pre-1948 property to state custody. Emigration is available but severs them from the only citizenship they hold, and their standing inside the polity is conditional in ways their votes do not fully secure.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel, beneficiary).

% United Nations agencies and bodies: the relief agency delivers services to registered refugees; the General Assembly reaffirms the return principle annually; Security Council resolutions and advisory opinions address the occupation's legality. None holds enforcement power over the arrangement. Their role is record-keeping, service provision, and norm articulation — they witness the structure and can name it, but cannot move it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, un_mandate_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a single sovereign governance order over the territory: security administration, population registries, planning, utilities, and economic regulation are solved once, centrally, by the controlling state; a parallel relief architecture (registration, services) manages the displaced population across host states.
% TRANSFER_FUNCTION: Moves land, housing, water resources, and political self-determination from the displaced Palestinian population to the Israeli state and, through it, to the civic polity and settlement enterprise; moves a share of the displaced population's maintenance costs onto the international community and the host states.
% ABSENT_VOICES: The refugees themselves — the direct holders of the return claim — sit outside every forum that has addressed their status: the interim accords deferred the question, the final-status rounds never convened them, and the diaspora votes in no election that governs the claim. Host-country camp populations speak through no state of their own. The unanimity of elite frameworks over decades rests partly on this absence.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — return barriers lifted, occupation administration dissolved, property custody ended — millions of people would move or lodge claims, borders and citizenship regimes would be renegotiated, the settlement enterprise would face immediate legal challenge, and the region's diplomatic architecture would reorganize around implemented rather than deferred remedies.
% FOUNDING_PROBLEM: After the 1948 war, the new state faced the presence of a large displaced Arab population whose return its founders held incompatible with a Jewish-majority self-determination project; the arrangement — mass prevention of return, absentee-property custody, armistice lines hardened into borders — was built to consolidate the state against that return.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: United Nations General Assembly Resolution 194(III) and its annual reaffirmation attest the return claim's standing; International Court of Justice advisory proceedings and Security Council Resolution 2334 attest the occupation's contested legality; the Israeli 'new historian' archival work corroborates the expulsion-and-prevention narrative from inside the state's own documentary record; and Amnesty International and Human Rights Watch findings attest the structure of the discrimination. No attesting source sits inside the beneficiary set alone.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.9, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.90 at interval end) because the arrangement transfers land, housing, water, and self-determination from the displaced population to the controlling polity, and the transfer compounds: each year of non-return converts a temporary fact into inherited title. Suppression (0.93) is higher still and is authored as a raw structural property — it is NOT scaled by power or scope in the way extractiveness is; the arrangement's persistence depends on physically barring return, militarily administering the territories, blockading Gaza, and maintaining legal barriers (absentee-property and citizenship regimes) that foreclose the remedy. Theater (0.40) is substantial but minority: checkpoints, planning regimes, and settlement construction are brutally functional; the performative layer is the permanent negotiation industry — interim autonomy without sovereignty, process without endpoints, 'economic peace' framing — which peaked at Oslo (0.48 in 1993) and has plateaued near 0.4 since. Accessibility collapse is 0.68: alternatives are normatively alive (annual General Assembly affirmation of return, advisory opinions) but practically collapsed — every negotiated channel has deferred the core claim, and unilateral channels meet overwhelming force. Resistance is 0.78: two intifadas, boycott movements, litigation, and diplomatic campaigns; the arrangement meets real, recurring, costly resistance. The temporal series run on one shared eight-point grid so every metric is authored at every examined time point. The series oscillate rather than drift monotonically: calm, accumulation, uprising, crackdown, negotiation process, repeat. The oscillation is partly the extraction mechanism itself — each peace-process phase absorbs resistance momentum (suppression and extraction dip, theater spikes) while facts on the ground accumulate, so the troughs finance the peaks (intermittent reinforcement, not noise). The base_properties scalars reflect the 2025 accumulation-phase endpoint. Coalition check: the victim set is large and plural but its coalition potential is repeatedly fractured — host-state divergence (citizenship in Jordan, exclusion in Lebanon), Authority-Israel security coordination, and Gaza's isolation prevent the class-level coalition that the refugees' numbers would otherwise support.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the state apparatus and civic polity, the arrangement is legitimate statecraft: security administration, civil registries, a working sovereign order — a coordination frame in which the displaced appear as a demographic-security question. From the refugee, West Bank, and Gaza seats, the identical structure operates as force-maintained dispossession: the coordination story is the cover, and the enforcement is the point. The observer seat sees contested legality — affirmed norms without enforcement. Same-level lateral differentiation: palestinian_citizens_of_israel and west_bank_palestinians are the same people divided by a citizenship line — one holds votes, courts, and municipal standing; the other holds permits and military orders — so equal origin produces unequal exit. Identity-lock dynamics: the refugees' exit is identity_locked, not merely trapped — the return claim is constituted through transmitted keys, deeds, and Nakba commemoration across generations; permanent resettlement elsewhere is experienced as betraying the dead who kept the claim alive. If that identity frame broke — through a package combining acknowledgment, choice, and material remedy — the seat's computed directionality would soften and the arrangement's classification for that seat could shift toward a hybrid coordination reading; the lock, not the geography alone, is what holds d near the full-target end.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the state apparatus sits nearest the beneficiary end (it writes and enforces the rules and receives the gains); the civic polity and settler movement collect through it (housing, land allocation, subsidies) with modest damping from their own exposure to resistance; the Authority sits mid-low — it collects salaries, budgets, and international standing, but its revenue is held at the counterparty's discretion, making it a partly captured beneficiary; host-state governments sit near symmetric, bearing real hosting costs while some regimes draw political utility. Victim declarations map to high directionality: refugees approach the full-target end (identity_locked exit amplifies), Gaza residents near it (trapped), West Bank residents high (constrained), citizens of Israel moderately high with partial-membership damping. No directionality overrides are authored: the derivation chain from declared roles, power atoms, and exit options reproduces these relationships, and the override mechanism keys on the power atom — our moderate-power agents are uniformly target-side, so an override calibrated for one would distort the others.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — consolidating a sovereign Jewish-majority state after 1948 while preventing the displaced population's return, which the founders held incompatible with that project — remains live: the beneficiaries still receive what the arrangement was built to deliver, and the harm it was built to impose is still being imposed. Status=live paired with disappearance_verdict=world_rearranges produces no zombie mismatch: this is not a mandate that outlived its function but a function still operating at full draw for its beneficiaries. The classification discipline matters in both directions here. Reading the arrangement as rope would erase the victims — the coordination it performs (sovereign order) is real but is not what the enforcement machinery defends; the machinery defends the exclusions. Reading it as piton would ignore the concentrated gain and the active, intensifying enforcement: this is not inertia but maintenance, and the maintenance is paid for because the gains are captured. The snare claim keeps both facts visible: a functioning sovereign order as the shell, and the suppression of the remedy as the load it carries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (palestinian_autochthony_reading) of the territorial_legitimacy_dual kernel; what would change structurally if a sibling reading (zionist_refuge_reading, two_state_coexistence_reading) were adopted instead?',
    'Cross-reading comparison of the compiled sibling stories: victim/beneficiary inversion or dissolution, re-based epsilon, changed enforcement profile.',
    'Adopting the refuge reading recasts the displaced as the cost side of a legitimate founding and lowers measured extraction; adopting the coexistence reading re-keys legitimacy to mutual recognition and dissolves the autochthony victim set into negotiating parties. The disagreement is located in what grounds title, and therefore in who counts as victim and who as beneficiary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed identity of the constraint within its contested kernel.').

omega_variable(
    epsilon_referent_standing_arrangement,
    'Is epsilon here anchored to the standing arrangement (displacement without remedy, occupation, settlement entrenchment) as this reading assesses it, rather than to the return-implementing arrangement this reading endorses?',
    'Re-reading the story''s referent declaration against the chi-formula inputs; any assessment of the endorsed alternative belongs to a different story.',
    'Misanchoring would collapse epsilon toward zero for every advocacy reading and destroy cross-reading comparability; correct anchoring keeps this reading''s high epsilon meaningful over the fixed referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_standing_arrangement, conceptual, 'Fixes what the extractiveness score is about: the arrangement under contest, not the endorsed remedy.').

omega_variable(
    return_feasibility_and_remedy_content,
    'How much of the descendant population''s return is physically and economically implementable, and does feasibility change the remedy''s content (full return, partial return plus compensation, acknowledged individual choice)?',
    'Demographic and land-capacity modeling combined with negotiated implementation studies.',
    'Does not alter the standing arrangement''s epsilon or type; determines whether the reading''s demand is executable or aspirational, and therefore what resolution of the underlying conflict would look like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(return_feasibility_and_remedy_content, empirical, 'Feasibility boundary on the remedy this reading demands.').

omega_variable(
    oslo_architecture_function,
    'Did the Oslo-era institutions function as transitional scaffolding toward Palestinian statehood, or as management cover under which entrenchment accelerated?',
    'Counterfactual trajectory analysis: settlement-unit growth, permit trends, and Area C land allocation during the interim period versus before and after.',
    'If transitional scaffolding that failed its sunset, a separate scaffold story with its own decay profile should be decomposed out of this one; if cover, the theater_ratio measured here already prices it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oslo_architecture_function, empirical, 'Whether the interim architecture warrants its own decomposed story.').

omega_variable(
    host_state_persistence_contribution,
    'What share of the arrangement''s persistence is attributable to host-state policies (naturalization bans, camp segregation, political instrumentalization) rather than to the enforcing state''s machinery?',
    'Comparative policy analysis across Jordanian, Lebanese, and Syrian trajectories, with naturalization-counterfactual modeling.',
    'High host-state contribution widens the victims'' suppression front and complicates coalition remedies; low contribution concentrates responsibility and simplifies the enforcement picture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(host_state_persistence_contribution, empirical, 'Multi-front attribution of the persistence mechanism.').

omega_variable(
    representation_validity,
    'Does the Palestinian Authority''s mandate extend to the refugee constituency, given elections indefinitely postponed since 2006 and the diaspora''s exclusion from every negotiating forum?',
    'Survey evidence on representational legitimacy among camp and diaspora populations; electoral-pathway analysis.',
    'If representation fails, any bargain signed by the Authority does not bind the return claim''s holders, extending the arrangement''s persistence beyond any elite agreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representation_validity, empirical, 'Whether the seated intermediary speaks for the core claim-holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tlpar_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement_basis(tlpar_tr_t1948, observed).
narrative_ontology:measurement(tlpar_tr_t1950, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement_basis(tlpar_tr_t1950, observed).
narrative_ontology:measurement(tlpar_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement_basis(tlpar_tr_t1967, observed).
narrative_ontology:measurement(tlpar_tr_t1988, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1988, 0.22).
narrative_ontology:measurement_basis(tlpar_tr_t1988, observed).
narrative_ontology:measurement(tlpar_tr_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1993, 0.48).
narrative_ontology:measurement_basis(tlpar_tr_t1993, observed).
narrative_ontology:measurement(tlpar_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement_basis(tlpar_tr_t2000, observed).
narrative_ontology:measurement(tlpar_tr_t2005, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement_basis(tlpar_tr_t2005, observed).
narrative_ontology:measurement(tlpar_tr_t2025, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(tlpar_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tlpar_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.9).
narrative_ontology:measurement_basis(tlpar_be_t1948, observed).
narrative_ontology:measurement(tlpar_be_t1950, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1950, 0.82).
narrative_ontology:measurement_basis(tlpar_be_t1950, observed).
narrative_ontology:measurement(tlpar_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.86).
narrative_ontology:measurement_basis(tlpar_be_t1967, observed).
narrative_ontology:measurement(tlpar_be_t1988, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1988, 0.83).
narrative_ontology:measurement_basis(tlpar_be_t1988, observed).
narrative_ontology:measurement(tlpar_be_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1993, 0.76).
narrative_ontology:measurement_basis(tlpar_be_t1993, observed).
narrative_ontology:measurement(tlpar_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement_basis(tlpar_be_t2000, observed).
narrative_ontology:measurement(tlpar_be_t2005, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2005, 0.88).
narrative_ontology:measurement_basis(tlpar_be_t2005, observed).
narrative_ontology:measurement(tlpar_be_t2025, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2025, 0.9).
narrative_ontology:measurement_basis(tlpar_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(tlpar_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.78).
narrative_ontology:measurement_basis(tlpar_su_t1948, observed).
narrative_ontology:measurement(tlpar_su_t1950, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1950, 0.74).
narrative_ontology:measurement_basis(tlpar_su_t1950, observed).
narrative_ontology:measurement(tlpar_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement_basis(tlpar_su_t1967, observed).
narrative_ontology:measurement(tlpar_su_t1988, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1988, 0.86).
narrative_ontology:measurement_basis(tlpar_su_t1988, observed).
narrative_ontology:measurement(tlpar_su_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement_basis(tlpar_su_t1993, observed).
narrative_ontology:measurement(tlpar_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement_basis(tlpar_su_t2000, observed).
narrative_ontology:measurement(tlpar_su_t2005, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2005, 0.91).
narrative_ontology:measurement_basis(tlpar_su_t2005, observed).
narrative_ontology:measurement(tlpar_su_t2025, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2025, 0.93).
narrative_ontology:measurement_basis(tlpar_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'whose legitimacy governs this territory' decomposes into three structurally distinct constraints sharing one kernel, per the epsilon-invariance principle: this autochthony reading (epsilon anchored to the standing displacement arrangement; victims are the displaced), zionist_refuge_reading (epsilon anchored to delegitimation pressure on the state; the state's civic polity occupies the target seat), and two_state_coexistence_reading (epsilon anchored to the compromise's stability; maximalists on both sides occupy the target seats). Each file carries its own stable epsilon over its own referent; the edges declared here route contamination propagation across the family — degradation of any reading's standing (a failed negotiation round, a ruling, an uprising) shifts the operating environment of the other two without merging them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
