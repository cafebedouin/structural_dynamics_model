% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__political_zionism_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionism: Sovereignty-and-Majority Remedy to Antisemitism
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   From the First Zionist Congress (Basel, 1897), the political Zionist
 *   program sought a 'publicly and legally assured home' for the Jewish
 *   people in Palestine, and progressively hardened that aim into a
 *   requirement of territorial sovereignty with a Jewish demographic
 *   majority. The program was carried by three instruments operating on one
 *   structure: great-power diplomacy (the 1917 Balfour Declaration and the
 *   League Mandate), organized immigration and land purchase, and finally the
 *   1947-49 war, which produced a Jewish-majority state alongside the
 *   displacement of roughly 700,000 Arabs whose return was subsequently
 *   denied. The same structure that delivered collective rescue and
 *   self-government to a persecuted minority imposed dispossession and
 *   permanent statelessness on the country's existing majority population.
 *   This file instantiates one reading of the decomposed territorial-claim
 *   kernel (see kernel_context); its epsilon referent is the standing
 *   sovereignty-and-majority arrangement as it actually operated, 1897-1949,
 *   not any alternative arrangement any party endorsed. KEY AGENTS (by
 *   structural relationship): - zionist_congress_leadership: Agenda-setting
 *   beneficiary (institutional/identity_locked) - plans and directs the
 *   program - yishuv_settler_community: Primary beneficiary
 *   (organized/constrained) - receives land, refuge, citizenship -
 *   european_jewish_refugees: Dependent beneficiary (powerless/trapped) - the
 *   rescue case that justifies the program - palestinian_arab_communities:
 *   Primary target (moderate/trapped) - bears dispossession -
 *   palestinian_refugee_diaspora: Residual target (powerless/trapped) - bears
 *   the arrangement's standing unpaid cost - british_mandate_authority:
 *   External administrator (institutional/arbitrage) - converts aspiration
 *   into law, then exits - great_power_patrons: Secondary beneficiary
 *   (institutional/arbitrage) - collects strategic alignment -
 *   binationalist_jewish_dissenters: Excluded voice
 *   (moderate/identity_locked) - outvoted inside the movement -
 *   neighboring_arab_states: Cost-bearing belligerents
 *   (institutional/constrained) - historians_of_nationalism: Analytical
 *   observer (analytical/analytical) - sees the full decision record.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.84).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.8).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionism: Sovereignty-and-Majority Remedy to Antisemitism").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, 'c84393d8-42d0-4126-a388-4ab1ccc7692c').
narrative_ontology:cs_kernel_codification('c84393d8-42d0-4126-a388-4ab1ccc7692c', formalized).
narrative_ontology:cs_authority_grounding('c84393d8-42d0-4126-a388-4ab1ccc7692c', lineage).
narrative_ontology:cs_interpretation_layer_present('c84393d8-42d0-4126-a388-4ab1ccc7692c').
narrative_ontology:cs_reading_relation('c84393d8-42d0-4126-a388-4ab1ccc7692c', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('c84393d8-42d0-4126-a388-4ab1ccc7692c', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('c84393d8-42d0-4126-a388-4ab1ccc7692c', jewish_territorial_claim__revisionist_zionism_reading, influences).
narrative_ontology:cs_axiom('c84393d8-42d0-4126-a388-4ab1ccc7692c', foundational, sovereignty_necessary_for_jewish_security).
narrative_ontology:cs_axiom_status(sovereignty_necessary_for_jewish_security, holdable).
narrative_ontology:cs_axiom_grounding('c84393d8-42d0-4126-a388-4ab1ccc7692c', sovereignty_necessary_for_jewish_security, empirically_contingent).
narrative_ontology:cs_axiom('c84393d8-42d0-4126-a388-4ab1ccc7692c', foundational, jewish_majority_precondition_of_legitimate_self_determination).
narrative_ontology:cs_axiom_status(jewish_majority_precondition_of_legitimate_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('c84393d8-42d0-4126-a388-4ab1ccc7692c', jewish_majority_precondition_of_legitimate_self_determination, conventional).
narrative_ontology:cs_reference_frame('c84393d8-42d0-4126-a388-4ab1ccc7692c', basel_publicly_assured_home).
narrative_ontology:cs_drift_state('c84393d8-42d0-4126-a388-4ab1ccc7692c', post_1949_armistice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c84393d8-42d0-4126-a388-4ab1ccc7692c', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_congress_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, yishuv_settler_community).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, european_jewish_refugees).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, great_power_patrons).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_communities).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_refugee_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, british_mandate_authority).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, neighboring_arab_states).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, statehood_as_persecution_remedy).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, demographic_majority_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected bodies — the Zionist Congress, its executive, and later the Jewish Agency — set immigration policy, land-purchase strategy, and great-power diplomacy, treating the demographic ratio in Palestine as the movement's central planning variable. Dues, donations, and diplomatic capital flow to them. Their careers, reputations, and life-work are fused with the program's success; abandoning the sovereignty-and-majority aim is unthinkable from inside, and private papers show candor about the demographic problem that public testimony softens.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_congress_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Immigrants who built towns, farms, unions, and schools in Palestine. Land tenure, employment, Hebrew-language public life, and eventually citizenship and physical safety flow to them. Sunk capital, planted lives, and membership in the defense organizations that became the state's army tie them to staying; many staff the very institutions that administer immigration, land, and security. Leaving means forfeiting everything built.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, yishuv_settler_community, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, yishuv_settler_community, agenda_setter).

% Jews fleeing pogroms, Nazi persecution, and postwar displacement camps. The sovereign-home program is the one destination that promised admission when the Evian conference states and the 1939 White Paper closed the others. Rescue, shelter, and nationality flow to those who arrive; they do not set the program's terms, and their desperation is the moral engine of its legitimacy. Their exit options are nil — the trap is Europe itself.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, european_jewish_refugees, beneficiary,
    powerless, immediate, trapped, continental).

% Farmers, tenants, and townsmen who formed the country's overwhelming majority before mass migration. Tenancies dissolve as absentee landowners sell; organized immigrant labor undercuts their wages; village land is registered away through purchase and, after 1947-49, through outright flight and expulsion. Those who remain live under military administration and land-confiscation statutes; those who flee become the refugee population. Exit means exile to Lebanon, Jordan, or Gaza — leaving is the injury, not relief from it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_communities, payer,
    moderate, generational, trapped, regional).

% Roughly 700,000 people displaced in 1947-49 and their descendants. Homes, orchards, bank accounts, and communal property remain behind under a custodian of absentees' property and are never returned. Host states bar integration; the new state bars return by force of arms. Their claim — restitution and return — is the arrangement's standing unpaid cost, transmitted intact to children born in camps.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, regional).

% Issued the 1917 declaration and administered the League mandate that gave it legal force, balancing Jewish immigration against Arab revolt until the 1939 White Paper reversed course to court the Arab world on the eve of war. Collected a strategic position guarding the Suez approach; paid in soldiers, treasury, and the 1936-39 suppression campaign. In 1948 it exercised the cleanest exit in the story: handing the unsolved problem to the United Nations and withdrawing entirely.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandate_authority, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, british_mandate_authority, beneficiary).

% Egypt, Transjordan, Syria, Lebanon, and Iraq intervened in 1948 and were defeated. They absorbed the refugee population they neither integrated nor repatriated, and their domestic politics reorganized around the unresolved result — coups, pan-Arab mobilization, and successive wars. As sovereign states they retain more mobility than the refugees themselves, but the conflict's costs are levied on them regardless of their choices.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, neighboring_arab_states, payer,
    institutional, generational, constrained, regional).

% The Brit Shalom circle, Judah Magnes, Martin Buber, and the older Ahad Ha'am tradition argued for a shared binational polity or a spiritual-cultural center without demographic supremacy. Outvoted in every congress, their position shrank to moral witness as war overtook argument. They remained inside the movement they dissented from — their Zionist identity left them no exit — and their proposals are preserved mainly in the archival record that later analysts read.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, binationalist_jewish_dissenters, excluded,
    moderate, generational, identity_locked, regional).

% Britain first, then the United States and the Soviet Union — which recognized the new state within minutes of each other in May 1948. Strategic alignment, intelligence cooperation, and regional leverage flow to them; they bear almost none of the arrangement's local costs and can withdraw or condition support at will. Their recognition converted the program's facts into internationally acknowledged sovereignty.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, great_power_patrons, beneficiary,
    institutional, generational, arbitrage, global).

% Scholars working from congress protocols, cabinet minutes, commission testimony, and captured archives. They occupy no seat in the arrangement, collect nothing from it, and can compare what each party professed publicly with what it recorded privately — the New Historians' archival work on 1948 being the decisive instance. Their seat is where the theater ratio becomes measurable.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, historians_of_nationalism, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of a dispersed, persecuted minority that lacks territorial concentration: it pools migration, capital, and institution-building toward sovereign self-defense capacity, converting millions of individually vulnerable households into one political agent capable of guaranteeing its own admission, refuge, and physical security.
% TRANSFER_FUNCTION: Moves land, sovereignty, and demographic composition in Palestine from Arab-majority control to Jewish national institutions; moves diaspora wealth and great-power sponsorship into the state-building apparatus; and, in the terminal phase, moves people — several hundred thousand Arabs out of the territory and into exile, and hundreds of thousands of Jews in.
% ABSENT_VOICES: Palestinian Arabs were absent from every forum where the arrangement was designed: not consulted at Basel, not consulted before the Balfour Declaration, their 1919 petition to the Paris Peace Conference (documented by the King-Crane Commission) shelved unread. Inside the Jewish conversation, the binationalist dissenters were present but permanently outvoted. The absence is constitutive rather than incidental: a program whose premise is a Jewish demographic majority presupposes the demographic subordination of the people it does not ask.
% DISAPPEARANCE_RATIONALE: If the sovereignty-and-majority arrangement vanished overnight, the rearrangement would be total: Israel's borders, citizenship regime, and Law of Return; the security architecture of world Jewry; the status of five million registered refugees; the treaty web of every neighboring state; and the precedent structure of post-imperial national self-determination claims generally. Nothing in the region's current configuration survives the removal.
% FOUNDING_PROBLEM: European antisemitism against a stateless minority: a people distributed across hostile nation-states, excluded from emancipation's promises, exposed to pogroms, and ultimately to genocide — the problem was how Jewish life could be made physically secure, and the political Zionist answer was that only concentrated territorial sovereignty could do it.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration for the founding problem's original reality comes from outside the benefiting parties: the British Peel Commission (1937) attested the force of the Jewish national claim while documenting Arab opposition; the American King-Crane Commission (1919) attested — and was ignored on — the Arab objection; the postwar Allied consensus treated Jewish statelessness as a demonstrated catastrophe. No comparable external source attests that the original European problem remains the operative justification today: the maintaining institutions assert its liveness themselves, while the refugee parties and much of the historiography attest that the arrangement now persists on the strength of its accomplished facts. The corroboration asymmetry is itself the signal that the status is contested.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__political_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__political_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.84 at interval end) because the end-state arrangement rests on completed dispossession: expropriated refugee property administered in perpetuity, return denied by force, and the remaining Arab minority placed under military government for nearly two decades. Suppression (0.80) reflects the enforcement profile: the demographic fact is maintained by an army, fire applied to returning refugees, absentee-property law, and emergency regulations — the arrangement cannot persist passively. Theater (0.38) is moderate: the underlying functions (immigration, defense, state-building) are real and functional, but the legitimating layer thickens with international scrutiny — the 1949 civic-equality proclamation sitting beside military rule, the 'war-caused' refugee framing beside documented expulsions, public moderation before commissions beside private maximalism in congress protocols. Accessibility collapse (0.62): once the sovereignty-plus-majority path locked in, alternatives (binational polity, cultural-center-only, return-inclusive partition) narrowed sharply but never fully vanished — the dissent tradition survived inside the movement and the refugee claim survived outside it. Resistance (0.70): the 1936-39 Arab Revolt, the 1948 war, sustained refugee politics, and internal Jewish opposition all met the program with real force; attempted Palestinian coalition power was repeatedly broken against imperial backing, which is why high resistance failed to arrest the trajectory. The measurement series runs on one shared eight-point grid (1897-1949) with all three metrics authored at every point; the trajectories are broadly monotonic with a 1939 modulation (the White Paper throttled the demographic engine and briefly lowered both suppression and theater) driven by external imperial policy, not by intermittent reinforcement — the oscillation is exogenous, not an extraction cycle. Identity-lock operates on two seats: the leadership's professional and biographical identity fused with the program's success (exit unthinkable from inside), and the dissenters' Zionist identity kept them inside a movement they had lost in every vote. Suppression here is overwhelmingly structural (military, legal, proprietary); a minor internalized component persists in the dissenters' inability to leave the movement they opposed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from identical facts. From the refugee seat, the structure is rescue: the one door that opened when every other door closed, worth any price paid by others because the alternative was annihilation. From the settler seat, it is national liberation built with sunk lives and capital. From the Palestinian community and diaspora seats, the same structure is dispossession enforced at gunpoint, with exit meaning permanent exile. From the leadership seat, it is a hard-won sovereign remedy to a European catastrophe. The engine derives these divergent classifications from the declared directionalities and exit options; the divergence is the finding, not noise to be reconciled. Trapped exit on the target seats amplifies their effective extraction; arbitrage exit on the patron and mandate seats damps theirs; the refugees' trapped position places even the beneficiary seat in structural dependence on the arrangement's continuation.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real collection points: the settler community received land, housing, and citizenship; the refugees received rescue and nationality; the leadership collected legitimacy, dues, and diplomatic capital; the patrons collected strategic alignment at negligible local cost. The victim declarations map to the displacement ledger: pre-migration Arab communities lost tenancies, wages, homes, and political standing; the diaspora carries the unrecovered property and the denied return across generations. Palestinian targets are identity-bound to place and legally barred from return — full-target directionality with zero arbitrage. The mandate authority is genuinely dual-positioned: it converted the aspiration into binding law and collected a strategic foothold near Suez, but it also paid in soldiers and treasure suppressing the revolt it had helped provoke, and it ultimately exercised the one clean exit in the story by walking away in 1948 — its net directionality sits mid-low, below a pure beneficiary, above a target. Neighboring states bear real costs (military defeat, permanent refugee absorption) with partial sovereign mobility, placing them mid-high rather than full-target. The binationalist dissenters are authored as excluded, not as a correction-grade input: their marginalization is evidence about the conversation's composition, not a lever on classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — European antisemitism against a stateless minority, culminating in genocide — was catastrophically real, and the remedy worked on its own terms: sovereignty was achieved and refuge was delivered. The mandatrophy question is therefore not whether the founding problem was real (it was) but whether the arrangement's persistence is still warranted by that problem or by the new fact it created. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the arrangement is emphatically load-bearing — borders, citizenship regimes, a diaspora's entire security architecture, and a regional alliance system depend on it — so this is not a zombie flag; the function has not atrophied. The classification's job here is to prevent symmetric mislabeling: calling the structure pure extraction erases the rescue function that was real and urgent; calling it pure coordination erases the Nakba that was equally real and unpaid. Tangled rope holds both truths in one structure: genuine coordination for a persecuted people, asymmetric and enforced payment by another. The contested founding-problem status marks where the genealogy is heading: as the European catastrophe recedes biographically, the arrangement's warrant migrates from remedy to fait accompli, and the omega on founding-problem liveness tracks that migration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (political_zionism_reading) of the contested kernel jewish_territorial_claim; the disagreement among readings is located in two structural elements: whether territorial sovereignty with a Jewish demographic majority is NECESSARY to solve the Jewish Question, and what mechanism may legitimately produce that majority.',
    'Cross-reading comparison across the four sibling stories: if the sibling files show materially different victim sets and epsilon values over the same territory and period, the kernel decomposition is confirmed and this file''s values must be read as reading-indexed, not topic-level.',
    'If another reading is adopted as the operative constraint, the victim set shrinks or shifts (cultural reading: no majority premise, displacement not entailed) or expands (revisionist reading: maximal territory, compulsion foregrounded), changing effective extraction for every seated agent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of the sovereignty-and-majority constraint within the decomposed territorial-claim kernel.').

omega_variable(
    sibling_reading_structural_delta,
    'What exactly would each sibling reading change structurally if adopted in place of this one?',
    'Structural diff against sibling stories: cultural_zionism_reading drops the majority precondition (victim set loses the displaced; enforcement machinery largely unnecessary); labor_zionism_reading keeps the demographic aim but routes it through economic facts-on-ground (same victim structure, slower tempo, lower measured suppression at any midpoint); revisionist_zionism_reading enlarges the territorial claim and makes compulsion explicit and immediate (expanded target set, higher epsilon).',
    'Averaging epsilon across readings would fabricate a constraint none of the parties holds; each reading must classify independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Per-sibling structural deltas: victim set, mechanism, and tempo differences.').

omega_variable(
    transfer_contingency_question,
    'Was Arab displacement a programmatic entailment of the majority premise, or a contingent escalation of the 1947-49 war that the premise made likely but did not require?',
    'Archival record: Herzl''s private writings on removal, the Peel Commission''s 1937 transfer proposal and its reception, Weizmann and Ben-Gurion statements on transfer, Plan Dalet''s provisions, and the differential fate of villages that signed non-belligerence agreements.',
    'If programmatic, the high end-state extractiveness attaches to the premise itself and the reading classifies as extraction-bearing at its core; if contingent, part of the measured extraction attaches to wartime execution rather than the sovereignty-and-majority structure, lowering the premise-attributed share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_contingency_question, empirical, 'Attribution of displacement between the majority premise and wartime contingency.').

omega_variable(
    coordination_extraction_separability,
    'Could the rescue function (collective physical security for a persecuted minority) have been delivered without the demographic-displacement mechanism — through earlier mass admission elsewhere, a binational polity, or a partition without transfer?',
    'Counterfactual analysis anchored in documented branch points: Evian Conference outcomes, the 1937 partition boundaries versus executed 1949 lines, Brit Shalom proposals'' rejection margins in congress votes.',
    'If separable, the excess extraction above the coordination floor is attributable to the chosen mechanism and the constraint sits nearer the pure-extraction pole; if inseparable, part of the measured extraction is the price of the coordination itself and the tangled-rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the rescue function and the displacement mechanism were structurally separable.').

omega_variable(
    founding_problem_liveness,
    'Does the antisemitism-remedy justification remain the operative reason the arrangement is maintained, or has it become post-hoc legitimation for a completed demographic and territorial fact?',
    'Track which justifications carry legislative and budgetary weight in the maintaining institutions versus which appear only in external advocacy; measure whether security policy tracks contemporary antisemitic threat data or inherited 1948 threat maps.',
    'If post-hoc, the arrangement''s persistence increasingly rides inertia and enforcement rather than its founding warrant, shifting lifecycle diagnosis toward degraded-function categories over time; if live, the coordination function remains primary and the tangled-rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, preference, 'Liveness of the founding warrant versus post-hoc legitimation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 1949).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(jewi_tr_t1907, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1907, 0.13).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.21).
narrative_ontology:measurement(jewi_tr_t1925, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1925, 0.25).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1936, 0.31).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1939, 0.28).
narrative_ontology:measurement(jewi_tr_t1945, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1945, 0.33).
narrative_ontology:measurement(jewi_tr_t1949, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1949, 0.38).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.12).
narrative_ontology:measurement(jewi_be_t1907, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1907, 0.2).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.34).
narrative_ontology:measurement(jewi_be_t1925, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1925, 0.46).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1936, 0.6).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1939, 0.62).
narrative_ontology:measurement(jewi_be_t1945, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(jewi_be_t1949, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1949, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1897, 0.05).
narrative_ontology:measurement(jewi_su_t1907, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1907, 0.14).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1917, 0.26).
narrative_ontology:measurement(jewi_su_t1925, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1925, 0.36).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1936, 0.56).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1939, 0.52).
narrative_ontology:measurement(jewi_su_t1945, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1945, 0.64).
narrative_ontology:measurement(jewi_su_t1949, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1949, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Zionism' conflates four structurally distinct claims over one kernel (jewish_territorial_claim). Per the epsilon-invariance principle, each reading is a separate story with its own epsilon, victim set, and classification: this file (political_zionism_reading) carries the sovereignty-plus-majority premise and hence the largest direct victim set among the non-revisionist readings; cultural_zionism_reading drops the majority precondition (displacement not entailed, epsilon far lower); labor_zionism_reading shares the demographic aim but routes it through economic transformation (same victim structure, different tempo and suppression profile); revisionist_zionism_reading enlarges the territorial claim and makes compulsion explicit (expanded target set, highest epsilon). This reading is upstream of labor_zionism_reading (its great-power charters created the legal umbrella under which settlement proceeded) and stands in mutual structural pressure with revisionist_zionism_reading (whose maximalism defined itself against this reading's gradualism). Cross-reading epsilon averaging is invalid; the family exists precisely to keep the readings separate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
