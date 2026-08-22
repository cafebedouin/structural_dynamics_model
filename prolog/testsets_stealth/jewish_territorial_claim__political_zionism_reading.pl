% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionism: Sovereignty-with-Majority as Solution to the Jewish Question
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel
 *   jewish_territorial_claim: the political_zionism_reading, in which Jewish
 *   statehood with a territorial majority is the solution to antisemitism,
 *   the existing Arab population is positioned as the obstacle to that
 *   majority, and population transfer is treated as a necessary mechanism.
 *   Per Rule 1, the contest between readings is NOT described inside this
 *   constraint — the siblings are separate files. Family decomposition and
 *   epsilon differences (required documentation for kinship claims): the
 *   cultural reading authors very low epsilon (a spiritual center requires no
 *   forced demographic change; its victim set is nearly empty), the labor
 *   reading authors mid-range epsilon (settlement displacement without an
 *   explicit transfer doctrine), this political reading authors high epsilon
 *   (displacement is constitutive — the majority cannot be reached otherwise
 *   given the demographic baseline), and the revisionist reading authors the
 *   highest epsilon (maximalist territory plus an open military-compulsion
 *   doctrine). The claim/metric gap is deliberate and independent: the
 *   reading CLAIMS a tangled-rope structure (real coordination function,
 *   asymmetric cost-bearing, active enforcement) and the metrics
 *   independently describe heavily extractive, actively enforced operation —
 *   the engine computes per-seat classifications from the structural data;
 *   nothing here reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - world_zionist_organization: Agenda setter (institutional/mobile) — writes the program, administers immigration and land, negotiates sovereignty; demonstrated mobility in the 1903 Uganda episode
 *   - yishuv_settler_community: Primary beneficiary (organized/identity_locked) — receives sovereignty, land, and refuge; identity-fused with the project it built
 *   - diaspora_jewish_refugee_population: Rescue beneficiary (powerless/trapped) — the population for whom the arrangement is the only exit
 *   - palestinian_arab_residents: Primary target (organized/trapped) — bears dispossession, revolt suppression, and post-1948 disenfranchisement
 *   - palestinian_refugees_denied_return: Hardest-trapped target (powerless/trapped) — their return is precisely what the majority rule forbids
 *   - british_mandate_authority: Inter-institutional dual actor (institutional/arbitrage) — enforces the charter while collecting strategic value, exits cleanly in 1948
 *   - great_power_patron_governments: Inter-institutional beneficiaries (institutional/arbitrage) — conditional support, withdrawable
 *   - binational_state_advocates: Excluded internal voice (moderate/identity_locked) — the suppressed alternative reading
 *   - international_legal_observers: Analytical observer (institutional/analytical) — records without enforcing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.82).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.78).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionism: Sovereignty-with-Majority as Solution to the Jewish Question").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '4a874f1c-083b-4482-8a65-93e890d64c1f').
narrative_ontology:cs_kernel_codification('4a874f1c-083b-4482-8a65-93e890d64c1f', distributed).
narrative_ontology:cs_authority_grounding('4a874f1c-083b-4482-8a65-93e890d64c1f', lineage).
narrative_ontology:cs_interpretation_layer_present('4a874f1c-083b-4482-8a65-93e890d64c1f').
narrative_ontology:cs_reading_relation('4a874f1c-083b-4482-8a65-93e890d64c1f', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('4a874f1c-083b-4482-8a65-93e890d64c1f', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('4a874f1c-083b-4482-8a65-93e890d64c1f', jewish_territorial_claim__revisionist_zionism_reading, influences).
narrative_ontology:cs_axiom('4a874f1c-083b-4482-8a65-93e890d64c1f', foundational, jewish_sovereignty_necessary_for_jewish_safety).
narrative_ontology:cs_axiom_status(jewish_sovereignty_necessary_for_jewish_safety, holdable).
narrative_ontology:cs_axiom_grounding('4a874f1c-083b-4482-8a65-93e890d64c1f', jewish_sovereignty_necessary_for_jewish_safety, instrumental).
narrative_ontology:cs_axiom('4a874f1c-083b-4482-8a65-93e890d64c1f', foundational, jewish_demographic_majority_constitutive_of_remedy).
narrative_ontology:cs_axiom_status(jewish_demographic_majority_constitutive_of_remedy, holdable).
narrative_ontology:cs_axiom_grounding('4a874f1c-083b-4482-8a65-93e890d64c1f', jewish_demographic_majority_constitutive_of_remedy, instrumental).
narrative_ontology:cs_axiom('4a874f1c-083b-4482-8a65-93e890d64c1f', secondary, population_transfer_legitimate_majority_mechanism).
narrative_ontology:cs_axiom_status(population_transfer_legitimate_majority_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('4a874f1c-083b-4482-8a65-93e890d64c1f', population_transfer_legitimate_majority_mechanism, instrumental).
narrative_ontology:cs_reference_frame('4a874f1c-083b-4482-8a65-93e890d64c1f', herzlian_chartered_majority_state).
narrative_ontology:cs_drift_state('4a874f1c-083b-4482-8a65-93e890d64c1f', contemporary_post_sovereignty, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4a874f1c-083b-4482-8a65-93e890d64c1f', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, world_zionist_organization).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, yishuv_settler_community).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, diaspora_jewish_refugee_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_refugees_denied_return).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, british_mandate_authority).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, great_power_patron_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convened annual congresses that set the program: a publicly guaranteed home culminating in statehood with a Jewish majority. Raised funds across diaspora communities, purchased land through its national institutions, negotiated with Ottoman, British, and great-power governments, and organized immigration. Briefly entertained a rival territory in the 1903 Uganda vote before reaffirming Palestine; after 1948 its core functions passed into the state it had brought into being.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, world_zionist_organization, agenda_setter,
    institutional, generational, mobile, global).

% Held the League of Nations charter incorporating the Balfour Declaration, administered immigration schedules and land transfers, suppressed the 1936-39 Arab revolt, cut immigration at the war's height under the 1939 White Paper, and finally referred the dispute to the United Nations and withdrew in 1948. Throughout, it collected strategic value from the position: the land route to the east and the eastern Mediterranean anchor.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandate_authority, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, british_mandate_authority, beneficiary).

% Built a parallel society — elected assembly, labor federation, militia, Hebrew university — absorbed successive immigration waves, assembled purchased land into contiguous blocks, and in 1948 declared and defended statehood. Members' language revival, children's schooling, and family futures became bound up with the project; after the destruction of European Jewry, returning to a diaspora life ceased to be a live option for most.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, yishuv_settler_community, beneficiary,
    organized, generational, identity_locked, regional).

% Lived under escalating exclusionary measures across Europe and later across Arab states. After the 1924 American quota cuts and the 1938 Evian conference, almost no state would take them in large numbers. The sovereignty project promised the admission channel they otherwise lacked; after 1948 the Law of Return converted that promise into an open door, and survivors from the camps, then communities expelled or pressured out of Arab states, entered in the hundreds of thousands.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, diaspora_jewish_refugee_population, beneficiary,
    powerless, immediate, trapped, global).

% Formed the overwhelming majority of the territory's population when the program was proclaimed in 1897. Experienced tenancy evictions following land sales, boycotted and then rose in revolt against the immigration regime in 1936-39, and in the 1948 war saw roughly 700,000 people flee or be driven from towns that fell inside the new state. Those who remained lived under military government until 1966; those who left were barred from coming back.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_residents, payer,
    organized, generational, trapped, regional).

% Displaced in 1948 and again in 1967, settled in camps in Lebanon, Syria, Jordan, Gaza, and the West Bank under international relief administration. Their return is the one change the majority rule cannot absorb, so successive negotiations have offered compensation and small token quotas instead. Citizenship in the countries they landed in was frequently withheld, keeping the camp, not a new life, as the inherited condition.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_refugees_denied_return, payer,
    powerless, generational, trapped, regional).

% A current inside the movement itself — Brit Shalom, Magnes, Buber, and later assorted one-state proposals — arguing for a shared polity or cultural center without demographic supremacy. Present at the margins of every congress and cabinet debate, consistently outvoted, and after 1948 widely treated as naive or disloyal. They had standing to speak and were never the audience that counted.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, binational_state_advocates, excluded,
    moderate, generational, identity_locked, regional).

% Britain took the strategic corridor; the Soviet Union armed the 1948 state to weaken the British position; the United States extended recognition, aid, and later durable diplomatic cover in exchange for alignment, intelligence cooperation, and regional leverage. Each patron's support ran on its own strategic ledger and remained withdrawable in principle.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, great_power_patron_governments, beneficiary,
    institutional, biographical, arbitrage, global).

% UN mediators (Count Bernadotte, killed in 1948 after proposing refugee return), General Assembly Resolution 194, Security Council resolutions on the territories, International Court of Justice advisory proceedings, and special rapporteurs. They compile the record and issue non-binding findings; they hold no enforcement arm over the structure they measure.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_legal_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__political_zionism_reading, yishuv_settler_community).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__political_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: dispersed, persecutable Jewish populations had no mechanism to guarantee physical safety or national continuity, and minority-rights treaty regimes had repeatedly failed them. Territorial sovereignty concentrates defense, immigration control, and law-making under Jewish command — one structure replacing a patchwork of unreliable host-state protections.
% TRANSFER_FUNCTION: Moves land, legal authority, and demographic composition. Land and sovereign authority move from Arab residents and the Ottoman/Mandate holders to Jewish national institutions; the burden of defending Jewish life moves from nothing (host-state whim) to a dedicated state; and the Arab share of the population moves outward — through flight and expulsion — so that a Jewish majority, the operative ingredient of the remedy, comes to exist.
% ABSENT_VOICES: Palestinian Arabs were structurally absent from every decisive forum: absent from Basel in 1897, from Sykes-Picot and the Balfour Declaration, turned away from effective consultation at Paris in 1919, and presented with partition as a fait accompli in 1947. Inside the Zionist tent, the binational advocates had the standing to speak and were outvoted into irrelevance. Where are they: outside the rooms where the majority requirement was written, and in the camps its operation produced.
% DISAPPEARANCE_RATIONALE: If the sovereignty-with-majority structure vanished overnight, the state's citizenship regime, land registry, Law of Return, and defense doctrine would lose their foundation simultaneously; the refugee-return question would flip from permanently deferred to immediately actionable; regional alliance structures, patron commitments, and the demography of every neighboring state would begin rearranging within months.
% FOUNDING_PROBLEM: The Jewish Question: a stateless minority scattered across states that turned exclusionary, culminating in a continent-scale attempt at extermination. The problem the arrangement was built to solve was the absence of any guaranteed place of Jewish self-defense and self-determination.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem was real and severe is corroborated from outside the benefiting parties: the postwar historical consensus on the failure of minority-protection regimes, the Evian conference record, and Holocaust historiography all attest it. That the problem still warrants the arrangement's present-day costs is disputed by those same outside sources — Palestinian and Arab scholarship, UN treaty bodies, and ICJ proceedings characterize the arrangement's continuing operation as majority-maintenance rather than rescue. No neutral attestation settles the warrant question; the parties themselves divide along the beneficiary/target line.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is high (0.82 at interval end) because the arrangement's operative mechanism — reaching and keeping a Jewish majority — prices the Arab population's presence as the cost object; the reading itself concedes the magnitude by declaring transfer 'necessary,' disputing the cost's justifiability, not its size. Suppression (0.78) is structural: military government over remaining Arab citizens until 1966, denial of refugee return, occupation administration, blockade — the enforcement machinery exists to hold the demographic line. Theater (0.40) is moderate: the state's core functions are real, but negotiation processes that never touch the majority question accumulate as performance layered over static facts (visible in the t=100 Oslo-era theater peak). Accessibility_collapse (0.58): alternatives — binationalism, cultural-center-only, diaspora autonomism — were progressively closed inside the movement's operative discourse and internationally after the 1947 partition endorsement, but never fully extinguished; they survive as marginal positions. Resistance (0.70): the 1936-39 revolt, three interstate wars, two intifadas, and sustained diplomatic isolation campaigns. Time grid: integer years since the 1897 Basel Program, one shared grid for all three tracked metrics; the t=50 spike is the 1947-48 partition-war/Nakba conjuncture, the t=100 dip the Oslo interlude. All measurement points are historical observations.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical structure. From the agenda-setter and beneficiary seats, the arrangement is hard-won self-determination and a functioning rescue channel — the same wall that is a shelter from one side. From the payer seats, the identical structure operates as dispossession and permanent exile — a cage. The diaspora-rescue seat experiences it as lifeline; the patron seats as a cheap, high-yield alignment asset; the observer seat as a documented, unenforceable violation record. The engine derives these divergences from power, exit, and role data; the divergence IS the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the low-d end: the WZO wrote and administered the rules (near-full beneficiary), the yishuv received sovereignty and land (identity lock keeps it committed despite the war costs it bore — costs accepted within a fused identity, not imposed by the constraint), and the refugee population received the exit nowhere else offered. Targets sit at the high-d end, amplified by trapped exit: residents who lost land and citizenship, and refugees whose return is the single change the rule cannot absorb — the hardest-trapped seat in the story, nearest the full-target pole. The British mandate and the patrons hold arbitrage-grade exit, damping their exposure despite their enforcement and funding roles; their d sits low-to-mid, reflecting collected strategic value without demographic stake. No directionality overrides were needed: role plus exit data yields the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real, externally corroborated, and lethal; the rescue function demonstrably operated (camp survivors 1945-55, Mizrahi exodus 1948-67, later Ethiopian, post-Soviet, and Ukrainian admissions). But the arrangement's operational center of gravity has shifted from admitting the persecuted to maintaining the majority — a different function wearing the founding warrant. The status is contested, not dead: antisemitism persists and new rescue episodes recur, so no clean mandatrophy resolution fires. The classification discipline prevents both symmetrical errors: labeling the structure a pure snare erases the genuine coordination achievement (a real collective-action problem, really solved for the beneficiary population); labeling it a pure rope erases extraction that is constitutive rather than incidental — the majority requirement makes the Arab population's removal part of the mechanism, not a side effect. Omega founding_problem_warrant_persistence tracks whether the warrant decays into inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading — political_zionism_reading — of the kernel jewish_territorial_claim. Which reading is instantiated determines the victim set and epsilon; what exactly would each sibling reading change structurally?',
    'Side-by-side compilation of the four sibling story files: the cultural reading drops the majority clause (victim set collapses toward empty, epsilon falls toward coordination-cost floor); the labor reading replaces the transfer mechanism with settlement facts (epsilon mid-range); the revisionist reading extends territory and hardens enforcement (epsilon rises, suppression rises).',
    'Classification is reading-indexed. Aggregating the four readings into one ''Zionism'' constraint would average away the extraction profile specific to the majority requirement and produce a verdict true of no actual party''s commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer indexicality: the constraint''s identity depends on which reading of the territorial-claim kernel is instantiated.').

omega_variable(
    disagreement_location_majority_clause,
    'Where the readings genuinely diverge is the demographic clause: does the remedy require a Jewish majority (making the fate of the existing Arab majority constitutive of the constraint) or only a Jewish center or homeland (leaving the Arab population''s position negotiable rather than structurally doomed)?',
    'Textual and institutional analysis of each reading''s program documents: the Basel Program and Herzl''s Der Judenstaat versus Ahad Ha''am''s essays versus Jabotinsky''s Iron Wall writings, traced through congress votes and institutional practice.',
    'If the majority clause is dropped (the cultural reading''s position), the transfer mechanism loses its warrant, the victim set empties of forced-displacement cases, and the constraint''s type shifts toward rope or scaffold. The entire extraction profile hangs on this clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_majority_clause, conceptual, 'The demographic-majority clause is the load-bearing disagreement separating this reading from its siblings.').

omega_variable(
    transfer_constitutive_vs_contingent,
    'Is population transfer constitutive of the political reading''s logic — unreachable majority making displacement unavoidable given the demographic baseline — or a contingent implementation choice that other versions of the same reading could have avoided?',
    'Counterfactual demographic analysis at the decision points: could higher absorptive immigration, differential birth rates, or economic integration have produced a Jewish majority without displacement? Compare partition-plan demographics against realized 1948 outcomes.',
    'If constitutive, the extraction is intrinsic to the reading and the tangled_rope classification is stable at high epsilon; if contingent, responsibility shifts to particular implementations and the reading could in principle have operated as a transitional scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_constitutive_vs_contingent, empirical, 'Whether displacement was structurally forced by the majority requirement or chosen among alternatives.').

omega_variable(
    founding_problem_warrant_persistence,
    'Does the founding problem — exterminatory statelessness — still warrant the arrangement''s current costs, or has the warrant decayed into inertia while the arrangement now runs on majority-maintenance?',
    'Comparative tracking of contemporary antisemitism and diaspora-safety data against the arrangement''s ongoing cost profile: do current rescue admissions track protection needs, or has admission become marginal to the structure''s operation relative to territorial and demographic management?',
    'If the warrant has decayed, the founding_problem_status flips to dead while disappearance_verdict stays world_rearranges — firing the capture/zombie mismatch flag and pushing the classification toward piton dynamics. If live, the tangled_rope reading stands with its contested warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_warrant_persistence, empirical, 'Persistence of the founding warrant versus inertial majority-maintenance as the arrangement''s operative driver.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__political_zionism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jewi_tr_t25, jewish_territorial_claim__political_zionism_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(jewi_tr_t50, jewish_territorial_claim__political_zionism_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(jewi_tr_t75, jewish_territorial_claim__political_zionism_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement(jewi_tr_t100, jewish_territorial_claim__political_zionism_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(jewi_tr_t125, jewish_territorial_claim__political_zionism_reading, theater_ratio, 125, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jewi_be_t25, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(jewi_be_t50, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 50, 0.9).
narrative_ontology:measurement(jewi_be_t75, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 75, 0.86).
narrative_ontology:measurement(jewi_be_t100, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 100, 0.79).
narrative_ontology:measurement(jewi_be_t125, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 125, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(jewi_su_t25, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(jewi_su_t50, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(jewi_su_t75, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 75, 0.8).
narrative_ontology:measurement(jewi_su_t100, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement(jewi_su_t125, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 125, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Zionism' covers four structurally distinct claims and is decomposed per the epsilon-invariance principle into a four-story constraint family linked by this array. Epsilon ordering across the family: cultural (lowest — no forced demographic change, victim set nearly empty), labor (mid — settlement displacement without explicit transfer doctrine), political (this file, high — displacement constitutive of the majority requirement), revisionist (highest — maximalist territory plus open military-compulsion doctrine). Causal structure: the cultural reading is the upstream critique the political reading answered; the political reading is the diplomatic spine whose charter achievements (Balfour, the Mandate) created the legitimacy conditions both labor settlement and revisionist radicalization operated within; revisionism is the political reading's downstream radicalization. Contamination propagation runs accordingly: degradation of the political reading's legitimacy propagates to all three siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
