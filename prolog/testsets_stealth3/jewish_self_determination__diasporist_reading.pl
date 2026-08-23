% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Settlement of Jewish Collective Fate (Atrophied Regime)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story models the diasporist reading of the kernel
 *   jewish_self_determination: the claim that Jewish collective survival and
 *   flourishing are best secured through diaspora pluralism and minority
 *   rights rather than territorial sovereignty, and that Zionism binds Jewish
 *   fate to a militarized state dangerously. Per the epsilon-referent rule
 *   for kernel readings, the measured referent is the standing arrangement
 *   this story is about - the diasporist normative regime as it actually
 *   stands today, not the revived diaspora-autonomy order the reading
 *   endorses. That standing regime is an inheritance: a coordination
 *   framework that organized dispersed Jewish life (autonomist politics,
 *   Yiddish cultural infrastructure, minority-rights advocacy) until the twin
 *   catastrophes of European destruction and sovereign success destroyed its
 *   institutional base, leaving a residue maintained largely by academic
 *   performance, commemoration, and small-circle identity work. The reading
 *   prices its own arrangement honestly: it still delivers legitimation to
 *   communities living permanently outside Israel, and it still names real
 *   costs borne by Jews bound to or endangered by the Israeli state's actions
 *   - but it no longer protects anyone, and its persistence is mostly
 *   inertia. KEY AGENTS (by structural relationship):
 *   diaspora_jewish_communities - primary residual beneficiary
 *   (moderate/constrained), legitimated in permanence-in-dispersal;
 *   diasporist_intelligentsia - administering cadre
 *   (moderate/identity_locked), maintains the residue theatrically;
 *   zion_identified_diaspora_jews - fate-bound majority payer
 *   (organized/identity_locked); jews_endangered_by_association - unprotected
 *   exposure payer (powerless/trapped); anti_zionist_diaspora_jews -
 *   holding-cost payer (moderate/constrained); israeli_state - anchor of the
 *   rival settlement (institutional/arbitrage), benefits from the
 *   alternative's dormancy; zionist_diaspora_institutions - rival's diaspora
 *   apparatus (institutional/constrained), collects relevance and absorbs
 *   backlash; host_state_governments - holders of the enabling condition
 *   (institutional/arbitrage); assimilated_exits - refusers who priced out
 *   both frameworks (moderate/mobile); palestinian_representatives - absent
 *   voice (organized/trapped); nationalism_scholars - analytical observers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.52).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.62).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Settlement of Jewish Collective Fate (Atrophied Regime)").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political philosophy/nationalism studies/postcolonial theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '8881099e-44bc-4a9f-b165-12427444555d').
narrative_ontology:cs_kernel_codification('8881099e-44bc-4a9f-b165-12427444555d', distributed).
narrative_ontology:cs_authority_grounding('8881099e-44bc-4a9f-b165-12427444555d', distributed).
narrative_ontology:cs_reading_relation('8881099e-44bc-4a9f-b165-12427444555d', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8881099e-44bc-4a9f-b165-12427444555d', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('8881099e-44bc-4a9f-b165-12427444555d', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('8881099e-44bc-4a9f-b165-12427444555d', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('8881099e-44bc-4a9f-b165-12427444555d', foundational, diaspora_pluralism_best_secures_survival).
narrative_ontology:cs_axiom_status(diaspora_pluralism_best_secures_survival, holdable).
narrative_ontology:cs_axiom_grounding('8881099e-44bc-4a9f-b165-12427444555d', diaspora_pluralism_best_secures_survival, empirically_contingent).
narrative_ontology:cs_axiom('8881099e-44bc-4a9f-b165-12427444555d', foundational, territorial_sovereignty_endangers_collective_fate).
narrative_ontology:cs_axiom_status(territorial_sovereignty_endangers_collective_fate, holdable).
narrative_ontology:cs_axiom_grounding('8881099e-44bc-4a9f-b165-12427444555d', territorial_sovereignty_endangers_collective_fate, empirically_contingent).
narrative_ontology:cs_axiom('8881099e-44bc-4a9f-b165-12427444555d', secondary, diasporic_minority_life_is_complete_jewish_life).
narrative_ontology:cs_axiom_status(diasporic_minority_life_is_complete_jewish_life, holdable).
narrative_ontology:cs_axiom_grounding('8881099e-44bc-4a9f-b165-12427444555d', diasporic_minority_life_is_complete_jewish_life, deontological).
narrative_ontology:cs_reference_frame('8881099e-44bc-4a9f-b165-12427444555d', diaspora_autonomy_normalcy).
narrative_ontology:cs_drift_state('8881099e-44bc-4a9f-b165-12427444555d', post_1948_sovereignty_settlement, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8881099e-44bc-4a9f-b165-12427444555d', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, host_state_governments).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, zion_identified_diaspora_jews).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, anti_zionist_diaspora_jews).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_association).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, zion_identified_diaspora_jews).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, israeli_state).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, zionist_diaspora_institutions).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, zionist_diaspora_institutions).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, diaspora_pluralism_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, minority_rights_treaty_regime).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, national_cultural_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live permanently outside Israel across North America, Europe, Latin America, and elsewhere; run schools, charities, cultural bodies, and mutual-aid networks premised on staying. Draw legitimation for their mode of life from the pluralist framework - permanence-in-dispersal presented as complete Jewish life rather than waiting room. Depend on host-state goodwill, which they cannot control and cannot collectively relocate away from. Pay vulnerability taxes whenever host climates turn, and watch communal resources migrate toward the rival settlement's priorities.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    moderate, generational, constrained, global).

% Academics, writers, editors, translators, and archivists staffing what remains: journals, conferences, university curricula, commemorative institutions, small Yiddishist circles. Decide what gets taught, remembered, and republished; in principle they could attempt mass revival - building schools, parties, movements - but the cost (career risk, absent funding, communal excommunication, certain failure against entrenched rivals) far exceeds anything they would personally gain, so maintenance takes the cheap form of performance. Their professional and personal identities are fused with the critique; abandoning the vocation would dissolve the self that chose it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diasporist_intelligentsia, agenda_setter,
    moderate, biographical, identity_locked, global).

% The demographic majority of affiliated diaspora Jews, whose Jewish identity is constituted through solidarity with Israel: they donate, lobby, organize missions, and celebrate its holidays alongside their own. They receive belonging, meaning, and a ready-made collective story. They also carry the binding the diasporist critique names: their personal safety and civic standing rise and fall with a foreign state's wars and scandals, and the fusion is braided too deep to undo without unravelling their communal selves. The pluralist framework diagnoses their predicament accurately and offers them no exit they are willing to take.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zion_identified_diaspora_jews, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, zion_identified_diaspora_jews, beneficiary).

% Jews everywhere who incur hostility for the actions of a state they may never have visited and may actively oppose: guarded synagogues, spiked incident counts during Israeli military operations, workplace and campus exposure. They cannot exit the ascribed identity that attracts the danger, and emigration to the state generating the association is for most of them neither desired nor available. The minority-rights protection the pluralist framework promises is, for them, a theoretical shield that is absent exactly when the danger arrives.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_association, payer,
    powerless, biographical, trapped, global).

% Jews who openly hold the diasporist line inside Zionist-dominated communal spaces: they are pushed out of federations, pulpits, day schools, and family tables, accused of handing ammunition to antisemites, and made to choose between silence and schism. The framework gives them vocabulary and fellow-traveler networks but almost no material protection. Fully leaving Jewish identification would end the costs at the price of self-erasure, so most pay indefinitely in reduced standing.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, anti_zionist_diaspora_jews, payer,
    moderate, biographical, constrained, national).

% Anchors the rival settlement and sets the terms on which Jewish collective identity operates worldwide: its Law of Return defines who belongs, its governments claim to speak for all Jews regardless of residence, and its military actions repriced the safety and standing of every diaspora community. It controls recognition, funding channels, and the definitional machinery of Jewish peoplehood, and it tolerates the pluralist residue in niches it does not consider threatening. Every year the alternative stays dormant, its monopoly consolidates; it bears almost none of the costs this regime distributes.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, israeli_state, beneficiary).

% Federations, defense agencies, campus organizations, and synagogue-movement Israel committees: they administer Israel-engagement as their core product, collecting donations, volunteer energy, and institutional relevance from the association. When the association turns toxic - donor fights over occupation, campus crises, staff burnout - they absorb the backlash directly and have no pivoting option, since the association is their reason for being. They outcompeted the pluralist institutions for the same philanthropic dollar and inherited the field.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_diaspora_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, zionist_diaspora_institutions, payer).

% The states hosting the communities. Their minority-rights regimes, hate-crime enforcement, and diplomatic balancing acts are the load-bearing wall under the pluralist promise: extend tolerance and the diaspora bargain holds; retract it and the bargain collapses regardless of what anyone Jewish believes. They receive loyal, productive, non-secessionist minorities and flexible intermediaries in international politics. No diaspora community can vote any of them out, and their tolerance moves with domestic coalitions the communities do not control.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_state_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, host_state_governments, agenda_setter).

% Descendants of the regime's former constituency who priced out both frameworks and dropped collective identification: intermarried, secularized, privately ethnic at most. They would testify that the sovereignty-tied life and the pluralist life alike charged more than they returned, and that the exit door was open the whole time - evidence both camps prefer not to seat. They appear nowhere in the venues where Jewish collective strategy is argued.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, assimilated_exits, excluded,
    moderate, biographical, mobile, national).

% The people most affected by the sovereignty settlement this regime refuses, and the population about whom diasporist writing most often theorizes without consulting. They would object that metropolitan imaginaries aestheticize their dispossession as a backdrop for Jewish identity debates, and that anti-sovereignty sentiment abroad changes nothing on the ground. Locked into the territory's politics, they have no exit comparable to the diaspora's mobility.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, palestinian_representatives, excluded,
    organized, generational, trapped, regional).

% Comparative researchers of nationalism, diaspora strategies, and minority-rights outcomes. They track which dispersal-based survival strategies historically delivered security and which failed, and publish the comparisons that partisans of every reading cite selectively. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, nationalism_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a dispersed minority's collective existence without state sovereignty: shared cultural infrastructure (language, schooling, memory institutions), minority-rights advocacy, and communal autonomy arrangements solve the collective-action problem of remaining a people across many host societies that would otherwise assimilate or expel them piecemeal.
% TRANSFER_FUNCTION: Moves identity labor, cultural tithing, and political loyalty from individual Jews and member households to communal and cultural institutions charged with continuity; in its present atrophied state it also moves attention and moral capital within Jewish politics toward the non-sovereign pole, while shifting security risk onto members, who depend for protection on host-state tolerance they do not control.
% ABSENT_VOICES: Palestinians, who are theorized about but almost never seated in diasporist venues; Mizrahi Jews whose diasporic experience unfolded under Arab-majority rule and whom the European autonomist canon speaks past; the assimilated exits who concluded both frameworks overcharged; and ordinary unaffiliated Jews for whom the entire debate is a stranger's argument. They are absent by language dominance, class filtering, communal gatekeeping, and geography - the conferences happen in English in capital cities, and the people bearing the sharpest costs are rarely in the room.
% DISAPPEARANCE_RATIONALE: Parties genuinely dispute the counterfactual. On one side: nothing rearranges, because the regime coordinates nothing at scale - its disappearance would be noticed by a few thousand academics and activists and by no one else, and the rival settlement would not move a centimeter. On the other: Jewish political imagination loses its only developed non-sovereign pole, narrowing the collective's conceivable futures to Zionism-or-assimilation, removing the existing moral vocabulary for criticizing the sovereignty settlement from inside, and foreclosing any future return to pluralist strategies if host-state tolerance fails again. Both stories are internally coherent; the parties disagree about which world they live in.
% FOUNDING_PROBLEM: How can a stateless minority scattered across hostile host nations survive persecution, preserve collective life, and flourish - the condition of European Jewry before 1948, to which the era answered with national-cultural autonomy, minority-rights treaty politics, and diasporic institution-building rather than sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Genuinely disputed, with attestation on both sides from outside the benefiting parties. Corroborating the problem's persistence: host-country antisemitism monitoring bodies and security-data series showing recurring incident spikes; historians of minority-rights regimes documenting the interwar guarantees' collapse; and diaspora-community surveys reporting persistent safety anxiety. Corroborating obsolescence: Zionist institutions and mainstream communal leadership attest the founding problem was solved by sovereign refuge in 1948 and again by subsequent rescue operations; several host-state policy archives record minority-protection frameworks that functioned adequately for decades. No single corroborator outside the benefiting parties settles the dispute; the disagreement is itself the documented finding.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52): the standing regime still confers real goods - identity legitimation, moral vocabulary, communal continuity for the non-sovereign majority of world Jewry - while diffusely taxing its holders and failing its promised protections. Suppression (0.62) is the sum of forces closing exit: the rival settlement's communal discipline, funder gatekeeping, conditional host-state tolerance, and internalized vocational identity; note this scalar is deliberately NOT the same quantity as the suppression_requirement series below, which isolates the regime's OWN enforcement machinery and traces its decay from kehillah-grade enforcement (0.55 in 1900) to essentially zero (0.06 in 2025) - enforcement decay is the signature dynamic of this story. Accessibility collapse is partial (0.42): unlike a natural law, the regime does not close alternatives hard - assimilation and quiet privatization are demonstrated exits, and conversion to the rival identity is open - which is why people demonstrably leave. Resistance (0.60) reflects sustained resistance to the regime from the hegemonic rival and continuous attrition from within. Theater_ratio (0.58) crosses the substitution threshold: most current activity is conference-and-commemoration performance consumed by the already convinced, with genuine transmission surviving in a minority of programs. The three series share one eight-point grid (1900-2025) so no metric is sampled against another metric's endpoint. Base_extractiveness spikes at 1948 - the moment the regime's promise went bankrupt while its remaining adherents paid full identity cost - then decays slowly as the residue shrinks and dissent-vocabulary regains marginal utility. Coalition check: the three payer classes do not coalesce - the endangered want security, the anti-Zionists want dissent legitimacy, the Zion-fused do not experience their binding as harm - so no payer coalition forms to revive or bury the regime.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the jews_endangered_by_association seat, the regime is a broken promise: a theoretical shield absent exactly when the danger arrives, so effective extraction runs high. From the diaspora_jewish_communities seat, the regime subsidizes: it is why their mode of life needs no apology, so extraction reads low or negative. From the diasporist_intelligentsia seat, the regime is a vocation: costs are tuition, not extraction. From the israeli_state seat, the regime is nearly invisible - neither payer nor collector, merely terrain. The engine computes these divergent classifications from the structural data; the authored piton claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit at the low-d end: diaspora_jewish_communities draw legitimation and continuity from the regime's operation, and host_state_governments draw stable, non-secessionist minorities from the minority-rights order it advocates. Declared victims sit at the high-d end: zion_identified_diaspora_jews bear the binding of personal fate to a foreign state's wars (partly offset by the belonging the fusion delivers, which their secondary beneficiary role records), jews_endangered_by_association bear security costs the regime no longer absorbs, and anti_zionist_diaspora_jews bear holding costs - communal exclusion - for maintaining a position the regime can no longer defend. The israeli_state and zionist_diaspora_institutions derive neither subsidy nor taxation from this regime directly; their advantage comes from its dormancy, which places them near the symmetric middle for THIS constraint despite their dominant position in the field. No directionality overrides were needed: the derivation chain from beneficiary/victim declarations plus exit options reproduces these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton classification does two jobs at once. Against the Zionist dismissal - 'an irrelevant remnant, therefore nothing to analyze' - it insists that the regime still taxes real people: Jews bound to a militarized state's fate, Jews endangered by association, Jews paying communal rent for heterodoxy, all paying into a structure that no longer delivers protection. Against the diasporist romanticization - 'a living tradition, therefore a functioning one' - it insists that the coordinating function is gone: no schools at scale, no parties, no enforcement, no protection, mostly performance. The classification resolves the mandatrophy question by locating it: the founding problem (stateless-minority survival) is disputed as to status, but the founding MECHANISM (interwar autonomism under minority-rights treaties) is historically superseded, and what persists does so by inheritance and identity rather than function. Naming the administering cadre (diasporist_intelligentsia), the diffuse payers, the prohibitive cost of revival relative to any benefit the administrators would capture, and the absence of any seat that captures the regime's gains pins the classification structurally rather than rhetorically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates only the diasporist reading of the kernel jewish_self_determination; what structurally different constraints do the sibling readings (liberal_nationalist_reading, indigenous_return_reading, settler_colonial_reading, religious_covenant_reading) instantiate, and where exactly do the readings disagree?',
    'Generate the four sibling stories as separate files and compare victim sets, epsilon referents, and computed types; the disagreement is located in whether territorial sovereignty is constitutive of Jewish collective life.',
    'Each sibling relocates the extraction: the settler_colonial reading locates it in Palestinian dispossession (snare-shaped); the liberal_nationalist reading locates legitimacy in an equal-nations settlement (coordination-plus-costs shaped). The epsilon referent of THIS story - the atrophied diasporist regime as lived by its holders - exists only under this reading; averaging across readings would destroy every epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame routing: one reading of a five-reading kernel; sibling readings are other constraints, not parts of this one.').

omega_variable(
    atrophy_attribution,
    'Was the collapse of the diasporist regime''s functioning institutions primarily internally driven (assimilation gravity, ideological exhaustion) or externally imposed (the destruction of European Jewish civil society, Soviet closure of Yiddish institutions, Zionist institutional capture of diaspora communal structures)?',
    'Comparative institutional histories of matched communities: the Bund''s trajectory in interwar Poland versus New York; Soviet Yiddish schools (forcibly closed) versus American Yiddish schools (voluntarily abandoned); funding-flow records of federation reallocation after 1948.',
    'External attribution supports reading the regime as a suppressed alternative killed by a rival settlement; internal attribution supports reading it as self-exhausted. The first reading raises measured suppression and implicates the rival settlement''s enforcement; the second hardens the inertial characterization and lowers the odds any revival program could work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_attribution, empirical, 'Whether the atrophy was murdered or died of natural causes.').

omega_variable(
    counterfactual_protection_capacity,
    'Could a functioning minority-rights diasporist regime have secured European Jewry''s survival through 1939-1945 in the absence of sovereign refuge - the strongest Zionist argument against the entire reading?',
    'Counterfactual historiography: refugee-admission capacities of Western democracies in the 1930s, the actual behavior of interwar minority-rights guarantees under stress, and evacuation logistics compared against what sovereignty actually delivered during the same window.',
    'If the answer is no, the regime''s founding claim is empirically broken and its persistence is pure inheritance-performance (hardening the inertial verdict and raising effective extraction on remaining holders); if partially yes, the regime retains a residual protective function and the moderate extraction estimate stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_protection_capacity, empirical, 'The counterfactual that decides whether the regime''s promise was ever deliverable.').

omega_variable(
    host_tolerance_trajectory,
    'Is host-state minority tolerance - the load-bearing wall under the pluralist promise - structurally stabilizing, or cyclical (long accretion, periodic purge) as a feature of nation-state formation?',
    'Longitudinal comparison of minority-security indices, antisemitism incident series, and citizenship-regime reversals across host states over the full interval, tested for cyclical structure rather than trend.',
    'A confirmed purge-cycle makes the regime''s core promise structurally unsound (raising effective extraction on everyone depending on it); stable tolerance keeps extraction moderate and the regime''s residual benefit genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(host_tolerance_trajectory, empirical, 'Whether the floor under the diasporist bargain is solid or removable.').

omega_variable(
    suppression_mechanism_split,
    'Of the suppression experienced around this regime, how much is structural (communal gatekeeping, funder pressure, hostile rival institutions, conditional host-state tolerance) and how much is internalized (holders'' vocational identity fusion, fear of betraying ancestors, inability to imagine a self outside the vocation)?',
    'Post-exit suppression trajectory: track holders who privately abandon the diasporist line or leave Jewish collective life entirely; if the sense of constraint persists after all external barriers are removed, a large share is internalized.',
    'If internalized share is high, effective suppression exceeds the structural measure - exit is cheaper than it feels and the regime holds people who no longer need to be held; if structural share is high, the rival settlement''s enforcement is doing the work and the regime is more captive than inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized composition of the measured suppression.').

omega_variable(
    theater_function_boundary,
    'How much of the regime''s current activity is irreducibly valuable cultural transmission (language acquisition, archival preservation, cross-generational continuity) versus performative maintenance (conferences, commemoration cycles, discourse production consumed by the already converted)?',
    'Outcome audits of diaspora cultural programs: enrollment retention, transmission into third-generation practice, and whether outputs reach anyone outside the performer class.',
    'A higher functional share would push the theater ratio down and reopen the possibility that the regime is a dormant-but-real coordination asset rather than inherited performance; a higher performative share confirms the inertial verdict and supports treating maintenance spending as pure sunk cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_function_boundary, empirical, 'Where the line between surviving function and performed memory actually sits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_diasporist_tr_t1900, jewish_self_determination__diasporist_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t1900, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t1920, jewish_self_determination__diasporist_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t1920, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t1939, jewish_self_determination__diasporist_reading, theater_ratio, 1939, 0.25).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t1939, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.55).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t1948, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.6).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t1967, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t1980, jewish_self_determination__diasporist_reading, theater_ratio, 1980, 0.62).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t1980, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t2000, jewish_self_determination__diasporist_reading, theater_ratio, 2000, 0.58).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t2000, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t2025, jewish_self_determination__diasporist_reading, theater_ratio, 2025, 0.58).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(jsd_diasporist_be_t1900, jewish_self_determination__diasporist_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement_basis(jsd_diasporist_be_t1900, observed).
narrative_ontology:measurement(jsd_diasporist_be_t1920, jewish_self_determination__diasporist_reading, base_extractiveness, 1920, 0.32).
narrative_ontology:measurement_basis(jsd_diasporist_be_t1920, observed).
narrative_ontology:measurement(jsd_diasporist_be_t1939, jewish_self_determination__diasporist_reading, base_extractiveness, 1939, 0.38).
narrative_ontology:measurement_basis(jsd_diasporist_be_t1939, observed).
narrative_ontology:measurement(jsd_diasporist_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement_basis(jsd_diasporist_be_t1948, observed).
narrative_ontology:measurement(jsd_diasporist_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.66).
narrative_ontology:measurement_basis(jsd_diasporist_be_t1967, observed).
narrative_ontology:measurement(jsd_diasporist_be_t1980, jewish_self_determination__diasporist_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement_basis(jsd_diasporist_be_t1980, observed).
narrative_ontology:measurement(jsd_diasporist_be_t2000, jewish_self_determination__diasporist_reading, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement_basis(jsd_diasporist_be_t2000, observed).
narrative_ontology:measurement(jsd_diasporist_be_t2025, jewish_self_determination__diasporist_reading, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(jsd_diasporist_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(jsd_diasporist_su_t1900, jewish_self_determination__diasporist_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement_basis(jsd_diasporist_su_t1900, observed).
narrative_ontology:measurement(jsd_diasporist_su_t1920, jewish_self_determination__diasporist_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement_basis(jsd_diasporist_su_t1920, observed).
narrative_ontology:measurement(jsd_diasporist_su_t1939, jewish_self_determination__diasporist_reading, suppression_requirement, 1939, 0.35).
narrative_ontology:measurement_basis(jsd_diasporist_su_t1939, observed).
narrative_ontology:measurement(jsd_diasporist_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.2).
narrative_ontology:measurement_basis(jsd_diasporist_su_t1948, observed).
narrative_ontology:measurement(jsd_diasporist_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.12).
narrative_ontology:measurement_basis(jsd_diasporist_su_t1967, observed).
narrative_ontology:measurement(jsd_diasporist_su_t1980, jewish_self_determination__diasporist_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement_basis(jsd_diasporist_su_t1980, observed).
narrative_ontology:measurement(jsd_diasporist_su_t2000, jewish_self_determination__diasporist_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement_basis(jsd_diasporist_su_t2000, observed).
narrative_ontology:measurement(jsd_diasporist_su_t2025, jewish_self_determination__diasporist_reading, suppression_requirement, 2025, 0.06).
narrative_ontology:measurement_basis(jsd_diasporist_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Jewish self-determination' conflates five structurally distinct claims, decomposed per the epsilon-invariance principle into five linked stories sharing the kernel jewish_self_determination. Each reading instantiates a different constraint with a different epsilon referent and victim set: this diasporist reading measures the atrophied diasporist regime as lived by its holders; the liberal_nationalist reading measures the equal-nations sovereignty settlement; the indigenous_return reading measures the return-as-decolonization settlement; the settler_colonial reading measures the dispossession-producing sovereignty project; the religious_covenant reading measures the covenant-obligation settlement. The upstream/downstream gradient runs from the liberal-nationalist settlement (highest empirical consolidation) through the indigenous-return and covenant readings to the diasporist and settler-colonial readings, which contest it; this story links bidirectionally to all four siblings so contamination and foreclosure analysis can traverse the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
