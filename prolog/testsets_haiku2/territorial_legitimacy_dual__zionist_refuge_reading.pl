% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Israeli Territorial Legitimacy (Zionist Refuge Reading)
 *   domain: political/international/territorial
 *
 * SUMMARY:
 *   This constraint story instantiates the Zionist refuge reading of the
 *   contested kernel 'territorial_legitimacy_dual'. The kernel is the
 *   contested legitimacy claim that grounds Israeli statehood and territorial
 *   control. The Zionist refuge reading frames Israel's existence and
 *   sovereignty as the solution to historical Jewish persecution, grounded in
 *   UN partition acceptance (1947) and the claim that Palestinian
 *   displacement resulted from Arab rejection of partition rather than from
 *   primary Israeli dispossession. This reading coexists with the Palestinian
 *   autochthony reading (which centers Palestinian continuous habitation and
 *   displacement) and the two-state coexistence reading (which accepts dual
 *   legitimacy and 1967 boundaries as the compromise framework). This story
 *   authors ONLY the Zionist refuge reading as a structurally coherent
 *   constraint with its own ε, beneficiary/victim structure, and narrated
 *   founding problem. The structural delta for this reading: 1948 legitimacy
 *   is presented as uncontested (UN partition + refuge); 1967 boundaries are
 *   presented as negotiable (security adjustments permissible); Palestinian
 *   displacement is framed as consequence of Arab state rejection, not
 *   primary extraction; security concerns justify ongoing territorial control
 *   and settlement policy.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_populations: Primary beneficiary (vindication of persecution narrative) — organized, mobile, generational time horizon
 *   - israeli_state_institutions: Agenda setter and institutional beneficiary (sets legitimacy framework, administers enforcement) — institutional power, arbitrage exit
 *   - palestinian_residents_1948: Primary payer (displaced/dispossessed) — powerless, trapped, biographical horizon
 *   - palestinian_territories_1967_onwards: Secondary payer (occupied populations under security doctrine) — moderate power, constrained exit, generational horizon
 *   - religious_zionist_constituencies: Beneficiary with foundational axiom (divine promise) — moderate power, identity-locked exit, civilizational horizon
 *   - security_establishment: Agenda setter and institutional beneficiary (administers enforcement apparatus, derives justification from security doctrine) — institutional power, arbitrage exit
 *   - arab_state_governments: Excluded from internal logic (their partition rejection is framed as originating cause, not legitimate counter-claim) — powerful but excluded
 *   - international_legal_apparatus: Observer seat (interprets UN resolutions, humanitarian law)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Israeli Territorial Legitimacy (Zionist Refuge Reading)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political/international/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '6eab7523-2c20-4761-bf31-87977c2cb5b0').
narrative_ontology:cs_kernel_codification('6eab7523-2c20-4761-bf31-87977c2cb5b0', fixed_text).
narrative_ontology:cs_authority_grounding('6eab7523-2c20-4761-bf31-87977c2cb5b0', lineage).
narrative_ontology:cs_interpretation_layer_present('6eab7523-2c20-4761-bf31-87977c2cb5b0').
narrative_ontology:cs_reading_relation('6eab7523-2c20-4761-bf31-87977c2cb5b0', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('6eab7523-2c20-4761-bf31-87977c2cb5b0', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('6eab7523-2c20-4761-bf31-87977c2cb5b0', foundational, historical_persecution_justifies_territorial_refuge).
narrative_ontology:cs_axiom_status(historical_persecution_justifies_territorial_refuge, holdable).
narrative_ontology:cs_axiom_grounding('6eab7523-2c20-4761-bf31-87977c2cb5b0', historical_persecution_justifies_territorial_refuge, empirically_contingent).
narrative_ontology:cs_axiom('6eab7523-2c20-4761-bf31-87977c2cb5b0', secondary, security_concerns_permit_territorial_expansion).
narrative_ontology:cs_axiom_status(security_concerns_permit_territorial_expansion, holdable).
narrative_ontology:cs_axiom_grounding('6eab7523-2c20-4761-bf31-87977c2cb5b0', security_concerns_permit_territorial_expansion, instrumental).
narrative_ontology:cs_axiom('6eab7523-2c20-4761-bf31-87977c2cb5b0', foundational, divine_promise_eretz_yisrael).
narrative_ontology:cs_axiom_status(divine_promise_eretz_yisrael, holdable).
narrative_ontology:cs_axiom_grounding('6eab7523-2c20-4761-bf31-87977c2cb5b0', divine_promise_eretz_yisrael, theological).
narrative_ontology:cs_reference_frame('6eab7523-2c20-4761-bf31-87977c2cb5b0', historical_persecution_justifies_refuge).
narrative_ontology:cs_drift_state('6eab7523-2c20-4761-bf31-87977c2cb5b0', contemporary_occupation_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6eab7523-2c20-4761-bf31-87977c2cb5b0', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_populations).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_1948).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_territories_1967_onwards).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, religious_zionist_constituencies).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, security_establishment).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, historical_persecution_justifies_refuge).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_legitimizes_jewish_state).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, security_concerns_justify_territorial_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Understand Israel's existence and territorial sovereignty as providing refuge from historical persecution and validating the Jewish political arc from vulnerability to self-determination. They benefit from the symbolic and material security the state offers, though most diaspora members do not physically inhabit it. Their security narrative is vindicated by the constraint's operation — the existence of a Jewish state proves the refuge-possibility thesis.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_populations, beneficiary,
    organized, generational, mobile, global).

% Set and defend the territorial legitimacy framework grounded in historical persecution, UN partition, and divine promise (for religious constituents). Administer the legal, military, and security apparatus that enforces territorial control and defends borders. Justify expansions and occupations through the security doctrine derived from the legitimacy narrative. Claim authority to interpret and adjust boundaries based on security assessment.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Displaced or dispossessed by Israel's founding and territorial consolidation, framed within this reading as collateral to the refugee-refuge arrangement rather than as primary targets. They bear concentrated costs: property loss, displacement to refugee camps or diaspora, loss of political voice in the territory they inhabited. Within this reading's internal logic they are not victims of primary extraction but consequences of Arab state rejection of partition; they have no legitimate claim to return within this frame.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_1948, payer,
    powerless, biographical, trapped, local).

% West Bank and Gaza populations under occupation and varying degrees of Israeli control, justified within this reading as necessary security measures to prevent attacks and ensure Israeli survival. They bear restrictions on movement, settlement rights, resource access, and political autonomy, framed as proportional to the threat level. Their constrained exit reflects occupation architecture (checkpoints, blockade, administrative barriers).
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_territories_1967_onwards, payer,
    moderate, generational, constrained, regional).

% For whom the divine promise (Eretz Yisrael biblical boundaries) is a foundational legitimacy claim, superseding or supplementing the UN partition framework. They vindicate the constraint by inhabiting and settling territory claimed as divinely promised. Their identity is constitutively fused with territorial claims that extend beyond 1967 boundaries; exit from the constraint would require abandoning religious identity itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, religious_zionist_constituencies, beneficiary,
    moderate, civilizational, identity_locked, global).

% Operates the enforcement apparatus (military, intelligence, border control, occupation administration, settlement guard) that maintains territorial control. Derives institutional power, budgetary justification, and operational logic from the security doctrine grounded in the legitimacy narrative. Threat assessment, prevention doctrine, and territorial buffer logic all rest on this reading's foundational claims; the establishment has institutional incentive to maintain the constraint.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, security_establishment, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, security_establishment, beneficiary).

% Would articulate an alternative territorial legitimacy framework (Palestinian autochthony, Arab right to the territory, Palestinian right of return) but are excluded from the internal logic of this reading, which treats their rejection of 1947 partition as the origin event justifying Israeli territorial expansion and Palestinian displacement. They remain excluded from the consensus even after formal peace agreements.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, arab_state_governments, excluded,
    powerful, generational, constrained, regional).

% Interprets UN partition (181), successive UN Security Council resolutions (242, 338, and others), international humanitarian law, and occupation status. From this reading's seat, UN 181 legitimizes partition; UN 242 is read as permitting security-based territorial adjustment rather than mandating 1967-boundary restoration. The apparatus observes all parties but is contested as to interpretive authority.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_legal_apparatus, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a territorial refuge for Jewish populations historically subject to persecution, diaspora statelessness, and genocidal violence. Coordinates diaspora identity, religious practice (for observant Jews), and political sovereignty around a singular state apparatus. Solves the 19th-20th century 'Jewish Question' by establishing a state where Jewish majority rule is guaranteed and Jewish citizens can exercise collective self-determination without depending on the tolerance of other states.
% TRANSFER_FUNCTION: Transfers territorial control, property rights, political voice, and freedom of movement from Palestinian residents (1948 displacement) and Palestinian territories (1967 occupation onward) to Israeli state institutions, Jewish settler populations, and the diaspora (via the vindication of refuge-as-solution). Justifies this transfer as the necessary cost of preventing hostile takeover and ensuring the refuge's stability and security.
% ABSENT_VOICES: Palestinian national movements would articulate an autochthony-based legitimacy grounding in continuous habitation and displacement trauma; Arab governments would assert that partition was unjust and Palestinian rejection was justified self-defense; international humanitarian law interpreters who read occupation as violating UN 242 and proportionality principles are marginalized; Jewish voices questioning whether statehood actually solves persecution vulnerability (alternative: diaspora integration in liberal democracies) are largely excluded from Israeli institutional framing.
% DISAPPEARANCE_RATIONALE: If this territorial legitimacy framework disappeared — if Israel's grounding in historical persecution and partition were repudiated as the legitimate basis for statehood and control — the state would face existential delegitimization. Palestinian claims to return and territorial restoration would become the default international framing. Settlement architecture would be exposed as occupation without legitimacy cover. The security doctrine would lose its foundational justification and would have to be re-grounded in raw power (untenable diplomatically). Regional power dynamics would shift from Israeli legitimacy-defended hegemony to contested occupation under international law. The refuge-narrative vindication would collapse.
% FOUNDING_PROBLEM: Jewish populations faced systematic persecution, mass expulsion, pogroms, and genocidal violence across multiple centuries and continents (Inquisition, Russian pogroms, Holocaust). Diaspora existence was structurally vulnerable to state violence and dependent on the tolerance of non-Jewish majorities. The 'Jewish Question' of 19th-century political theory demanded a territorial solution where Jews could exercise sovereign self-determination, defend themselves militarily, and ensure that persecution could never again succeed at scale.
% FOUNDING_PROBLEM_CORROBORATION: Historical persecution of Jewish populations is documented by non-interested historical scholarship, victim testimony, state records, and public archives — the factual record of persecution is not seriously contested. The claim that territorial SOVEREIGNTY SOLVES this vulnerability is the contested element: (1) Some argue that diaspora integration and legal equality in liberal democracies suffice (Jewish communities in France, UK, Canada, US achieved security without return to Eretz Yisrael). (2) Others argue that the Holocaust proved only Jewish statehood guarantees security, that assimilation is illusory, and that diaspora vulnerability is permanent (Zionist movement orthodoxy, Israeli establishment position, many diaspora communities). (3) Some argue that statehood creates a NEW form of vulnerability — territorial conflict, military burden, regional isolation — that exceeds the original persecution risk (critics of Zionist premise). The causal link from historical persecution to territorial necessity is WHERE the contest is located; the persecution itself is not.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 at T0 (1948: legitimacy fresh, refugee-refuge coordination real, victims not yet fully displaced/forgotten) to 0.68 at T75 (2023: decades of occupation, settlement expansion, security doctrine expanded to justify territorial control far beyond immediate refuge function). The measurement series track the constraint's lifecycle drift: as the initial refuge function stabilizes and security doctrine replaces it as the operating justification, the extraction function becomes more visible. Theater ratio rises from 0.18 to 0.41: early years the coordination is genuine (absorbing refugees, building state); later years an increasing share of enforcement activity defends the security doctrine and settlement logic rather than refugee protection. Suppression requirement climbs from 0.48 to 0.72: early Israeli state suppression was minimal (Jewish majority willing, Arab populations could not effectively resist); modern suppression requires active enforcement (checkpoints, occupation apparatus, settlement guard). The shared time grid (T=0,12,25,40,60,75) allows measurement of each metric at each point — no metric is missing from any point.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state institutions and security establishment seats experience this constraint as legitimate coordination solving an existential problem (refuge from persecution, self-defense against hostile states). The Palestinian payer seats experience it as violent dispossession and occupation justified retroactively. The diaspora beneficiary seat vindicated by the constraint's operation. The international legal observer sees the same territorial control but interprets UN resolutions divergently: UN 181 (partition) as legitimizing origin versus UN 242/338 as mandating 1967-boundary framework. These divergences are STRUCTURAL, not observational — they derive from power asymmetry (who sets the rules, who enforces them) and from the foundational axiom choice (persecution-refuge vs. autochthony-displacement). The engine computes per-seat type from power, exit options, directionality; the authored claim (tangled_rope) reflects the asymmetry: genuine coordination function (refuge for diaspora) coupled with asymmetric extraction (Palestinian displacement justified by the same legitimacy framework).
 *
 * DIRECTIONALITY LOGIC:
 *   jewish_diaspora_populations: d ≈ 0.1 (beneficiary, organized, mobile exit — can leave Israel, vindicated by its existence but not dependent on its territorial expansion; low d). israeli_state_institutions: d ≈ 0.15 (institutional beneficiary-setters, arbitrage exit — can reframe legitimacy but not easily abandon statehood; beneficiary side but with enforcement obligations). palestinian_residents_1948: d ≈ 0.95 (powerless, trapped, victim — no exit, maximal extraction, collateral to the refuge arrangement). palestinian_territories_1967_onwards: d ≈ 0.82 (moderate power constrained by occupation, victim, generational horizon — higher than 1948 victims due to some organizational capacity, lower due to lack of exit to recognized alternatives). religious_zionist_constituencies: d ≈ 0.05 (beneficiary with foundational axiom, identity-locked but toward the axiom rather than away from it — the axiom makes exit unthinkable, but that fuses them with the beneficiary side, not the target side; divine promise is vindication, not extraction). security_establishment: d ≈ 0.2 (agenda setter with institutional benefit — high power, arbitrage exit, derives power from the constraint, beneficiary side). arab_state_governments: d ≈ 0.65 (excluded, powerful but constrained by regional configuration, high time horizon — they bear the cost of Israeli power but cannot exit the region; moderate target). international_legal_apparatus: d ≈ 0.5 (analytical, observes all seats, symmetric position).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: founding problem was 'Jewish persecution vulnerability and diaspora statelessness requiring sovereign refuge.' Status is contested — some hold it solved (Jews now have a state, diaspora security institutionally guaranteed), others hold it still live (antisemitism persists, diaspora vulnerability persists). The divergence reveals the mandatrophy: the constraint's operation has shifted from refugee absorption and diaspora protection (early function) to territorial control and security doctrine (current function). Theater ratio rising indicates performative activity (settlement ideology, security rhetoric) replacing genuine coordination. A piton hypothesis: the constraint persists because the security establishment and religious constituencies benefit from the legitimacy narrative, not because the original refugee-refuge function requires ongoing Palestinian displacement and occupation. The constraint's classification as tangled_rope (not piton) reflects the genuine coordination component (diaspora refuge) still operating, coupled with asymmetric extraction (Palestinian payers). If the founding problem (persecution vulnerability) were actually solved, the constraint could become a pure coordinate mechanism (covenant among diaspora members and Israeli state to maintain a refuge). That it persists in extractive form suggests either (a) the founding problem is still live (antisemitism, diaspora vulnerability real) or (b) secondary beneficiaries (security establishment, religious settlers) maintain it beyond its original function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_solution_causality,
    'Does Jewish statehood actually solve the historical vulnerability to persecution, or does it create a new form of vulnerability (territorial conflict, occupation, military burden)?',
    'Comparative historical analysis: examine Jewish diaspora security in liberal democracies post-WWII (France, UK, Canada) versus Israeli security situation post-1948 (conflicts with Arab states, Palestinian resistance, regional militarization). Compare mortality/displacement rates from persecution in diaspora vs. from territorial conflict in Israel.',
    'If statehood increases vulnerability, the founding problem framing is undermined and the constraint''s legitimacy shifts from refuge-necessity to preference-for-sovereignty. If statehood decreases vulnerability, the foundational axiom holds. If the answer is context-dependent (statehood provides security in some eras, not others), mandatrophy becomes clearer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persecution_solution_causality, empirical, 'Whether territorial sovereignty solves or exacerbates the historical persecution vulnerability.').

omega_variable(
    partition_as_legitimacy_origin,
    'Is UN 181 (partition) actually a source of legitimacy for Israeli statehood, or is it merely a historical coincidence used retroactively to justify it?',
    'Examine: (a) whether Israeli founding documents cite partition as legitimacy source; (b) whether the state would have claimed legitimacy without partition (it would have, via Jewish historical claim + refugee refuge claim); (c) whether partition is binding or merely recommendatory under international law; (d) whether Arab rejection of partition actually justifies subsequent Palestinian displacement (counterfactual: if Arabs had accepted partition, would Israeli expansion have stopped at partition boundaries?).',
    'If partition is foundational (logically necessary), challenge to partition legitimacy undermines the entire constraint. If partition is post-hoc framing, it is vulnerable to competing legitimacy claims (autochthony, continuous habitation). If partition is merely convenient but not necessary, the constraint''s justification shifts to pure persecution-refuge or religious claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_as_legitimacy_origin, conceptual, 'Whether UN partition is the foundational legitimacy source or a convenient historical marker.').

omega_variable(
    security_doctrine_expansion,
    'Does the security doctrine justifying territorial control represent a genuine threat-response, or has it expanded beyond the original refugee-refuge function to justify indefinite occupation and settlement?',
    'Track the scope and rhetoric of security justifications over time: early period (1948–1967) security doctrine narrower (border defense, refugee absorption); post-1967 security doctrine expands to justify occupation, settlements, control of Palestinian movement. Examine threat assessment: are the described threats (Arab state invasion, Palestinian attacks) proportional to the suppression deployed, or has suppression become the maintenance logic independent of threat level?',
    'If security doctrine has expanded, the theater_ratio rising is explained: performative maintenance of a constraint whose founding function has been satisfied. The constraint becomes piton-adjacent (maintained by institutional inertia, security establishment power, rather than by genuine ongoing refuge need). If security doctrine remains proportional to threat, theater_ratio should stay flat or decline; its rise signals drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_doctrine_expansion, empirical, 'Whether security doctrine has expanded beyond original refuge-protection function.').

omega_variable(
    foundational_axiom_disagreement_autochthony,
    'Is the foundational claim ''historical persecution justifies territorial refuge'' logically incompatible with the autochthony reading''s claim ''continuous Palestinian habitation justifies territorial presence,'' or do these represent different frameworks that could coexist?',
    'Examine the logical structure: if both claims are true-in-fact (Jews did suffer persecution AND Palestinians did inhabit continuously), then the readings differ in PRIORITY, not in logical compatibility. The question is whether one party''s legitimacy forecloses the other''s. In a two-state framework both could be honored; in a single-state framework they conflict directly.',
    'If logically incompatible (forecloses relation), the contest is irresolvable within a single framework and territorial partition is the only settlement. If they coexist because they apply to different moral agents (collective refugee right vs. individual inhabitation right), coexistence_with is correct and compromise frameworks become possible. The relation chosen (forecloses vs. coexists_with) shapes the political resolution logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_axiom_disagreement_autochthony, conceptual, 'Whether the zionist_refuge and palestinian_autochthony axioms logically foreclose each other or can coexist.').

omega_variable(
    religious_zionist_axiom_status,
    'For religious Zionists, does the divine promise (Eretz Yisrael boundaries) supersede the UN partition boundaries, making territorial expansion a foundational commitment rather than a security adjustment?',
    'Examine religious Zionist movement statements, settler theology, and political behavior: do they treat 1948 boundaries as permanent, 1967 as negotiable, or as merely interim steps toward fuller biblical claim? Survey the influence of religious constituencies on territorial expansion policy.',
    'If divine promise is foundational (not superseded by partition), then the reading''s claimed constraint-type (tangled_rope based on partition + security) understates the extractiveness: there is a second foundation (religious expansion drive) running parallel to the security doctrine, both justifying Palestinian displacement. The axioms section would need to include both secular-refuge and religious-promise as distinct (potentially conflicting) foundations within the same reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_zionist_axiom_status, preference, 'Whether religious Zionism''s divine promise axiom supersedes UN partition as the foundational legitimacy claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t12, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(terr_tr_t12, observed).
narrative_ontology:measurement(terr_tr_t25, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(terr_tr_t25, observed).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(terr_tr_t40, observed).
narrative_ontology:measurement(terr_tr_t60, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement_basis(terr_tr_t60, observed).
narrative_ontology:measurement(terr_tr_t75, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(terr_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t12, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(terr_be_t12, observed).
narrative_ontology:measurement(terr_be_t25, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(terr_be_t25, observed).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(terr_be_t40, observed).
narrative_ontology:measurement(terr_be_t60, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement_basis(terr_be_t60, observed).
narrative_ontology:measurement(terr_be_t75, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(terr_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t12, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(terr_su_t12, observed).
narrative_ontology:measurement(terr_su_t25, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 25, 0.63).
narrative_ontology:measurement_basis(terr_su_t25, observed).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(terr_su_t40, observed).
narrative_ontology:measurement(terr_su_t60, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(terr_su_t60, observed).
narrative_ontology:measurement(terr_su_t75, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement_basis(terr_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__zionist_refuge_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'territorial_legitimacy_dual'. The kernel is the legitimacy claim(s) grounding Israeli/Palestinian territorial control and statehood. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and foundational axioms. The zionist_refuge_reading frames legitimacy in historical persecution, UN partition acceptance, and security concerns; it coexists with the palestinian_autochthony_reading (which centers continuous habitation and displacement trauma) and influences the two_state_coexistence_reading (which accepts dual legitimacy and 1967 boundaries as compromise). All three readings must be generated as separate constraint stories with distinct ε-invariance; their relationship is recorded via network.affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, powerless, 0.95).
constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
