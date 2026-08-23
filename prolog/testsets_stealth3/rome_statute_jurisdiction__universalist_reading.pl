% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Jurisdiction — Universalist Reading (Consent-Transcending Mandate)
 *   domain: international law/treaty interpretation/institutional authority
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The Rome
 *   Statute's jurisdictional provisions are read here as establishing a
 *   mandate that transcends state consent: jurisdiction attaches through
 *   territorial presence on a state party's territory or through Security
 *   Council referral regardless of the accused's state's membership, victims'
 *   access does not depend on their state's consent, and official immunity
 *   yields for core crimes. The epsilon referent is the standing arrangement
 *   under contest — the jurisdictional regime as actually operated under this
 *   reading (territorial-trigger assertions over non-party nationals, council
 *   referrals, warrants against sitting officials) — assessed by this
 *   reading's own lights: what a universalist counts as legitimate
 *   accountability is not counted as extraction, but the reading can still
 *   see selective application, enforcement dependence, and institutional
 *   self-expansion as real costs. The sibling readings
 *   (sovereigntist_reading, hybrid_complementarity_reading) are separate
 *   constraints with their own files; the contest between readings is carried
 *   in the omega variables, not folded into this constraint. KEY AGENTS (by
 *   structural relationship): see key_agents. The claimed type and the
 *   metrics are authored independently — the claim states what this reading's
 *   structure is; the metrics describe how the arrangement actually operates.
 *
 * KEY AGENTS:
 *   - icc_court_organs: Primary agenda setter (institutional/trapped) — asserts jurisdiction, issues warrants, runs trials; commands no police of its own
 *   - un_security_council: Co-agenda setter (institutional/arbitrage) — holds the referral and deferral levers; three of five veto holders sit outside the treaty
 *   - atrocity_victims_unwilling_states: Primary beneficiary (powerless/trapped) — receives the forum, participation rights, and reparations access
 *   - state_parties_coalition: Beneficiary (organized/constrained) — pools deterrence and diplomatic standing; bears cooperation duties
 *   - host_territorial_states: Dual-positioned beneficiary/payer (moderate/constrained) — gains a path to accountability on its soil, absorbs retaliation from the accused's home state
 *   - international_criminal_law_profession: Secondary beneficiary (organized/mobile) — careers and funding scale with the mandate's reach
 *   - nonparty_great_powers: Primary target with arbitrage-grade exit (institutional/arbitrage) — nominally exposed, practically shielded; pays in friction and isolation, not surrendered suspects
 *   - nonparty_holdout_state_officials: Full-weight target (moderate/trapped) — indicted and exposed without their state's consent and without means of symmetrical retaliation
 *   - third_state_arrest_gatekeepers: Payer (moderate/constrained) — each warrant-execution decision imposes costs whichever way it goes
 *   - targeted_state_populations: Excluded voice (powerless/trapped) — judged by a tribunal their governments rejected and they were never consulted on
 *   - international_law_commentators: Analytical observer (analytical/analytical) — traces doctrinal evolution from outside operations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.48).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.58).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Jurisdiction — Universalist Reading (Consent-Transcending Mandate)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international law/treaty interpretation/institutional authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, 'c8c19bc2-fb69-4c0c-abf1-28d3c5539c11').
narrative_ontology:cs_kernel_codification('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', fixed_text).
narrative_ontology:cs_authority_grounding('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', lineage).
narrative_ontology:cs_interpretation_layer_present('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11').
narrative_ontology:cs_reading_relation('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', foundational, core_crime_jurisdiction_transcends_state_consent).
narrative_ontology:cs_axiom_status(core_crime_jurisdiction_transcends_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', core_crime_jurisdiction_transcends_state_consent, deontological).
narrative_ontology:cs_axiom('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', foundational, territorial_trigger_binds_nonparty_nationals).
narrative_ontology:cs_axiom_status(territorial_trigger_binds_nonparty_nationals, holdable).
narrative_ontology:cs_axiom_grounding('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', territorial_trigger_binds_nonparty_nationals, conventional).
narrative_ontology:cs_axiom('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', secondary, official_immunity_yields_to_core_crime_accountability).
narrative_ontology:cs_axiom_status(official_immunity_yields_to_core_crime_accountability, holdable).
narrative_ontology:cs_axiom_grounding('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', official_immunity_yields_to_core_crime_accountability, deontological).
narrative_ontology:cs_reference_frame('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', nuremberg_lineage_universal_mandate).
narrative_ontology:cs_drift_state('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', post_ukraine_warrant_sanctions_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8c19bc2-fb69-4c0c-abf1-28d3c5539c11', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, atrocity_victims_unwilling_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, state_parties_coalition).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, host_territorial_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_criminal_law_profession).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, nonparty_great_powers).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, nonparty_holdout_state_officials).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, third_state_arrest_gatekeepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, host_territorial_states).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, nuremberg_individual_responsibility_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, erga_omnes_character_of_core_crimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The chambers and Office of the Prosecutor of the permanent court at The Hague. They select situations, assert jurisdiction over conduct on state-party territory regardless of the accused's nationality, issue arrest warrants including against sitting heads of government of non-member states, and run trials and reparations proceedings. They command no police; every arrest, transfer, and evidence handover arrives through voluntary state cooperation. Their docket, budget, and staffing grow with each new situation opened.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_court_organs, agenda_setter,
    institutional, generational, trapped, global).

% The fifteen-member body holding two levers over the court's reach: it can refer situations in any state (as with Darfur and Libya) and can suspend investigations for renewable twelve-month periods. Its five permanent members hold vetoes; three of the five are not themselves parties to the statute, so the referral lever exposes others' nationals far more readily than their own.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% Survivors and families of mass atrocities in countries whose governments cannot or will not prosecute — northern Uganda, eastern Congo, Darfur. The court offers them a forum, participation rights, and a reparations register that exists nowhere else; access depends on investigators reaching territory their own state may control or contest.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, atrocity_victims_unwilling_states, beneficiary,
    powerless, biographical, trapped, global).

% The states that ratified the statute. They pool deterrence against atrocity perpetrators, gain standing in human-rights diplomacy, and share the court's assessed budget. In exchange they accept arrest-and-cooperation duties that can force choices between treaty loyalty and relations with powerful non-members, as South Africa's 2015 al-Bashir episode showed.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, state_parties_coalition, beneficiary,
    organized, generational, constrained, global).

% States on whose territory foreign forces commit alleged crimes — Afghanistan with respect to US conduct, Palestine with respect to Israeli conduct. Accepting or seeking an investigation opens a path to accountability for acts on their soil, and simultaneously draws intense pressure from the accused nationals' home governments, including aid and sanctions threats.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, host_territorial_states, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, host_territorial_states, payer).

% Prosecutors, defense counsel, NGO monitors, and academics whose careers, funding, and publications are organized around the court and its jurisprudence. Each expansion of jurisdictional doctrine opens new research agendas, consultancies, and posts; retrenchment shrinks them.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_criminal_law_profession, beneficiary,
    organized, biographical, mobile, global).

% Major military powers outside the treaty — the United States, Russia, China, India. Their nationals are formally reachable through territorial triggers or council referral, yet their size lets them retaliate (sanctions on court staff, bilateral non-surrender agreements, domestic cases against judges) and shield their personnel in practice. They pay in diplomatic friction, travel isolation for named officials, and legitimacy contests rather than in surrendered suspects.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, nonparty_great_powers, payer,
    institutional, generational, arbitrage, global).

% Officials of smaller states outside the treaty — Sudanese and Libyan figures under council referral, for instance. They carry the fullest weight of jurisdiction asserted without their state's consent: indictments, travel exposure, and asset risks they have little power to deflect, since their governments cannot retaliate symmetrically.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, nonparty_holdout_state_officials, payer,
    moderate, biographical, trapped, national).

% Governments of states that a wanted figure visits — South Africa with al-Bashir, Mongolia with Putin. Each visit forces a choice between executing a warrant (rupturing ties with the suspect's state) and letting the visitor pass (breaching cooperation duties and inviting assembly scrutiny). Costs land on whichever choice they make.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, third_state_arrest_gatekeepers, payer,
    moderate, biographical, constrained, national).

% Citizens of states under investigation who experience the court as an outside tribunal judging their country's conflicts. Their consent was never solicited — many live under governments that rejected the treaty — and the continental body articulating their grievance (the African Union's selectivity critique) gained them no seat in the jurisdictional design.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, targeted_state_populations, excluded,
    powerless, generational, trapped, regional).

% Scholars, former judges, and treaty-monitoring bodies analyzing the court's jurisdictional doctrines from outside its operations. They trace how territoriality, complementarity, and immunity doctrines evolve, and their assessments shape how states and counsel read the statute's reach.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_law_commentators, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, icc_court_organs).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a standing, pre-committed prosecutorial forum for genocide, war crimes, crimes against humanity, and aggression, available wherever territorial or council triggers fire, so that accountability does not depend on negotiating a new tribunal for each atrocity or on the goodwill of the perpetrator's own government.
% TRANSFER_FUNCTION: Moves prosecutorial authority across borders: from non-consenting states and their officials to the court and the victims it serves; and moves material burdens — arrest execution, evidence sharing, assessed contributions, diplomatic exposure — onto states parties, territorial hosts, and third-state gatekeepers.
% ABSENT_VOICES: Accused individuals and the publics of investigated states had no seat where jurisdictional doctrine was settled — the Assembly of States Parties and the chambers decided. African Union selectivity critiques arrived after the docket was set; defense perspectives enter only case-by-case, never in the design of the mandate's reach.
% DISAPPEARANCE_RATIONALE: If the consent-transcending assertion vanished overnight, victims in unwilling states would lose their only standing forum; non-member militaries would regain an absolute shield for conduct abroad; the deterrence architecture would revert to ad hoc council bargains priced case-by-case; and the professional ecosystem built on the court would contract sharply.
% FOUNDING_PROBLEM: Post-Nuremberg impunity: atrocity perpetrators were shielded by sovereignty, victors' discretion, and the absence of any standing forum, while the ad hoc tribunals of the 1990s proved slow, expensive, and dependent on political windows that open rarely.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Security Council's own referrals for Darfur and Libya concede the problem persists where the court's strongest opponents sit; General Assembly resolutions and independent commissions of inquiry document continuing atrocity crimes in states unable to prosecute. The sovereigntist bloc attests the problem is real while disputing this remedy — corroboration of the problem crosses the dispute even though endorsement of the solution does not.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48 sits mid-scale because the reading's own lights discount much of what a sovereigntist would count as extraction (prosecution of atrocity suspects is accountability, not rent), leaving the genuinely extractive residue: application that concentrates on actors too weak to resist, institutional growth that rewards each new situation opened, and cooperation burdens shifted onto third states. Suppression 0.58 is authored as a raw structural property — it is NOT scaled by power or scope; the engine scales only extractiveness. It reflects the machinery the mandate needs to hold: cooperation regimes, council leverage, and the doctrinal foreclosure of consent objections, checked by the court's inability to execute anything itself. Theater ratio 0.42 captures the growing declaratory share — warrants that will never be executed, sessions maintaining the universal posture — against a real functional core (convictions, reparations, travel isolation of named officials). Accessibility collapse 0.38: alternatives remain workable (national prosecution under complementarity, ad hoc and hybrid tribunals, national universal-jurisdiction statutes), so understanding the constraint does not close the option space. Resistance 0.72: sustained organized pushback — superpower sanctions on court personnel, bilateral non-surrender agreement campaigns, member withdrawals, continental-block hostility. Measurements run on ONE shared grid (points 0-24, roughly 2002-2026 at four-year steps: T0 entry into force, T4 first DRC/Uganda cases and the Kony warrant, T8 Kampala aggression amendments, T12 Kenya-case collapse and al-Bashir evasion, T16 Burundi/Philippines withdrawals and US threat escalation, T20 Ukraine referrals, T24 Putin-warrant and sanctions era): all three tracked metrics are authored at every point, so no metric borrows another's end-state. Gain flow: the gains demonstrably accrue to the court's own organs — each new situation enlarges docket, budget, and mandate; no other named seat captures the extraction. Fixing cost is prohibitive: unwinding the reading requires either treaty amendment (two-thirds adoption, seven-eighths ratification) or reversal of entrenched appellate doctrine, and the seats able to attempt either are split across the very contest this reading embodies.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the court's bench the arrangement is the rule of law completing Nuremberg; from a holdout official's position it is a foreign tribunal acting without his state's voice; from a great-power capital it is a jurisdictional claim that binds only those unable to punish its enforcers; from a survivor's position it is the only forum that ever summoned her tormentor. Same structure, divergent per-seat classifications — the engine computes the divergence from power, exit, and role; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (victims, state parties, host states, the profession) derive low directionalities — the mandate subsidizes them. Victim declarations (great powers, holdout officials, gatekeepers) derive high ones. Exit modulates within the victim set: great powers' arbitrage-grade retaliation places them far from the full-target end despite bearing nominal exposure, while trapped holdout officials sit nearest it. Host states' dual position (beneficiary with a payer secondary role) lands them near symmetric. Excluded voices (investigated publics) feed the consensus-provenance check, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sovereignty-shielded impunity — is live, so no mandatrophy is declared and the mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag. The tangled-rope claim keeps both halves visible: calling the arrangement pure coordination would erase the consent-bypass costs borne by non-parties; calling it pure extraction would erase the forum that exists nowhere else for victims of unwilling states. The resolution here is preventive: the measurement series shows extraction and theater rising together, which is the signature to watch for a future slide toward performance-heavy maintenance if enforcement keeps failing on high-value targets — flagged, not yet resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_consent_transcendence,
    'This constraint is one reading of the rome_statute_jurisdiction kernel: does the Statute''s jurisdictional architecture actually establish a consent-transcending mandate, or is that a contestable interpretive overlay on a consent-gated framework?',
    'Track appellate doctrine (territoriality and immunity rulings), Assembly of States Parties amendment attempts, and state-practice responses; a sustained contrary appellate line or a binding amendment closing the territoriality gap would resolve the reading contest.',
    'Under the sovereigntist sibling the victim set collapses to consenting-state cases and the costs measured here migrate to that file; under the hybrid sibling complementarity becomes a jurisdictional gate and this reading''s non-party exposure claims narrow sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_consent_transcendence, conceptual, 'Whether consent-transcendence is fixed in the kernel or is one contested reading of it.').

omega_variable(
    selectivity_vs_universality_gap,
    'Does the mandate operate universally in practice, or does enforcement select by target power — reaching Sudanese and Libyan officials while the personnel of great-power non-members remain untouchable?',
    'Comparative caseload and warrant-execution analysis indexed to target-state power and alliance position across the full interval.',
    'If selection dominates, the coordination story thins toward cover for power-filtered prosecution, effective costs concentrate on weak targets, and the arrangement slides toward the extractive end despite the universalist frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_vs_universality_gap, empirical, 'Gap between the universal claim and power-selective operation.').

omega_variable(
    enforcement_capacity_ceiling,
    'Can a court that executes no arrests itself sustain a consent-transcending mandate against determined great-power resistance, or does its persistence depend on the acquiescence it cannot compel?',
    'Warrant-execution rates correlated with target alignment and retaliatory capacity; count of situations closed or stalled for want of cooperation.',
    'If the ceiling holds, enforcement against high-value targets decays into declaratory maintenance while low-value targets carry the entire binding load — a bifurcated operation that strains any single-type verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_ceiling, empirical, 'Enforcement dependence on voluntary state cooperation.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the reading''s authority grounded in pooled treaty consent extended through territoriality, or in an independent Nuremberg-lineage mandate that requires no consent at all?',
    'Doctrinal analysis of the chambers'' own justifications: appeals to Article 12 text and state delegation versus appeals to erga omnes obligations and customary international law.',
    'Delegated-consent grounding keeps the arrangement inside negotiated coordination with an extractive edge; independent-mandate grounding recasts it as a claimed natural-law limit benefiting identifiable actors — a different beneficiary structure altogether.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Whether the mandate''s legitimacy is consent-derived or consent-independent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__universalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(rome_tr_t0, observed).
narrative_ontology:measurement(rome_tr_t4, rome_statute_jurisdiction__universalist_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement_basis(rome_tr_t4, observed).
narrative_ontology:measurement(rome_tr_t8, rome_statute_jurisdiction__universalist_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement_basis(rome_tr_t8, observed).
narrative_ontology:measurement(rome_tr_t12, rome_statute_jurisdiction__universalist_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement_basis(rome_tr_t12, observed).
narrative_ontology:measurement(rome_tr_t16, rome_statute_jurisdiction__universalist_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(rome_tr_t16, observed).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__universalist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(rome_tr_t20, observed).
narrative_ontology:measurement(rome_tr_t24, rome_statute_jurisdiction__universalist_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(rome_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(rome_be_t0, observed).
narrative_ontology:measurement(rome_be_t4, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement_basis(rome_be_t4, observed).
narrative_ontology:measurement(rome_be_t8, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement_basis(rome_be_t8, observed).
narrative_ontology:measurement(rome_be_t12, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(rome_be_t12, observed).
narrative_ontology:measurement(rome_be_t16, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement_basis(rome_be_t16, observed).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(rome_be_t20, observed).
narrative_ontology:measurement(rome_be_t24, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(rome_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(rome_su_t0, observed).
narrative_ontology:measurement(rome_su_t4, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement_basis(rome_su_t4, observed).
narrative_ontology:measurement(rome_su_t8, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement_basis(rome_su_t8, observed).
narrative_ontology:measurement(rome_su_t12, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(rome_su_t12, observed).
narrative_ontology:measurement(rome_su_t16, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement_basis(rome_su_t16, observed).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(rome_su_t20, observed).
narrative_ontology:measurement(rome_su_t24, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(rome_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, nuremberg_charter_precedent).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial label 'Rome Statute jurisdiction' covers three structurally distinct claims with different victim sets and enforcement structures. This file instantiates the universalist reading alone (epsilon authored for the consent-transcending arrangement as the universalist assesses it); the sovereigntist and hybrid-complementarity siblings carry their own epsilon, beneficiaries, and victims. The universalist reading is downstream of the Nuremberg precedent (which supplies its lineage authority), stands in foreclosure tension with the sovereigntist sibling, and coexists with the hybrid sibling; the links above carry the contamination-propagation edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
