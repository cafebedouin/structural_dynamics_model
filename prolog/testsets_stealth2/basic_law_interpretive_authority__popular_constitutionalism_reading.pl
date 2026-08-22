% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Distributed Interpretive Authority Regime
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'basic law
 *   interpretive authority': the popular-constitutionalism reading, under
 *   which constitutional meaning emerges from ongoing democratic contestation
 *   and no institution - court or legislature - may close it terminally. As a
 *   standing arrangement it has a real coordination face (it solves
 *   constitutional adaptation and legitimacy across generations by pooling
 *   interpretive sites) and a real extraction face (it taxes participants
 *   with permanent mobilization labor, taxes planners with permanent
 *   uncertainty, and exposes unpopular minorities to whatever wave is
 *   currently mobilized). The claim/metric independence rule is honored:
 *   claimed_type is authored from structure (both faces present, enforcement
 *   actively required to hold the no-finality line against recurrent closure
 *   attempts), and the metrics are authored from the arrangement's observed
 *   operation at interval end. KEY AGENTS (by structural relationship): -
 *   grassroots_social_movements: primary beneficiary and de facto
 *   agenda-setter (organized/identity_locked) - supplies contestation labor,
 *   harvests responsiveness; - apex_court_justices: principal institutional
 *   target (institutional/identity_locked) - denied terminal authority; -
 *   elected_legislatures: dual-positioned institutional actor
 *   (institutional/constrained) - loses finality claims, gains a responsive
 *   channel; - vulnerable_minorities_in_adverse_waves: exposed payers with
 *   contingent benefits (powerless/trapped); - finality_seeking_litigants and
 *   commercial_planning_interests: certainty-seeking payers
 *   (moderate/constrained; powerful/arbitrage); -
 *   ordinary_nonparticipating_citizens: diffuse payers outside the contest
 *   (powerless/trapped); - comparative_constitutional_theorists: analytical
 *   observer - sees the full structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.57).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism: Distributed Interpretive Authority Regime").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, 'a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0').
narrative_ontology:cs_kernel_codification('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', fixed_text).
narrative_ontology:cs_authority_grounding('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', distributed).
narrative_ontology:cs_reading_relation('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', foundational, persistent_popular_constituent_authority).
narrative_ontology:cs_axiom_status(persistent_popular_constituent_authority, holdable).
narrative_ontology:cs_axiom_grounding('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', persistent_popular_constituent_authority, deontological).
narrative_ontology:cs_axiom('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', secondary, intergenerational_meaning_revisability).
narrative_ontology:cs_axiom_status(intergenerational_meaning_revisability, holdable).
narrative_ontology:cs_axiom_grounding('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', intergenerational_meaning_revisability, instrumental).
narrative_ontology:cs_reference_frame('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', constituent_people_as_standing_authority).
narrative_ontology:cs_drift_state('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', contemporary_judicialized_advocacy_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a1e2f45d-1c86-4d73-adcf-1a4e9a36a9a0', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, grassroots_social_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, out_of_power_political_factions).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, civic_associations).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_legislatures).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, apex_court_justices).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, vulnerable_minorities_in_adverse_waves).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, finality_seeking_litigants).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, ordinary_nonparticipating_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, vulnerable_minorities_in_adverse_waves).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_legislatures).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, commercial_planning_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized citizen coalitions - abolitionists, suffragists, labor federations, civil-rights organizations - that press constitutional claims through rallies, conventions, boycotts, and electoral campaigns rather than through any single institution. They decide which questions become constitutional controversies and supply the labor of argument. Their organizational identities are built around the causes they contest; stepping back from contestation would dissolve the coalitions themselves.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, grassroots_social_movements, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, grassroots_social_movements, agenda_setter).

% Political coalitions currently excluded from governing institutions. Open contestation gives them a path to reshape fundamental law without first capturing courts or legislatures; when they return to office, the same openness exposes their achievements to reversal. Membership turns over with elections, but the structural position recurs.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, out_of_power_political_factions, beneficiary,
    organized, biographical, mobile, national).

% Churches, unions, bar associations, universities, and issue groups that host constitutional debate, train participants, and lend legitimacy to popular claims. They depend on a public sphere in which constitutional questions remain open; their convening role shrinks if a single institution settles everything.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, civic_associations, beneficiary,
    moderate, generational, constrained, national).

% Representative bodies that write ordinary law and propose amendments. They hold no final word on constitutional meaning - their statutes are one input among many, and popular majorities can repudiate their readings at the next mobilization. In exchange they receive a responsive channel: movements deliver mandates to their doorsteps, and major realignments of constitutional meaning run through legislation and amendment. Their planning horizon is the electoral calendar.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_legislatures, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_legislatures, payer).

% Judges on the highest court, appointed for life and trained to resolve disputes by issuing binding reasons. Under this arrangement their rulings are treated as contributions to an ongoing argument rather than endpoints: officials, movements, and scholars openly rehearse decided questions, and compliance is negotiated rather than commanded. Their professional identity is formed around authoritative judgment, which the arrangement permanently qualifies.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, apex_court_justices, payer,
    institutional, generational, identity_locked, national).

% Discrete groups - religious dissenters, racial and ethnic minorities, unpopular political sects - whose protections depend on winning public argument repeatedly. When contestation runs their way they gain recognition no single institution would have granted; when majorities mobilize against them, no terminal decision-maker stands between them and the wave, and exit by emigration or secession is rarely available.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, vulnerable_minorities_in_adverse_waves, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, vulnerable_minorities_in_adverse_waves, beneficiary).

% Parties to concrete disputes - property holders, contract parties, criminal defendants, administrative agencies - who need a definitive answer in order to plan. Every victory stays provisional: opponents may relitigate politically what they won or lost judicially, and private arbitration offers only partial escape at a price.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, finality_seeking_litigants, payer,
    moderate, immediate, constrained, national).

% Residents who live under constitutional rules but join no movement, file no suit, and attend no convention. Constitutional meaning shifts around them through contests they do not wage; their interests are protected only if someone else mobilizes, and the arrangement offers them no procedural guarantee that anyone will.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, ordinary_nonparticipating_citizens, payer,
    powerless, biographical, trapped, national).

% Firms and investors who price long-horizon commitments - infrastructure, contracts, compliance regimes - against the legal environment. Perpetual contestability raises hedging costs; they respond by lobbying, contracting around public law, relocating activity across jurisdictions, and funding favored sides of constitutional arguments.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, commercial_planning_interests, payer,
    powerful, immediate, arbitrage, global).

% Scholars who compare how different polities settle, or refuse to settle, interpretive authority, and who trace which arrangements protect minorities, adapt to crises, or decay into ritual. They bear few of the arrangement's costs and collect little of its gains; their analyses travel across the cases they study.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, comparative_constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of legitimate constitutional adaptation: how a polity updates its fundamental law across generations without freezing it under unelected guardians or subjecting it to bare electoral whim. Distributed contestation pools many interpretive sites - movements, elections, conventions, courts, scholarship - so meaning tracks shifting democratic understandings while remaining tethered to argument rather than raw preference.
% TRANSFER_FUNCTION: Moves interpretive authority and agenda-setting power from institutional centers (apex courts, legislatures) to whichever organized coalitions are currently most effective at contestation; moves certainty and finality away from litigants and planners into the open contest; moves mobilization labor out of citizens' lives and into the contest itself.
% ABSENT_VOICES: Those without organizational capacity - the unorganized poor, non-citizens subject to the constitutional order, future generations - have no seat in the contest yet live under its outputs. Committed judicial supremacists and parliamentary sovereigntists are present in the discourse but their terminal claims are structurally unheard by design: the arrangement's constitutive move is to deny any seat from which a final claim could be made.
% DISAPPEARANCE_RATIONALE: If the no-terminal-authority norm vanished overnight, either judicial finality or legislative sovereignty would consolidate within a decade: the apex court's holdings would become self-executing endpoints, or parliamentary majorities would claim plenary interpretive power. Movements would lose their channel and reroute through litigation or statute; minority protections would come to depend wholly on whichever institution won the consolidation; the amendment-and-mobilization economy that sustains civic associations would collapse into professionalized doctrine.
% FOUNDING_PROBLEM: How to make fundamental law legitimate and correctable in a republic without a monarch or established church: the founding generation rejected both British parliamentary omnipotence and entrenching guardianship, and the Anti-Federalist and later Reconstruction-era lineage insisted that the people, not their agents, remain the constituent power - that each generation must be able to reopen what its predecessors settled.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: the judicial-supremacy tradition (Federalist 78, the Cooper v. Aaron line) affirms that the interpretive-authority question remains live even while answering it oppositely; comparative constitutional scholarship documents parallel unresolved struggles across jurisdictions; the recurring amendment crises and movement-driven transformations of the historical record corroborate that the founding problem never closed. No party to the dispute treats the question as settled - which is itself the corroboration.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.57: the arrangement's costs are real and recurring - mobilization labor levied on anyone who wants constitutional meaning defended, relitigation risk levied on litigants, hedging costs levied on planners, exposure levied on minorities during adverse waves - but they purchase genuine adaptive capacity, so epsilon sits mid-range rather than high. Suppression 0.58: the arrangement actively suppresses terminal-settlement alternatives; courts asserting finality and legislatures claiming plenary sovereignty are overridden by political and civic force, and in the polarized present the enforcement effort needed to hold the no-finality line is high. Theater 0.35: much contestation is functional (meaning demonstrably changes through it), but a growing share is performative - symbolic resolutions, ritualized confirmation battles, academic commemoration of contests already decided elsewhere. Accessibility_collapse 0.28: alternatives do NOT collapse - judicial supremacy and parliamentary sovereignty remain live and periodically reasserted, which is precisely why enforcement is perpetually required. Resistance 0.68: the arrangement meets continuous resistance from the institutions it dispossesses and from elites who prefer settlement. The temporal series run on one shared ten-point grid and display a full crisis-mobilization-transformation-relaxation-accumulation cycle: extraction and suppression peak together in the Lochner-era and New Deal crisis (t=100-125), trough in the post-transformation settlement (t=150), then ratchet upward again as contestation professionalizes and polarizes (t=175-220). The oscillation is partly an extraction mechanism in itself: episodic openings reward mobilization, then close, conditioning movements to keep investing - intermittent reinforcement at civilizational scale. Base_properties were measured at interval end (t=220), on the rising branch after the trough. Fixing_cost is prohibitive: terminating the contest would require one coalition to defeat entrenched movement infrastructure and civic culture while the benefits of settlement disperse across everyone else - the fixer bears concentrated costs for diffuse gains. Gain_flow is authored as grassroots_social_movements: the transfer function demonstrably moves interpretive authority and agenda control first to the currently-most-effective contesting coalition; movements cannot bank their winnings (everything is recontestable), but they are the seat where the arrangement's yields accrue before diffusing.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute differently, and the divergence is structural, not rhetorical. From the apex-court seat, the arrangement operates as dispossession: lifetime-appointed judges trained for final judgment find their holdings rehearsed, relitigated, and renegotiated indefinitely, with compliance hanging on persuasion. From the movement seat, the same arrangement operates as liberty: the absence of a terminal gatekeeper is what keeps their claims admissible. Elected legislatures straddle the gap - stripped of sovereignty they barely exercised, enriched with a responsiveness channel their judicial-supremacy counterparts lack. Vulnerable minorities experience both faces in sequence: empowerment when the wave runs their way, exposure when it turns. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (movements, out-of-power factions, civic associations, legislatures) drive those seats toward the subsidized end of the directionality range; victim declarations (apex court justices, exposed minorities, finality-seeking litigants, non-participating citizens) drive those seats toward the target end. Exit modulation sharpens the spread: the court's identity_locked position (professional identity fused with authoritative judgment, life tenure eliminating exit) places it nearer the full-target end than its nominal power would suggest, while commercial planning interests' arbitrage exit dampens their effective extraction despite paying real hedging costs. Dual-positioned agents carry secondary_role so both directions register: legislatures (beneficiary/payer) sit nearer symmetric than a pure-beneficiary derivation would place them, and exposed minorities (payer/beneficiary) sit somewhat below the pure-victim derivation because movement-era gains are real. No directionality overrides were authored: the beneficiary/victim-plus-exit derivation captures every seat's relationship adequately, and the two genuinely dual seats are handled through secondary_role rather than override, since overrides keyed to power atoms would collide with other stakeholders sharing those atoms (the court and the legislatures are both institutional; minorities and non-participating citizens are both powerless).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - reconciling constitutional stability with democratic self-authorship - is live, so no mandatrophy is declared and none should be inferred from the rising late-interval extraction: that rise reflects re-polarization and professionalization, not a dead mandate kept alive by habit. The classification guards against two opposite mislabels. Reading the arrangement as pure rope (its self-description: 'democracy') erases the asymmetric burdens - the mobilization tax, the certainty levy, minority exposure - that fall unevenly across seats. Reading it as pure snare (its critics' description: 'chaos that rewards the loudest') erases the genuine coordination function and the absence of any capturing seat: gains accrue to movements only episodically and cannot be banked, exits exist for some seats, and the arrangement's product disperses. Tangled rope holds both faces in one structure, which is what two centuries of operation display. The piton risk is real but prospective, not current: if the no-finality norm decays into mutual elite veto performed as principle (see the persistence_basis omega), theater_ratio would climb past 0.5 and the arrangement would drift toward inertial performance - the late-interval theater trend (0.16 to 0.35 since the trough) is the early-warning signature worth monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of the kernel basic_law_interpretive_authority (reading: popular_constitutionalism). If the judicial_supremacy_reading were instantiated instead, which structural facts invert?',
    'Compile the sibling stories and compare computed classifications: under judicial supremacy the movements'' seat flips from beneficiary to constrained supplicant, the apex court flips from payer to agenda_setter/beneficiary, and epsilon migrates onto litigants and minorities excluded from doctrinal access.',
    'The disagreement between readings is located in exactly one structural element - whether any institution may close constitutional meaning. Resolving it in favor of a sibling converts this story''s beneficiaries into payers and vice versa; the epsilon values of the three stories are not comparable across readings because each reading constitutes a different arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame omega: which sibling reading is instantiated determines the entire beneficiary/victim topology.').

omega_variable(
    minority_protection_under_contestation,
    'Does distributed contestation systematically expose discrete and insular minorities to majoritarian waves relative to a court-protected baseline, or do movement-era gains (abolition, suffrage, civil rights) outweigh episodic exposure?',
    'Comparative panel analysis of minority-rights outcomes across polities differing in interpretive-authority settlement, controlling for wealth and democratization sequence; within-case analysis of minority fortunes during mobilization spikes versus doctrinal-consolidation periods.',
    'If exposure is systematic, the arrangement''s effective extraction on the powerless seats rises sharply and the classification trends snare-flavored for those seats; if episodic gains dominate, the tangled_rope reading holds and the secondary beneficiary role of minorities is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_under_contestation, empirical, 'Whether perpetual contestation protects or exposes vulnerable minorities.').

omega_variable(
    contestation_channel_capture,
    'Are the arrangement''s contestation channels open in fact, or captured by organized wealth - as in the Gilded Age, when formally open contestation produced doctrine favorable to industrial capital?',
    'Correlate campaign-finance and lobbying expenditure with constitutional-change outcomes across the interval; test whether movement success rates track resource asymmetry more than popular support.',
    'Demonstrated capture concentrates the arrangement''s gains in a named seat, converting diffuse receipt into captured receipt and pushing the classification toward snare; sustained openness supports the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contestation_channel_capture, empirical, 'Whether money captures the channels through which constitutional meaning is contested.').

omega_variable(
    persistence_basis_no_finality_norm,
    'Does the no-terminal-authority norm persist by genuine democratic attachment, or merely as a trench-warfare equilibrium in which rival elites mutually veto each other''s closure attempts?',
    'Elite-commitment surveys combined with counterfactual institutional history: identify episodes where one coalition could have consolidated finality at acceptable cost and declined; distinguish principled renunciation from strategic forbearance.',
    'If the norm is mere mutual veto, the arrangement''s performative share is far higher than authored - the theater_ratio understates decay toward piton-like ritual contestation; if attachment is genuine, the arrangement is robustly coordinative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persistence_basis_no_finality_norm, conceptual, 'Whether the arrangement is lived practice or an unstable truce performed as principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blia_popcon_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(blia_popcon_tr_t25, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(blia_popcon_tr_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(blia_popcon_tr_t75, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 75, 0.33).
narrative_ontology:measurement(blia_popcon_tr_t100, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 100, 0.36).
narrative_ontology:measurement(blia_popcon_tr_t125, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 125, 0.22).
narrative_ontology:measurement(blia_popcon_tr_t150, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 150, 0.16).
narrative_ontology:measurement(blia_popcon_tr_t175, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 175, 0.24).
narrative_ontology:measurement(blia_popcon_tr_t200, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 200, 0.31).
narrative_ontology:measurement(blia_popcon_tr_t220, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 220, 0.35).

% Extraction over time
narrative_ontology:measurement(blia_popcon_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(blia_popcon_be_t25, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 25, 0.36).
narrative_ontology:measurement(blia_popcon_be_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(blia_popcon_be_t75, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement(blia_popcon_be_t100, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement(blia_popcon_be_t125, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 125, 0.44).
narrative_ontology:measurement(blia_popcon_be_t150, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 150, 0.38).
narrative_ontology:measurement(blia_popcon_be_t175, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 175, 0.47).
narrative_ontology:measurement(blia_popcon_be_t200, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 200, 0.53).
narrative_ontology:measurement(blia_popcon_be_t220, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 220, 0.57).

% Suppression requirement over time
narrative_ontology:measurement(blia_popcon_su_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(blia_popcon_su_t25, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 25, 0.26).
narrative_ontology:measurement(blia_popcon_su_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(blia_popcon_su_t75, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 75, 0.48).
narrative_ontology:measurement(blia_popcon_su_t100, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement(blia_popcon_su_t125, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 125, 0.6).
narrative_ontology:measurement(blia_popcon_su_t150, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 150, 0.42).
narrative_ontology:measurement(blia_popcon_su_t175, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 175, 0.5).
narrative_ontology:measurement(blia_popcon_su_t200, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 200, 0.56).
narrative_ontology:measurement(blia_popcon_su_t220, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 220, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'who interprets the constitution' covers three structurally distinct arrangements - judicial finality, legislative finality, and distributed contestation - each with its own epsilon, beneficiary/victim structure, and failure modes. This story authors ONLY the popular-constitutionalism arrangement. The judicial-supremacy story is the empirically dominant settlement in many jurisdictions and exerts downstream pressure on this one (visible in this story's drift_state: practice has migrated toward judicialized advocacy); the parliamentary-sovereignty story is the operative settlement in Westminster systems. Each sibling is a separate file; the family links run through network.affects_constraints in all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
