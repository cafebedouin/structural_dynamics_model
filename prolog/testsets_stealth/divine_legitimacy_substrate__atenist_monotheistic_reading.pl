% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Exclusive-Revelation Legitimacy Arrangement
 *   domain: religious/political
 *
 * SUMMARY:
 *   During roughly seventeen years in the mid-fourteenth century BCE, the
 *   Egyptian crown proclaimed the Aten the sole existing god, closed and
 *   dispossessed the temples of the older gods, erased their names from
 *   monuments, relocated the capital to a purpose-built city, and reserved
 *   all teaching of the god to the royal couple. This story instantiates ONE
 *   reading of the divine_legitimacy_substrate kernel — the
 *   atenist_monotheistic_reading — as a clean, epsilon-invariant constraint;
 *   the amun_polytheistic_reading and folk_syncretistic_reading are separate
 *   files linked through the network, not described here. The epsilon
 *   referent is the standing Atenist arrangement itself, assessed by the
 *   reading's own lights: within the frame, exclusive royal mediation is
 *   ontological necessity rather than theft, so the reading-indexed
 *   extraction is low-to-moderate (0.34 at interval end), registering mainly
 *   the mounting coercive costs even a committed insider would acknowledge.
 *   The claim/metric gap is deliberate: the arrangement is CLAIMED as
 *   mountain (its self-presentation is cosmic fact — the god's uniqueness
 *   asserted as reality's structure, not policy) while the authored metrics
 *   describe totalizing enforcement, substantial covert resistance, and
 *   late-period performative hollowing. The engine measures that divergence;
 *   the author does not reconcile it.
 *
 * KEY AGENTS:
 *   - pharaoh_royal_household: agenda-setting seat (institutional/arbitrage) — authors the doctrine, commands its enforcement, and receives the displaced flows
 *   - amarna_appointed_elite: secondary beneficiary (organized/identity_locked) — holds office, estate, and tomb entirely by regime grant
 *   - amun_priesthood: primary dispossessed party (institutional/trapped) — endowments seized, name proscribed, institutional continuity maintained underground until the regime passes
 *   - traditional_cult_personnel: dispossessed cult staff (moderate/trapped) — livelihoods dissolved with the endowed cults
 *   - egyptian_households: diffuse cost-bearers (powerless/constrained) — lose cult access, route devotion through the throne, supply corvee labor
 *   - foreign_vassal_chancelleries: excluded external parties (moderate/mobile) — petitions unanswered, hedging toward rival powers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.34).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.9).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, mountain).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Exclusive-Revelation Legitimacy Arrangement").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "religious/political").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).
domain_priors:emerges_naturally(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '763387f9-2f12-46dd-a30a-e00f57056702').
narrative_ontology:cs_kernel_codification('763387f9-2f12-46dd-a30a-e00f57056702', fixed_text).
narrative_ontology:cs_authority_grounding('763387f9-2f12-46dd-a30a-e00f57056702', extraction).
narrative_ontology:cs_reading_relation('763387f9-2f12-46dd-a30a-e00f57056702', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('763387f9-2f12-46dd-a30a-e00f57056702', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('763387f9-2f12-46dd-a30a-e00f57056702', foundational, aten_exclusive_deity_claim).
narrative_ontology:cs_axiom_status(aten_exclusive_deity_claim, holdable).
narrative_ontology:cs_axiom_grounding('763387f9-2f12-46dd-a30a-e00f57056702', aten_exclusive_deity_claim, theological).
narrative_ontology:cs_axiom('763387f9-2f12-46dd-a30a-e00f57056702', foundational, royal_mediation_necessity).
narrative_ontology:cs_axiom_status(royal_mediation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('763387f9-2f12-46dd-a30a-e00f57056702', royal_mediation_necessity, theological).
narrative_ontology:cs_reference_frame('763387f9-2f12-46dd-a30a-e00f57056702', exclusive_pharaonic_revelation).
narrative_ontology:cs_drift_state('763387f9-2f12-46dd-a30a-e00f57056702', late_amarna_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('763387f9-2f12-46dd-a30a-e00f57056702', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_royal_household).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, amarna_appointed_elite).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_cult_personnel).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, egyptian_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proclaims the Aten the sole god through royal hymns and decrees, closes the temples of the older gods, redirects their endowments to the new capital Akhetaten, and teaches the god's ways personally alongside the queen, so that ordinary worshippers address the royal pair rather than the god directly. Renames himself, moves the court, and rebuilds the festival calendar around the new cult. Abandoning the doctrine would mean renouncing the stated basis of his own authority.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_royal_household, agenda_setter,
    institutional, biographical, arbitrage, national).

% Officials, soldiers, and scribes raised up from outside the old priestly families, granted office, estates, and rock-cut tombs in the new capital. Rank, property, and burial places all exist by royal grant under the new order; reverting to earlier allegiances would cost them everything they hold.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amarna_appointed_elite, beneficiary,
    organized, biographical, identity_locked, national).

% Custodians of the wealthiest god's establishment in Egypt. Their temples are closed, their endowments diverted, and their god's name chiselled out of monuments, including from inside the royal family's own cartouches. They keep institutional memory and clandestine fidelity alive across the generation, with no lawful arena in which to speak.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, excluded).

% Priests, lectors, and artisans serving Osiris, Ptah, Hathor, and the other established gods. Their livelihoods depend on endowed cults that are closed or starved of offerings; some are absorbed into the new capital's establishments under supervision, others dispersed.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_cult_personnel, payer,
    moderate, biographical, trapped, regional).

% Villagers and townspeople whose household shrines, local festivals, and funerary customs presuppose the older gods. Official devotion runs exclusively through the royal couple; the old rites continue only privately and at risk. They also supply the corvee labor that builds the new capital.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, egyptian_households, payer,
    powerless, biographical, constrained, national).

% Client rulers in the Levantine city-states and neighboring territories who petitioned the palace for gold, troops, and arbitration. The preserved diplomatic archive shows repeated appeals going unanswered while the court attends to its internal religious program; some begin corresponding with rival powers instead.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, foreign_vassal_chancelleries, excluded,
    moderate, immediate, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_royal_household).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a single state cult with one patron, one festival calendar, and one authorized teaching, administered centrally from the new capital, replacing a landscape of many endowed cults with one uniform observance.
% TRANSFER_FUNCTION: Moves interpretive authority, temple endowments, corvee labor, and devotional attention from the established priesthoods and village practice to the royal house and its new capital.
% ABSENT_VOICES: The dispossessed Amun clergy and the adherents of the older cults had no forum; their objection survives only in covert practice and in the successor administration's later restoration inscriptions. Village practitioners continued quietly beneath the official observance, and foreign client rulers petitioned through channels that went largely unanswered.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the court returns to Thebes and Memphis, the new capital is abandoned, the closed temples reopen, confiscated endowments are restored, and the appointed elite is reabsorbed into the restored establishments — which is approximately what happened within a decade of the founder's death.
% FOUNDING_PROBLEM: Wealth and independent interpretive authority had accumulated in the Amun establishment and allied cults to a degree that rivaled and constrained the crown's own legitimacy apparatus.
% FOUNDING_PROBLEM_CORROBORATION: Pre-Amarna administrative records and donation stelae document the scale of the Amun endowments independently of any Atenist claim; the restoration stela of the succeeding administration — issued from outside the Atenist beneficiary set — attests both the depth of the suppression and the disorder its architects left behind; Hittite diplomatic correspondence independently corroborates the regime's inward turn and neglected obligations.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, ExtMetricName, E),
    domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(divine_legitimacy_substrate__atenist_monotheistic_reading),
    narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-to-moderate (0.34) because the referent is assessed by the reading's own lights: inside the frame the mediation monopoly is how reality works, and the residual registered cost is the deepening confiscation and proscription of the later reign. Suppression is authored high (0.90) as a raw structural property — it is NOT scaled by power or scope — reflecting temple closures, name erasure, endowment seizure, and enforced ritual conformity; the trajectory shows an enforcement ratchet that intensifies rather than relaxes across the interval. Theater ratio rises from 0.08 to 0.38: early devotion appears sincere and the ritual machinery functional, while the late period shows rote procession, stalled construction, and a thinning court — performance replacing conviction as voluntary attachment fails. Accessibility collapse is 0.78: official alternatives were closed almost completely, but archaeology of the workers' village shows old-god figures in private households, keeping the value below mountain-grade collapse. Resistance is 0.50: open resistance was effectively impossible, but covert fidelity, scribal preservation of proscribed names, and the speed of post-reign reversal document real withheld consent. All three tracked series run on one shared time grid (years 0, 3, 6, 9, 12, 15, 17) so every metric is authored at every examined point; the trajectories are monotonic, not cyclical — the oscillation-driven dynamics guidance does not apply.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute opposite types from the same structure. From the royal seat the arrangement is the correct ordering of reality that the throne maintains at great cost; from the priesthood seat it is dispossession enforced by erasure; from the household seat it is the loss of approachable gods in exchange for a remote one reachable only through the palace; from the vassal chancellery it is the withdrawal of a mediating role they had built diplomacy around. The engine computes this divergence from power, exit, and directional position — the authored mountain claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The royal household sits nearest the beneficiary pole: it writes the rules, holds arbitrage-grade exit (it can rename, relocate, and redefine the doctrine at will), and receives the displaced endowments and labor. The appointed elite is derivative-beneficiary with identity-locked exit — its entire position exists by regime grant. The Amun priesthood is the fullest target: trapped exit, seized assets, proscribed identity, maximal effective extraction. Traditional cult personnel and households are targets with constrained-to-trapped exit; households bear diffuse costs (labor, lost access) that keep them high-d but slightly short of the priesthood's position. The vassal chancelleries are excluded rather than targeted — they bear costs of the throne's inward turn but retain partial exit through realignment, placing them mid-scale.
 *
 * MANDATROPHY ANALYSIS:
 *   This arrangement's failure mode is the opposite of atrophy: it did not decay into theatrical maintenance while its function lapsed — it terminated outright when its enforcer died, with the capital abandoned rather than curated and the successor administration actively reversing it. Nothing theatrical sustained it afterward, so the piton signature is absent despite the rising late-period theater ratio, which measures hollowing within the arrangement's life, not post-functional curating. The classification discipline prevents two mislabels: accepting the mountain claim would naturalize a constructed legitimacy monopoly (the false-summit path the declared beneficiaries trigger), while reading the thin coordination function as proof of pure cover would miss the genuine unified-cult coordination the arrangement did provide in its early phase. The founding-problem interview locates the arrangement correctly: a live problem (clerical power rivaling the crown) addressed by a solution that died with its author rather than outliving its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_monopoly,
    'Is the exclusivity of the Aten a genuine natural-law fact about divinity, or a constructed arrangement whose principal beneficiary is the royal house?',
    'Comparative doctrinal history against the post-reign reversal record: if the arrangement dissolves immediately upon the founder''s death and the successor administration restores everything it displaced, the naturality claim fails.',
    'If constructed, the mountain claim fails false-summit review and the arrangement classifies instead by its enforcement and asymmetry profile; if genuine, the low reading-indexed extraction stands as the price of cosmic order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_monopoly, empirical, 'Whether the exclusivity doctrine reports reality''s structure or concentrates legitimacy in one seat.').

omega_variable(
    sincerity_vs_legitimacy_cover,
    'Was exclusive Atenism sincerely held theology, or post-hoc cover for consolidating legitimacy and diverting cult wealth?',
    'Internal chronology: whether the hymnic and cosmological innovation predates and exceeds what wealth diversion would require, versus material-flow records showing confiscation driving doctrine.',
    'Sincere theology leaves a genuine coordination residue (a unified cult people would partly maintain voluntarily); cover leaves none, and the arrangement reads as pure extraction with a doctrinal mask.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_vs_legitimacy_cover, conceptual, 'Whether the coordination face of the arrangement is functional or decorative.').

omega_variable(
    covert_practice_depth,
    'How much traditional household and cult practice persisted covertly beneath the official exclusivity?',
    'Archaeology of the Amarna workers'' village and domestic contexts: private votive objects of the old deities found in officially Atenist households.',
    'Deeper covert persistence lowers effective accessibility collapse below the authored 0.78 and raises measured resistance; near-total compliance would push accessibility collapse toward mountain-grade values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covert_practice_depth, empirical, 'Gap between official exclusivity and actual practiced religion.').

omega_variable(
    mediation_necessity_in_frame,
    'Within the Atenist frame itself, is exclusive royal mediation an ontological necessity (the god is too remote for direct approach) or a self-serving concentration?',
    'Internal theological analysis: test the transcendence argument in the frame''s own hymns against whether any text licenses lay approach to the god, and against the practical routing of all petition through the throne.',
    'This fixes the reading-indexed extraction floor: if mediation is necessary within the frame, the low authored epsilon stands; if self-serving even by the frame''s lights, epsilon rises toward the analytical estimate and the beneficiary structure becomes decisive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediation_necessity_in_frame, conceptual, 'Source of the reading-indexed extraction assessment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.09).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(divi_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.14).
narrative_ontology:measurement(divi_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(divi_tr_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.38).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.23).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement(divi_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.29).
narrative_ontology:measurement(divi_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.31).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 15, 0.33).
narrative_ontology:measurement(divi_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.66).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.76).
narrative_ontology:measurement(divi_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.83).
narrative_ontology:measurement(divi_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.87).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 15, 0.89).
narrative_ontology:measurement(divi_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Akhenaten's religious revolution' covers three structurally distinct claims about where divine legitimacy originates, decomposed per the epsilon-invariance principle into a constraint family: this file authors the atenist_monotheistic_reading (exclusive revelation through the king; interpretive monopoly; dismantled temple economies); amun_polytheistic_reading authors the incumbent arrangement (legitimacy through established priestly interpretation of a multi-deity cosmology headed by Amun-Ra); folk_syncretistic_reading authors the persistent substratum (legitimacy through pragmatic household and village incorporation of many deities). The sibling files carry their own epsilon values and victim structures; the Amun reading is the upstream incumbent whose displacement this reading enforced, and the folk reading is the downstream practice that persisted covertly beneath both official arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
