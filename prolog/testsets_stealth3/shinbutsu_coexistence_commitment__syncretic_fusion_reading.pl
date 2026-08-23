% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Syncretic-Fusion Order (Medieval Kenmitsu Settlement)
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   In the medieval Japanese settlement historians call the kenmitsu order,
 *   the great Buddhist establishments resolved the standing rivalry between
 *   the imported universal religion and the native kami cults by declaring
 *   the two pantheons one reality under two aspects: every kami is a local
 *   appearance — a 'trace' — of a universal Buddhist original (Amaterasu of
 *   Dainichi, Hachiman of Amida, the Kasuga deity of Kannon). This was not a
 *   private opinion but the operating constitution of a religious order:
 *   shrine-temple complexes institutionalized it, ordained clergy took ritual
 *   precedence at shrines, correspondence tables assigned every major deity
 *   its original, and the doctrinal schools' transmission lineages policed
 *   interpretation. The framework delivered real integration — one ritual
 *   cosmos, standardized calendars, salvation extended to native gods and
 *   their worshippers — while transferring doctrinal jurisdiction, estate
 *   income, and ritual precedence from hereditary shrine lines to the
 *   monastic complex. The Meiji state destroyed the arrangement wholesale
 *   beginning in 1868, dissolving the shrine-temples and outlawing the
 *   doctrine's public assertion. This story measures the arrangement across
 *   its full operational life, 850-1868. KEY AGENTS (by structural
 *   relationship): - kenmitsu_temple_establishments: Agenda-setter and
 *   primary beneficiary (institutional/arbitrage) — authors the
 *   correspondence doctrine, staffs the shrine-temples, receives the revenue
 *   flows - court_ritual_aristocracy: Secondary beneficiary
 *   (powerful/constrained) — purchases unified legitimation for imperial
 *   sacrality - kamakura_muromachi_warrior_elite: Dual-positioned
 *   beneficiary-payer (institutional/arbitrage) — pays patronage, collects
 *   legitimation and administrative cover - yamabushi_shugendo_orders:
 *   Beneficiary (moderate/mobile) — the fusion is their operating licence -
 *   hereditary_shrine_priesthoods: Primary target (moderate/identity_locked)
 *   — deities demoted, precedence lost, office inescapable -
 *   ise_watarai_priest_lineage: Resisting target (organized/constrained) —
 *   internal counter-tradition of kami primacity -
 *   local_kami_cult_communities: Diffuse target (powerless/trapped) — absorbs
 *   calendar and revenue effects without voice - kokugaku_nativist_scholars:
 *   Excluded critic (organized/constrained) — outside the doctrinal
 *   conversation until it mattered - historians_of_japanese_religion:
 *   Analytical observer (analytical/analytical) — reconstructs the political
 *   economy from records
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.66).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.52).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Syncretic-Fusion Order (Medieval Kenmitsu Settlement)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '0bd2305e-d3da-4f14-819a-4fe8776b9ae2').
narrative_ontology:cs_kernel_codification('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', formalized).
narrative_ontology:cs_authority_grounding('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', lineage).
narrative_ontology:cs_interpretation_layer_present('0bd2305e-d3da-4f14-819a-4fe8776b9ae2').
narrative_ontology:cs_reading_relation('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', shinbutsu_coexistence_commitment__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', foundational, kami_are_trace_manifestations_of_buddhas).
narrative_ontology:cs_axiom_status(kami_are_trace_manifestations_of_buddhas, overridden).
narrative_ontology:cs_axiom_grounding('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', kami_are_trace_manifestations_of_buddhas, theological).
narrative_ontology:cs_axiom('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', secondary, kami_cults_complete_only_under_buddha_dharma).
narrative_ontology:cs_axiom_status(kami_cults_complete_only_under_buddha_dharma, overridden).
narrative_ontology:cs_axiom_grounding('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', kami_cults_complete_only_under_buddha_dharma, instrumental).
narrative_ontology:cs_reference_frame('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', medieval_honji_suijaku_orthodoxy).
narrative_ontology:cs_drift_state('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', post_meiji_separation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('0bd2305e-d3da-4f14-819a-4fe8776b9ae2', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kenmitsu_temple_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, court_ritual_aristocracy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kamakura_muromachi_warrior_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, yamabushi_shugendo_orders).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, hereditary_shrine_priesthoods).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, ise_watarai_priest_lineage).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cult_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kamakura_muromachi_warrior_elite).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, mahayana_universal_salvation_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, chingokokka_protective_rulership_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The great Nara and esoteric monastic complexes — Todaiji, Kofukuji, Onjoji, Enryakuji, Toji — whose scholar-monks formulate the correspondence doctrine, assign each major kami its Buddhist original, staff the shrine-temples with ordained clergy, and adjudicate disputes over ritual precedence. Estate income, offerings, and ritual fees routed through shrine-temple complexes flow into their treasuries, and their transmission lineages define who may interpret the doctrine at all. When the framing stops serving them, their recourse is doctrinal reinvention rather than withdrawal — they can re-describe the relation, as later insiders did when they inverted it.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kenmitsu_temple_establishments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kenmitsu_temple_establishments, beneficiary).

% The noble houses around the throne commission and endow the great shrine-temple complexes, donate estates, and receive in return a unified ceremonial order in which native cult and continental religion legitimate imperial sacrality together. Their ritual calendar, their claim to rule through harmony of gods and Buddhas, and their access to protective rites all presuppose the unified framework; stepping outside it would unsettle the sacral basis of their own position.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, court_ritual_aristocracy, beneficiary,
    powerful, generational, constrained, national).

% Warrior governments patronize temples and shrines, confirm and occasionally seize their estates, and call on both kami and Buddhas for victory and order. They pay in land and privilege; they receive legitimation, administrative partnership, and a religious vocabulary that sanctifies military rule. Patronage is selective and movable, so their commitments track advantage rather than doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kamakura_muromachi_warrior_elite, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kamakura_muromachi_warrior_elite, payer).

% Mountain ascetic orders whose practice openly fuses native deities with Buddhist figures. The unified ontology is the operating licence for their rites, their clientele among villages seeking this-worldly benefits, and their standing in official religion. Their itinerant structure lets them relocate practice, but their ritual identity is constituted by the fusion itself.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, yamabushi_shugendo_orders, beneficiary,
    moderate, biographical, mobile, regional).

% Hereditary custodial lines at major shrines — Kasuga, Hiyoshi, Miwa, the inner precincts of Ise — who once held autonomous ritual authority over their deities. Under the unified doctrine their gods are redescribed as local appearances of Buddhist originals requiring salvific completion, resident clergy take precedence in ritual ordering, and a share of offerings and income is redirected to the associated temples. Office passes by blood; leaving it means abandoning the ancestral service and the deity itself, so their position is fixed by birth and vocation together.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, hereditary_shrine_priesthoods, payer,
    moderate, generational, identity_locked, regional).

% The priestly house of Ise Grand Shrine's outer precinct, which accommodated Buddhist frameworks early, then produced a body of polemical writing asserting the kami's primacy and rejecting their reduction to appearances of Buddhas. Tolerated at the margin because the imperial cult they serve made suppression costly, they kept a counter-tradition alive without displacing the dominant doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, ise_watarai_priest_lineage, payer,
    organized, generational, constrained, regional).

% Village and provincial communities whose festivals are folded into temple calendars, whose local gods receive Buddhist identities and are enrolled in universal salvation schemes, and whose contributions pass through shrine-temple intermediaries. They have no doctrinal voice; their participation is secured by custom and by the presence of the institutions that organize their ritual life.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cult_communities, payer,
    powerless, biographical, trapped, local).

% Edo-period philologists — the Motoori and Hirata lineages among them — who argue from ancient texts that the kami are prior and sovereign and that the Buddhist overlay is contamination to be stripped away. For most of the period they speak from outside the official doctrinal conversation, publishing within limits; their scholarship later arms the state movement that abolishes the unified order.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kokugaku_nativist_scholars, excluded,
    organized, biographical, constrained, national).

% Modern scholars of Japanese religion who reconstruct the medieval settlement's political economy from estate records, doctrinal texts, and institutional histories. They hold no stakes in the arrangement, can compare it with neighboring systems, and supply the evidence base for every other seat's retrospective self-understanding.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, historians_of_japanese_religion, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kenmitsu_temple_establishments).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates two coexisting cult systems into one ritual cosmos: assigns every kami a place in a universal Buddhist soteriology, standardizes festival calendars and ordination across shrines and temples, and settles the metaphysical-status question so court, clergy, and cultivators can coordinate worship without adjudicating pantheon rivalry case by case.
% TRANSFER_FUNCTION: Moves doctrinal jurisdiction, estate income, offering revenues, and salvific-mediation authority from hereditary shrine lines and local cult communities to the Buddhist establishments; moves continental prestige, textual sophistication, and protective legitimation outward to shrine cults and their aristocratic and warrior patrons.
% ABSENT_VOICES: Kami-primacy partisans — shrine purists, the Watarai polemicists working at Ise's margins, and later the Kokugaku philologists — would object that the trace doctrine demotes the native gods; they sat outside the doctrinal councils where correspondences were fixed. Village communities bore the calendar and revenue effects with no seat at all; their interests reached the table only filtered through patron temples.
% DISAPPEARANCE_RATIONALE: At the system's height, overnight removal would unravel the shrine-temple complexes, sever the kami cults from the salvific framework that had absorbed them, and force the court and warrior regimes to rebuild ritual legitimacy from scratch — the medieval settlement was organized around the fusion, not merely decorated with it.
% FOUNDING_PROBLEM: Reconciling an imported universal soteriology with an entrenched native cult: determining what the kami ARE relative to the Buddhas so that foreign doctrine and indigenous worship could proceed together under a single legitimate order.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: academic historians of the kenmitsu order (the Kuroda Toshio school and successors) document the problem's construction and its forcible closure; the Meiji state's own separation edicts attest that the problem was 'solved' by abolishing the fusion; and the nativist (Kokugaku) polemicists attested from outside that the problem admitted resolution only by denying the fusion premise. No surviving institution attests the founding problem as still live in its original form; some philosophers of religion treat the underlying question — how vernacular deity-cults relate to universal frameworks — as permanently recurring, hence contested rather than plainly dead.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope on the merits: the arrangement solved a real integration problem no alternative mechanism addressed (one ritual cosmos for two cult systems), and the same structure transferred jurisdiction, income, and precedence from shrine lines to the monastic complex, held in place by active enforcement (clerical staffing of shrines, doctrinal policing, estate-confirmation politics). Metrics are authored independently of the claim and describe the arrangement's mature operation. Extractiveness 0.66 reflects the composite transfer at its medieval plateau — material revenue, ritual precedence, and the ontological demotion of the kami — with the referent being the standing honji suijaku arrangement itself, assessed by this reading's own lights: even sympathizers inside the tradition (the Watarai polemicists) registered the asymmetry while accepting the ontology. Suppression 0.52 is a raw structural property, unscaled by power or scope: doctrinal monopoly, economically dependent shrine lines, episodic coercion of dissent — part structural, part internalized (subordination preached until believed). Theater 0.38: the integration services were real, but apologetic maintenance grew as nativist critique mounted. Accessibility_collapse 0.5: alternatives (kami-primacy claims, domain-separation framings, purist rejection) narrowed without closing — Ise kept a counter-theology, an insider later inverted the doctrine wholesale, folk dual practice persisted. Resistance 0.6: sustained polemic, foot-dragging, and finally state abolition. Receipt surface: gains demonstrably accrue to the monastic seat, and fixing was prohibitive for any medieval actor — the arrangement underwrote everyone's authority — while the one actor who did fix it paid with nationwide institutional destruction. The measurement series runs on one shared seven-point grid so every metric carries an authored value at every examined year; trajectories show enforcement built to a medieval peak, partially decayed in the wars, stabilized under Tokugawa regulation, and annihilated in 1868 — suppression_requirement falling to 0.09 marks enforcement destruction, not consent. Identity-lock note: the shrine priesthoods' exit is locked less by economics than by vocation — office passes by blood and service to a specific deity constitutes the lineage's identity, so the subordinated seat cannot walk out even where geography would allow. Cyclical dynamics: none; the series is a rise-plateau-collapse arc, not an oscillation, so no intermittent-reinforcement reading applies.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the monastic seat the arrangement is a cosmic order they authored and maintain: integration services rendered, correspondences defended, revenue a fair return on civilization-work. From the hereditary priesthoods' seat the same structure is subordination dressed as metaphysics: their gods redescribed as needing completion, their precedence ceded, their incomes shared — and no exit that does not betray ancestry. The court experiences the arrangement as purchased legitimacy; the warrior elite as a usable sanctifying vocabulary; village communities feel the calendar and the fee schedule with no seat at the doctrinal table. The engine derives these divergent per-seat classifications from power, exit, and directionality data; nothing in the authored claim adjudicates between them. Victim-side coalition potential stayed unrealized: shrine lines were stitched into rival temple patronage networks that cut across their shared interest, the Watarai house fought alone at Ise, and no cross-cutting coalition formed until nativist scholarship armed a state actor eight centuries in — fragmentation, not absence of grievance, kept a three-member victim class individually weak.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map onto real flows. The temple establishments sit nearest the beneficiary pole (override d 0.06): the agenda_setter role alone would drag a derived value toward symmetry because they also labor to maintain the doctrine, but structurally they are the receipt point for jurisdiction, estates, and fees, so the derivation understates their subsidized position. The court sits low (override d 0.14): it pays estates, yet buys the legitimation that secures its own sacrality — a net gain the raw payment flows would underweight. The warrior elite sits mid-low (override d 0.32): a genuine two-way exchange of tribute for legitimation and administrative cover, which a beneficiary-listing-only derivation would score too cheaply. The ascetic orders derive low without help (licensed by the fusion, mobile). On the target side: hereditary priesthoods derive high (victim status plus identity-locked exit, d near 0.85); the Watarai line carries an override upward (d 0.78) because its early partial accommodation of Buddhist elements would soften a derived value below the subordination it actually experienced and polemicized against; village communities sit highest (trapped, voiceless, d near 0.9). Excluded nativist scholars and the analytical observers carry no extraction relationship to the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two symmetrical errors: calling the arrangement pure coordination erases the documented victims (demoted gods, displaced precedence, diverted income); calling it pure extraction erases integration services nothing else provided. On mandatrophy specifically: the founding problem — reconciling universal doctrine with native cult — did not die of disuse; the arrangement was still executing its function when the Meiji state abolished it by force. That is termination-by-conquest, not atrophy: no theatrical husk persisted, no administrator kept the form while abandoning the function. Had 1868 not come, the trajectory pointed toward degradation — theater rising steadily (0.15 to 0.44 across the series), extraction routinized, enforcement habitual — a slow slide toward inertial persistence that the measurement series encodes explicitly. The R5 mismatch machinery should read this story accordingly: a contested founding-problem status paired with a world that genuinely rearranged around the arrangement. The mismatch reflects a state's forcible answer to a question some still count open, not a zombie institution collecting rent past its purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'Which construal of the shinbutsu coexistence commitment does the historical arrangement instantiate — and does this file''s syncretic-fusion premise correctly locate the contest?',
    'Comparative analysis of which structure best predicts practice: whether the correspondence tables functioned as ontological identity claims (this file), as domain-allocation conventions (the domain-partition sibling file), or as deliberately unfalsifiable ambiguity (the incoherent-bundle sibling file).',
    'This file is authored as the syncretic-fusion reading of kernel shinbutsu_coexistence_commitment with its own stable epsilon. Under the domain-partition sibling the victim set thins (shrine cults retain ontological parity) and enforcement needs drop; under the incoherent-bundle sibling there is no single arrangement for epsilon to be about and this file decomposes into unrelated institutional deals. The disagreement is located in the ontological-status claim itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings carried as separate constraint files.').

omega_variable(
    sincere_metaphysics_vs_managed_ambiguity,
    'Was the single-ontology claim sincerely held as metaphysics by its bearers, or maintained as strategically useful ambiguity by the institutions that profited from it?',
    'Uptake analysis: devotional literature, popular practice patterns, and whether internal disputes over kami-Buddha assignments were settled by argument and reported revelation or by institutional fiat and interest.',
    'If managed ambiguity dominated, the coordination-function credit shrinks and effective classification shifts toward pure extraction; if the ontology was widely held across seats, the tangled-rope reading stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_metaphysics_vs_managed_ambiguity, empirical, 'Sincerity of the fusion premise among elites and laity.').

omega_variable(
    extraction_composition_weighting,
    'What did the arrangement principally extract — material revenue, ritual precedence, or the ontological status of the kami — and do these components move together?',
    'Reconstruction of shrine-temple estate and fee flows against close reading of status language in doctrinal texts and shrine petitions.',
    'Material dominance validates the authored extractiveness profile; status dominance raises the priesthoods'' effective burden beyond the material measure and pushes their directionality further toward the target pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_composition_weighting, conceptual, 'Composite extraction: revenue versus precedence versus ontological demotion.').

omega_variable(
    meiji_exogeneity_of_collapse,
    'Did the arrangement end because its kernel failed an internal test, or because an exogenous revolutionary state destroyed a still-operating system?',
    'Late-Tokugawa vitality indicators immediately preceding 1868: temple-registration compliance, shrine-temple finances, doctrinal production volume, elite career investment in the system.',
    'Internal failure would support the incoherent-bundle sibling''s collapse narrative; exogenous destruction means this reading''s constraint never faced a fair test of fit, and the suppression-requirement collapse of 1868 registers conquest rather than consent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_exogeneity_of_collapse, empirical, 'Exogenous versus endogenous termination of the arrangement.').

omega_variable(
    internal_inversion_conventionality,
    'When an insider inverted the doctrine — Buddhas as traces of kami — did that reveal the trace-identity relation as conventional (assignable at will), or did it operate within the same fusion grammar it appeared to reject?',
    'Analysis of whether the inversion presupposes the identity-of-aspects premise (one reality, two aspects, polarity reversible) or abandons that premise entirely.',
    'If the relation is conventional, this reading''s foundational axiom loses its discovery-claim character and the arrangement reads as a constructed constraint benefiting its authors; if the inversion is a grammatical continuation, even the tradition''s internal opposition confirms the fusion framework''s coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_inversion_conventionality, conceptual, 'Status of the intra-traditional inversion of the trace doctrine (Yoshida-style reversal).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 850, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t850, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 850, 0.15).
narrative_ontology:measurement_basis(shin_tr_t850, observed).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1000, 0.18).
narrative_ontology:measurement_basis(shin_tr_t1000, observed).
narrative_ontology:measurement(shin_tr_t1150, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1150, 0.22).
narrative_ontology:measurement_basis(shin_tr_t1150, observed).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1300, 0.28).
narrative_ontology:measurement_basis(shin_tr_t1300, observed).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1500, 0.31).
narrative_ontology:measurement_basis(shin_tr_t1500, observed).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1700, 0.38).
narrative_ontology:measurement_basis(shin_tr_t1700, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1868, 0.44).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t850, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 850, 0.35).
narrative_ontology:measurement_basis(shin_be_t850, observed).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1000, 0.46).
narrative_ontology:measurement_basis(shin_be_t1000, observed).
narrative_ontology:measurement(shin_be_t1150, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1150, 0.57).
narrative_ontology:measurement_basis(shin_be_t1150, observed).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1300, 0.66).
narrative_ontology:measurement_basis(shin_be_t1300, observed).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1500, 0.61).
narrative_ontology:measurement_basis(shin_be_t1500, observed).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1700, 0.63).
narrative_ontology:measurement_basis(shin_be_t1700, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1868, 0.66).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t850, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 850, 0.2).
narrative_ontology:measurement_basis(shin_su_t850, observed).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1000, 0.28).
narrative_ontology:measurement_basis(shin_su_t1000, observed).
narrative_ontology:measurement(shin_su_t1150, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1150, 0.42).
narrative_ontology:measurement_basis(shin_su_t1150, observed).
narrative_ontology:measurement(shin_su_t1300, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1300, 0.55).
narrative_ontology:measurement_basis(shin_su_t1300, observed).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1500, 0.47).
narrative_ontology:measurement_basis(shin_su_t1500, observed).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement_basis(shin_su_t1700, observed).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1868, 0.09).
narrative_ontology:measurement_basis(shin_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu-shugo' covers three structurally distinct claims with different epsilon profiles, authored as separate epsilon-invariant stories per the decomposition principle. This file: single coherent ontology, hierarchical unity, enforced doctrinal consistency. Domain-partition sibling: two existential domains without ontological unification — thinner victim set, lighter enforcement. Incoherent-bundle sibling: no coherent kernel at all — no single arrangement for epsilon to be about. Upstream-downstream texture: this reading's institutional success is the primary evidence the bundle sibling argues against, and its 1868 destruction is the partition sibling's posthumous vindication event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, institutional, 0.06).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, powerful, 0.14).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, organized, 0.32).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
