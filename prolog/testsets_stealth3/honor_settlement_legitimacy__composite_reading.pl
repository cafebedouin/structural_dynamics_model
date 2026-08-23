% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Convergent Suppression of the European Duel — Composite Reading
 *   domain: historical sociology / legal history / cultural anthropology
 *
 * SUMMARY:
 *   This story instantiates the composite reading of the
 *   honor_settlement_legitimacy kernel: the European duel, the formalized
 *   single-combat settlement of disputes of honor among officers and
 *   gentlemen, did not decline because any one force defeated it. Between the
 *   French Revolution's demolition of aristocratic legal privilege and the
 *   aftermath of the Second World War, statutory prohibition, military
 *   disciplinary codes, the state's consolidation of a monopoly on legitimate
 *   violence, the professionalization of armies, the displacement of landed
 *   elites by bourgeois professional classes, and the press publicity that
 *   turned private affairs of honor into scandals all pressed on the practice
 *   at once. The composite reading's distinctive claim is that these
 *   mechanisms were mutually reinforcing and jointly sufficient while none
 *   was singly sufficient, and that the terminal mechanism was cultural
 *   contraction: by the mid-twentieth century the honor frame itself had
 *   dissolved, rendering dueling cognitively unthinkable, so the enforcement
 *   machinery fell silent not because it was repealed but because there was
 *   nothing left for it to reach. The nineteenth-century revivals are the
 *   reading's key evidence: whenever the cultural frame revived, as in
 *   Restoration officer cultures and fin-de-siecle nationalist cults, dueling
 *   resurged under mature legal prohibition, and wherever the frame held, as
 *   in German student corporations, the practice survived bans outright;
 *   after 1918, with the frame gone, practice collapsed even where
 *   enforcement relaxed. The story models the convergent suppression
 *   arrangement across its whole working life: an actively enforced structure
 *   with a real coordination function and real imposed costs, whose end state
 *   is dormant, largely theatrical, and sustained by nobody in particular.
 *   KEY AGENTS (by structural relationship): - national_governments
 *   (institutional/arbitrage): primary agenda setter; legislated prohibition
 *   and owned the courts - general_staffs (institutional/constrained):
 *   enforcement arm inside the services and collector of the
 *   manpower-protection benefit - ecclesiastical_authorities
 *   (institutional/constrained): early condemner with an indirect,
 *   moral-authority stake - honor_class_officers (organized/identity_locked):
 *   principal paying class; bore prosecution, dismissal, and coerced
 *   abandonment of the code constituting their professional selves -
 *   hereditary_aristocracy (powerful/constrained): paying class with
 *   political resources; financed the code's defense -
 *   prosecuted_duel_participants (moderate/trapped): the sharp edge;
 *   individuals courts-martialled, fined, imprisoned -
 *   bourgeois_legal_professions (powerful/mobile): principal beneficiary;
 *   inherited dispute-settlement jurisdiction and its standing -
 *   humanitarian_reform_leagues (organized/mobile): beneficiary; converted
 *   the campaign into organizational capital - plebeian_disputants
 *   (powerless/trapped): excluded; punished more harshly for analogous
 *   violence, never admitted to honorable settlement -
 *   dueling_practitioner_holdouts (powerless/identity_locked): excluded
 *   late-period residualists whose testimony was dismissed as atavism -
 *   comparative_historians (analytical/analytical): analytical observer;
 *   reconstructs mechanism weights from the comparative record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.38).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.71).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.84).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.11).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Convergent Suppression of the European Duel — Composite Reading").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical sociology / legal history / cultural anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '89cef959-e0ed-4434-8388-4524e80f1639').
narrative_ontology:cs_kernel_codification('89cef959-e0ed-4434-8388-4524e80f1639', distributed).
narrative_ontology:cs_authority_grounding('89cef959-e0ed-4434-8388-4524e80f1639', practice).
narrative_ontology:cs_interpretation_layer_present('89cef959-e0ed-4434-8388-4524e80f1639').
narrative_ontology:cs_reading_relation('89cef959-e0ed-4434-8388-4524e80f1639', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('89cef959-e0ed-4434-8388-4524e80f1639', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_axiom('89cef959-e0ed-4434-8388-4524e80f1639', foundational, decline_required_converging_mechanisms).
narrative_ontology:cs_axiom_status(decline_required_converging_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('89cef959-e0ed-4434-8388-4524e80f1639', decline_required_converging_mechanisms, empirically_contingent).
narrative_ontology:cs_axiom('89cef959-e0ed-4434-8388-4524e80f1639', foundational, contraction_edge_is_terminal).
narrative_ontology:cs_axiom_status(contraction_edge_is_terminal, holdable).
narrative_ontology:cs_axiom_grounding('89cef959-e0ed-4434-8388-4524e80f1639', contraction_edge_is_terminal, empirically_contingent).
narrative_ontology:cs_reference_frame('89cef959-e0ed-4434-8388-4524e80f1639', practitioner_consensus_legitimacy).
narrative_ontology:cs_drift_state('89cef959-e0ed-4434-8388-4524e80f1639', post_second_war_extinction, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('89cef959-e0ed-4434-8388-4524e80f1639', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, national_governments).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, general_staffs).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, bourgeois_legal_professions).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, humanitarian_reform_leagues).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_class_officers).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, hereditary_aristocracy).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, prosecuted_duel_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislated the prohibitions, owned the courts that tried dueling cases, and calibrated enforcement through prosecutors and pardons. Collected the order dividend: fewer elite deaths, fewer feud cycles, and uncontested taxation and conscription of the honor classes. Could amend, intensify, or shelve the statutes at will, and did all three across the period.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, national_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Administered discipline inside the services: courts-martial, honor courts, dismissal and non-promotion for duelists. Paid for the machinery in morale friction, since punishing a duel meant punishing courage as the regiments understood it, while collecting the benefit of keeping trained officers alive and command unchallenged.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, general_staffs, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, general_staffs, beneficiary).

% Condemned the practice from pulpit and canon law for centuries before the states acted decisively, withheld burial rites from some who fell, and campaigned alongside the reform leagues. Spent credibility on the campaign, since congregations included the dueling class, and collected moral-authority returns that were real but indirect.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, ecclesiastical_authorities, agenda_setter,
    institutional, civilizational, constrained, continental).

% Commissioned officers raised in codes that made the readiness to stake one's life the proof of fitness for rank. Refusing a challenge ended a career as surely as losing one; accepting one risked death, prosecution, or both. Across the period they absorbed prosecution, dismissal, and the slow conversion of their defining ritual from sacred duty to criminal embarrassment. Leaving the corps was conceivable; leaving the code was not, until the code dissolved around them after the First World War.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, honor_class_officers, payer,
    organized, biographical, identity_locked, continental).

% Landed families whose sons filled the officer corps and whose social season ran on the honor economy. Financed defenses of prosecuted duelists, pressured juries and ministries, and read the campaign against the duel as an attack on their order. Their resources bought delay and exceptions, not survival; the estate economy that paid for the code shrank faster than the code did.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, hereditary_aristocracy, payer,
    powerful, generational, constrained, continental).

% The individuals actually caught: the officer awaiting court-martial, the principal facing assizes, the second charged as accessory. Once charged they had no procedural exit; sympathy moderated sentences more often than law did. Their cases supplied the precedents the enforcement machinery ran on.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, prosecuted_duel_participants, payer,
    moderate, immediate, trapped, national).

% Jurists, barristers, and civil servants whose professions inherited the work the duel used to do: insult, defamation, and redress became pleadings, damages, and regulations. Gained jurisdiction, fees, and the standing of the forum that replaced the field of honor, and opposed the duel in print and parliament with material consistency.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_legal_professions, beneficiary,
    powerful, biographical, mobile, national).

% Anti-dueling societies, evangelical networks, and later socialist and feminist critics who made the practice a standing exhibit of aristocratic barbarism. Built membership, publications, and legislative wins on the campaign, and pivoted to successor causes once the victory was complete.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, humanitarian_reform_leagues, beneficiary,
    organized, generational, mobile, continental).

% Laborers, soldiers, and townsmen who settled their own quarrels with fists and blades. Never admitted to honorable settlement: their killings were murders while their social superiors' were affairs of honor, and they were punished more harshly under the same statutes that rank softened for the gentry. Entirely outside the reform conversation; the campaign spoke about them as beneficiaries and never heard them.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, plebeian_disputants, excluded,
    powerless, immediate, trapped, local).

% Late-period remnants, aging officers, student corporation veterans, and a handful of duelists in fading ceremonial posts, for whom the practice retained its full seriousness decades after their societies had stopped sharing it. Their insistence that the code still bound was received as eccentricity; they kept no institutions capable of reproducing themselves.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, dueling_practitioner_holdouts, excluded,
    powerless, biographical, identity_locked, regional).

% Scholars reconstructing the decline from trial records, army archives, press runs, and memoirs across national cases. Compare jurisdictions where legal pressure matched but the cultural frame differed, which is where the mechanism weights in this story are argued.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidated dispute settlement under central authority: replaced self-executing lethal redress among armed elites with courts, military discipline, and hierarchical petition channels, and protected armies from losing trained officers to private combat.
% TRANSFER_FUNCTION: Moved dispute-settlement jurisdiction and its attached status from the honor classes' self-help institution to state courts and service hierarchies; imposed on honor adherents the costs of surrendering an identity-bearing practice (prosecution, career loss, stigma), while distributing the safety dividend of fewer elite deaths and fewer feud cycles across the whole population.
% ABSENT_VOICES: Two seats were never in the reform conversation: common men who settled disputes with fists and knives and were hanged or transported for violence their social superiors settled by appointment, since the honor settlement's protections and its suppression were both class-specific; and the late-period holdouts for whom the duel retained religious seriousness, whose testimony was filed as atavism rather than counted.
% DISAPPEARANCE_RATIONALE: By 1968 the arrangement is redundant rather than load-bearing: repealing every dead-letter anti-dueling statute overnight would change no observable behavior, because the practice is sustained nowhere by anything the statutes restrain. The composite reading predicts exactly this: the contraction edge, once complete, carries the outcome alone. What would rearrange upon removal is only the archive, not any living practice.
% FOUNDING_PROBLEM: Armed honor elites killed one another and their kin in cycles of private redress: challenge, refusal, feud. Officer corps bled trained men; states could not reliably tax, conscript, or command a class that answered insults with pistols.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the former beneficiary set: comparative historians of the duel and criminologists attest that elite self-help killing vanished with the honor classes rather than being merely suppressed, and military administrative records preserve the manpower-loss motive in the staff colleges' own papers. The honor classes themselves, while they lasted, attested the opposite, namely that no problem existed, which is why corroboration is cited here from academic and administrative outsiders and not from the arrangement's defenders.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).
:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are anchored to the interval end state, matching the terminal measurements, and epsilon's referent throughout is the standing arrangement under contest, the convergent suppression arrangement across its life, as the composite reading assesses it; the endorsed alternative (a rights-respecting order without the campaign's coercions) is not the referent. Base extractiveness 0.38 is the end-state flow, not the working-life peak: the series shows extraction rising to 0.63 by 1899, when enforcement maturity coincided with a still-numerous payer class, then falling as that class collapsed and internalized the new frame. Suppression 0.71 is a raw structural property measuring coercion and lack of alternatives, deliberately NOT scaled by power or scope; it is high at the end because alternatives are closed, even though applied force is low, and the distinction is carried by the separate suppression_requirement series, which is authored precisely because this story tracks enforcement-capacity change: buildup through 1899 (0.42 to 0.75), then decay to 0.21 as contraction made enforcement unnecessary. Theater_ratio ends at 0.61, above the proxy-substitution threshold; this is a symptom of a completed mandate, not evidence of captured maintenance, and the claim below is not tuned to reconcile with it. Accessibility_collapse 0.84 reflects alternatives (secret duels, foreign fields, resignation) that closed progressively and finally motivationally rather than procedurally. Resistance 0.11 is the end-state residue of what was once the constraint's defining feature: the honor classes mounted genuine coalition resistance with organized power, funding defenses, packing juries, and speaking in parliaments, and lost anyway, which is itself the strongest available evidence for the convergence claim. The three series share one nine-point grid (1789-1968) so every metric is authored at every examined time point; the 1815-1870 revival episode is a level shift under tightening enforcement, not an oscillation, so no cyclical battery is warranted.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats experience the arrangement as a completed public reform: governments and staffs paid enforcement costs from general revenues and collected diffuse order dividends, computing something close to a well-run protective institution. The payer seats compute the opposite: from inside the officer corps the arrangement reads as the confiscation of a way of being, since the code was not a habit but the substance of professional identity. Three locks held that seat shut: professional identity (commission and code were one credential), ideology (honor as a worldview in which life-staking proved fitness for rank), and relational identity (regimental brotherhood enforced participation). When the frame broke after 1918, the locks broke with it, and exit appeared where none had existed; a per-seat computation run before that break and after it yields different types for the same nominal agent. A further inversion complicates the payer seat: many adherents privately welcomed release from a status game that had itself coerced them into the field, so part of what the arrangement took away was also a burden it lifted. The excluded seats see a fourth picture: the campaign that civilized the gentry's quarrels hung theirs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: national_governments (arbitrage-grade flexibility over the statutes) sit nearest the beneficiary end; bourgeois_legal_professions and humanitarian_reform_leagues collect without running the machinery. Victim declarations drive high directionalities: prosecuted_duel_participants (trapped, individually charged) sit nearest the full-target end; honor_class_officers (identity_locked) sit near it; hereditary_aristocracy bear real costs but their power and partial exit soften the derivation slightly. No directionality overrides are authored: the structural data suffice for every seat, and the schema keys overrides by power atom rather than by agent, so an override intended for one institutional actor would collide with the other institutional seats sharing that atom. The one derivation strain, general_staffs' dual position as administrator and beneficiary, is handled through its secondary_role rather than an override. The excluded seats fall outside the beneficiary and victim arrays; their position is recorded qualitatively, and the class asymmetry they expose (identical violence punished unequally by rank) belongs to the transfer story rather than to any single agent's directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetric errors. Reading only the end state (theater 0.61, dead-letter statutes, a disappearance verdict of world_unchanged) would invite a zombie reading; but the capture flag keys on founding_problem_status dead combined with world_rearranges, and here the verdict is world_unchanged, so the machinery correctly reports benign obsolescence rather than capture: nobody profits from the dead letters, and repeal is cheap. Reading only the working life would invite a pure-protection reading that erases the prosecuted, the coerced, and the class asymmetry in punishment. The honest structure is a working arrangement with genuine coordination function and real imposed costs, whose mandate outlived its function and is declared resolved. The piton-flavored end state is a phase of this constraint, not its truth; the composite reading expects exactly this profile, since a contraction-dominated decline leaves enforcement standing after the conduct has died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This file instantiates the composite_reading of the honor_settlement_legitimacy kernel; what would the sibling readings (contraction_reading, drop_reading) change structurally, and where exactly is the disagreement located?',
    'Not resolvable by data alone: the readings partition the same historical record by mechanism plurality and terminal-state claim. Adjudication proceeds by comparative-case fit (Mensur survival under bans, the revival episodes, the post-1918 collapse) and by which reading best predicts the timing of enforcement decay relative to frame collapse.',
    'Adopting contraction_reading alone would delete the independent material-suppression edge, lowering mid-interval extraction attribution and concentrating everything on the cultural mechanism. Adopting drop_reading would relocate the surviving arrangement into residual honor communities with persistent small-scale enforcement and a smaller, different victim set. The disagreement is located at mechanism plurality and at the nature of the terminal state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega: this constraint is one reading of a three-reading kernel; the siblings are other files, not parts of this one.').

omega_variable(
    material_mechanism_counterfactual_weight,
    'Would the material and institutional mechanisms (statutes, courts-martial, the violence monopoly, professionalization) have suppressed dueling independently of the cultural contraction, and on what timetable?',
    'Comparative counterfactual cases: jurisdictions with matched legal pressure and divergent cultural frames (German student corporations under repeated bans; Anglophone officers dueling abroad beyond prosecutorial reach); dose-response analysis of enforcement intensity against practice rates within single jurisdictions.',
    'If the material mechanisms were independently sufficient, the composite reading reduces to plural causation with no dominant edge and the enforcement phase carries more of the extractive credit; if insufficient alone, contraction''s terminal dominance is confirmed and the enforcement machinery''s assessment leans harder on its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_mechanism_counterfactual_weight, empirical, 'Counterfactual weight of the material mechanisms absent the contraction edge.').

omega_variable(
    victim_liberation_extraction_inversion,
    'Were honor-class adherents harmed by the suppression arrangement, or liberated from a status game that had itself coerced their participation, and in what proportion?',
    'Revealed-preference evidence: private correspondence and memoirs of officers who welcomed escape from compulsory challenges; refusal and emigration rates; the speed with which the class abandoned the code once social permission arrived.',
    'If net-liberation dominates, payer-seat extraction collapses toward subsidy and the arrangement''s real costs concentrate in the enforcement episodes (prosecutions, dismissals) rather than in the prohibition itself; if net-confiscation dominates, the current moderate extraction estimate stands or rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_liberation_extraction_inversion, conceptual, 'Whether the paying class''s loss was confiscation or release.').

omega_variable(
    suppression_internalization_partition,
    'How much of the end-state closure (no one duels, no one could) is internalized and would survive repeal, versus enforced and revivable if attitudes shifted?',
    'Natural experiments: jurisdictions that repealed or lapsed anti-dueling provisions; the outcomes of interwar nationalist revival attempts measured against elapsed time since frame collapse.',
    'A large internalized share confirms the contraction edge as terminal and the current arrangement as inert obsolescence; a large enforceable share implies latent revival capacity, raising effective suppression and blocking any benign-obsolescence reading of the dead letters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_partition, empirical, 'Partition of end-state suppression into internalized and enforced components.').

omega_variable(
    mensur_survival_case_weight,
    'How much evidential weight does the survival of German student corporate dueling (the Mensur) under repeated legal prohibition carry against the contraction-dominant thesis, given that it persisted precisely where the corporate honor frame remained intact?',
    'Code comparison: determine whether the Mensur is the same constraint as the lethal affair of honor over personal insult or a distinct descendant (ritual scarring, no personal quarrel); trace the corporations'' frame collapse dates against practice collapse dates.',
    'Counted as continuity, the drop_reading gains and contraction''s dominance narrows to the post-1918 civilian elites; classified as a distinct descendant, the case instead corroborates the composite claim that the frame, not the law, was decisive, since the practice died with its frame in both directions, bans notwithstanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mensur_survival_case_weight, empirical, 'The Mensur as the pivotal test case for mechanism weights.').

omega_variable(
    cs_framing_underdetermination,
    'The commitment-system fields characterize the honor culture''s kernel (distributed codification, practice-grounded authority) as this reading construes it; an equally coherent framing characterizes the successor regime''s own legitimating kernel (violence-monopoly doctrine as fixed statutory text under lineage authority). Which framing governs the cs_structure block?',
    'Fix by convention: specify that kernel-reading stories characterize the kernel-under-reading''s commitment system, and require the sibling readings'' files to adopt the same convention so cross-reading comparison remains valid.',
    'Under the successor-kernel framing, kernel_codification flips to fixed_text and authority_grounding to lineage, which changes foreclosure and drift computations and would re-date the axiom_overriding event from the interwar extinction to the Revolutionary decade.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Which side''s kernel the commitment-system fields describe; flagged per CS-framing under-determination guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1789, 1968).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1789, honor_settlement_legitimacy__composite_reading, theater_ratio, 1789, 0.14).
narrative_ontology:measurement(hono_tr_t1815, honor_settlement_legitimacy__composite_reading, theater_ratio, 1815, 0.16).
narrative_ontology:measurement(hono_tr_t1848, honor_settlement_legitimacy__composite_reading, theater_ratio, 1848, 0.19).
narrative_ontology:measurement(hono_tr_t1870, honor_settlement_legitimacy__composite_reading, theater_ratio, 1870, 0.24).
narrative_ontology:measurement(hono_tr_t1899, honor_settlement_legitimacy__composite_reading, theater_ratio, 1899, 0.29).
narrative_ontology:measurement(hono_tr_t1914, honor_settlement_legitimacy__composite_reading, theater_ratio, 1914, 0.33).
narrative_ontology:measurement(hono_tr_t1932, honor_settlement_legitimacy__composite_reading, theater_ratio, 1932, 0.41).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__composite_reading, theater_ratio, 1950, 0.52).
narrative_ontology:measurement(hono_tr_t1968, honor_settlement_legitimacy__composite_reading, theater_ratio, 1968, 0.61).

% Extraction over time
narrative_ontology:measurement(hono_be_t1789, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1789, 0.26).
narrative_ontology:measurement(hono_be_t1815, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1815, 0.34).
narrative_ontology:measurement(hono_be_t1848, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1848, 0.46).
narrative_ontology:measurement(hono_be_t1870, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1870, 0.58).
narrative_ontology:measurement(hono_be_t1899, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1899, 0.63).
narrative_ontology:measurement(hono_be_t1914, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1914, 0.57).
narrative_ontology:measurement(hono_be_t1932, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1932, 0.47).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1950, 0.41).
narrative_ontology:measurement(hono_be_t1968, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1968, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1789, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1789, 0.42).
narrative_ontology:measurement(hono_su_t1815, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1815, 0.48).
narrative_ontology:measurement(hono_su_t1848, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1848, 0.58).
narrative_ontology:measurement(hono_su_t1870, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1870, 0.68).
narrative_ontology:measurement(hono_su_t1899, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1899, 0.75).
narrative_ontology:measurement(hono_su_t1914, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1914, 0.7).
narrative_ontology:measurement(hono_su_t1932, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1932, 0.52).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1950, 0.33).
narrative_ontology:measurement(hono_su_t1968, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1968, 0.21).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1789, tn=1968
narrative_ontology:measurement(hono_grid_01, honor_settlement_legitimacy__composite_reading, accessibility_collapse(class), 1789, 0.33).
narrative_ontology:measurement(hono_grid_02, honor_settlement_legitimacy__composite_reading, accessibility_collapse(class), 1968, 0.87).
narrative_ontology:measurement(hono_grid_03, honor_settlement_legitimacy__composite_reading, accessibility_collapse(individual), 1789, 0.28).
narrative_ontology:measurement(hono_grid_04, honor_settlement_legitimacy__composite_reading, accessibility_collapse(individual), 1968, 0.82).
narrative_ontology:measurement(hono_grid_05, honor_settlement_legitimacy__composite_reading, accessibility_collapse(organizational), 1789, 0.22).
narrative_ontology:measurement(hono_grid_06, honor_settlement_legitimacy__composite_reading, accessibility_collapse(organizational), 1968, 0.78).
narrative_ontology:measurement(hono_grid_07, honor_settlement_legitimacy__composite_reading, accessibility_collapse(structural), 1789, 0.38).
narrative_ontology:measurement(hono_grid_08, honor_settlement_legitimacy__composite_reading, accessibility_collapse(structural), 1968, 0.84).
narrative_ontology:measurement(hono_grid_09, honor_settlement_legitimacy__composite_reading, resistance(class), 1789, 0.67).
narrative_ontology:measurement(hono_grid_10, honor_settlement_legitimacy__composite_reading, resistance(class), 1968, 0.07).
narrative_ontology:measurement(hono_grid_11, honor_settlement_legitimacy__composite_reading, resistance(individual), 1789, 0.56).
narrative_ontology:measurement(hono_grid_12, honor_settlement_legitimacy__composite_reading, resistance(individual), 1968, 0.08).
narrative_ontology:measurement(hono_grid_13, honor_settlement_legitimacy__composite_reading, resistance(organizational), 1789, 0.61).
narrative_ontology:measurement(hono_grid_14, honor_settlement_legitimacy__composite_reading, resistance(organizational), 1968, 0.05).
narrative_ontology:measurement(hono_grid_15, honor_settlement_legitimacy__composite_reading, resistance(structural), 1789, 0.49).
narrative_ontology:measurement(hono_grid_16, honor_settlement_legitimacy__composite_reading, resistance(structural), 1968, 0.11).
narrative_ontology:measurement(hono_grid_17, honor_settlement_legitimacy__composite_reading, stakes_inflation(class), 1789, 0.26).
narrative_ontology:measurement(hono_grid_18, honor_settlement_legitimacy__composite_reading, stakes_inflation(class), 1968, 0.83).
narrative_ontology:measurement(hono_grid_19, honor_settlement_legitimacy__composite_reading, stakes_inflation(individual), 1789, 0.32).
narrative_ontology:measurement(hono_grid_20, honor_settlement_legitimacy__composite_reading, stakes_inflation(individual), 1968, 0.74).
narrative_ontology:measurement(hono_grid_21, honor_settlement_legitimacy__composite_reading, stakes_inflation(organizational), 1789, 0.38).
narrative_ontology:measurement(hono_grid_22, honor_settlement_legitimacy__composite_reading, stakes_inflation(organizational), 1968, 0.69).
narrative_ontology:measurement(hono_grid_23, honor_settlement_legitimacy__composite_reading, stakes_inflation(structural), 1789, 0.44).
narrative_ontology:measurement(hono_grid_24, honor_settlement_legitimacy__composite_reading, stakes_inflation(structural), 1968, 0.79).
narrative_ontology:measurement(hono_grid_25, honor_settlement_legitimacy__composite_reading, suppression(class), 1789, 0.24).
narrative_ontology:measurement(hono_grid_26, honor_settlement_legitimacy__composite_reading, suppression(class), 1968, 0.09).
narrative_ontology:measurement(hono_grid_27, honor_settlement_legitimacy__composite_reading, suppression(individual), 1789, 0.3).
narrative_ontology:measurement(hono_grid_28, honor_settlement_legitimacy__composite_reading, suppression(individual), 1968, 0.14).
narrative_ontology:measurement(hono_grid_29, honor_settlement_legitimacy__composite_reading, suppression(organizational), 1789, 0.34).
narrative_ontology:measurement(hono_grid_30, honor_settlement_legitimacy__composite_reading, suppression(organizational), 1968, 0.18).
narrative_ontology:measurement(hono_grid_31, honor_settlement_legitimacy__composite_reading, suppression(structural), 1789, 0.42).
narrative_ontology:measurement(hono_grid_32, honor_settlement_legitimacy__composite_reading, suppression(structural), 1968, 0.23).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'dueling's decline' decomposes under the epsilon-invariance principle into at least three structurally distinct claims: (i) terminal extinction produced by converging mechanisms with the contraction edge dominant (this file: moderate epsilon, class-wide victim set, enforcement-heavy middle age, dormant theatrical end state); (ii) extinction by cognitive framework transformation alone (contraction_reading: single mechanism, no independent material-suppression edge, lower attributed extraction); (iii) non-extinction, meaning persistence as fringe practice among residual adherents (drop_reading: a surviving small-scale arrangement with persistent local enforcement and a different, smaller victim set). Each claim gets its own epsilon, beneficiaries, victims, and classification; this file links the family. No logical upstream-downstream ordering obtains among the readings: each cites the same comparative record, and their differences are differences of mechanism attribution and terminal-state claim, not of evidence hierarchy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
