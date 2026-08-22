% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Priestly Mediation of Divine Legitimacy (Amun-Ra Polytheistic Order)
 *   domain: religious/political-economic
 *
 * SUMMARY:
 *   In the Amun-polytheistic settlement, divine legitimacy reaches the throne
 *   only through established priestly interpretation: the king acts, and the
 *   colleges — above all the oracle of Amun at Karnak — confirm that the god
 *   willed it. Around this mediation stands a plural cosmos of regionally
 *   rooted deities whose cults are accommodated rather than suppressed, and a
 *   temple economy funded by tax-exempt endowments, corvee labor, and
 *   offering surpluses. This file instantiates ONE reading of the contested
 *   kernel divine_legitimacy_substrate; the atenist_monotheistic_reading
 *   (exclusive revelation, all other gods false) and the
 *   folk_syncretistic_reading (household pragmatism) are separate constraints
 *   with their own epsilon values, linked only through network edges. Per the
 *   epsilon-referent rule, epsilon here measures the standing Amun-order
 *   arrangement as this reading sees it — never the arrangement a sibling
 *   reading would install. Time points are years across the New Kingdom arc:
 *   T0 approximates the elevation of Amun under the early Dynasty 18; T400
 *   approximates the late Dynasty 20, when the High Priest of Amun rules
 *   Upper Egypt in fact if not in title. KEY AGENTS (by structural
 *   relationship): - amun_priesthood_thebes: agenda-setting interpreter
 *   (institutional/identity_locked) — confirms royal acts, administers the
 *   largest estate - temple_estates_economy: primary beneficiary
 *   (institutional/constrained) — holds tax-exempt land, herds, labor -
 *   regional_cult_priesthoods: accommodated secondary beneficiaries
 *   (organized/constrained) - pharaoh: dual-positioned seat
 *   (powerful/trapped) — pays in endowments, receives legitimation -
 *   peasant_laborers: primary bearers of the surplus burden
 *   (powerless/trapped) - provincial_tax_assessors: cost-bearing outsiders
 *   (moderate/excluded) - scribal_recorder_class: analytical observers
 *   (moderate/analytical)
 *
 * KEY AGENTS:
 *   - amun_priesthood_thebes: agenda-setting interpreter (institutional/identity_locked) — issues the validations by which royal acts stand, administers the Karnak estate, collects offering shares; office, lineage, income, and burial provision form one inseparable package
 *   - temple_estates_economy: primary beneficiary (institutional/constrained) — holds tax-exempt farmland, herds, workshops, and labor crews; grows by grant, anchored to cult sites
 *   - regional_cult_priesthoods: accommodated secondary beneficiaries (organized/constrained) — retain local revenues and ritual autonomy under Theban primacy
 *   - pharaoh: dual-positioned seat (powerful/trapped) — pays in endowments, exemptions, and conceded privileges; receives the legitimation that is the throne's only warrant; exit means dissolving kingship itself
 *   - peasant_laborers: primary bearers of the surplus burden (powerless/trapped) — corvee crews, tithes, festival obligations; recourse limited to stoppage and petition
 *   - provincial_tax_assessors: cost-bearing outsiders (moderate/excluded) — absorb quota shortfalls created by each new exemption; memorialize but are never consulted
 *   - scribal_recorder_class: analytical observers (moderate/analytical) — literate archivists who see the full ledger of flows in every direction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.52).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Priestly Mediation of Divine Legitimacy (Amun-Ra Polytheistic Order)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "religious/political-economic").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, 'eb3d19b5-3a24-4dbe-a432-5be136211ddb').
narrative_ontology:cs_kernel_codification('eb3d19b5-3a24-4dbe-a432-5be136211ddb', distributed).
narrative_ontology:cs_authority_grounding('eb3d19b5-3a24-4dbe-a432-5be136211ddb', lineage).
narrative_ontology:cs_interpretation_layer_present('eb3d19b5-3a24-4dbe-a432-5be136211ddb').
narrative_ontology:cs_reading_relation('eb3d19b5-3a24-4dbe-a432-5be136211ddb', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('eb3d19b5-3a24-4dbe-a432-5be136211ddb', divine_legitimacy_substrate__folk_syncretistic_reading, influences).
narrative_ontology:cs_axiom('eb3d19b5-3a24-4dbe-a432-5be136211ddb', foundational, plurality_of_gods_under_amun_primacy).
narrative_ontology:cs_axiom_status(plurality_of_gods_under_amun_primacy, holdable).
narrative_ontology:cs_axiom_grounding('eb3d19b5-3a24-4dbe-a432-5be136211ddb', plurality_of_gods_under_amun_primacy, theological).
narrative_ontology:cs_axiom('eb3d19b5-3a24-4dbe-a432-5be136211ddb', foundational, royal_legitimacy_requires_priestly_validation).
narrative_ontology:cs_axiom_status(royal_legitimacy_requires_priestly_validation, holdable).
narrative_ontology:cs_axiom_grounding('eb3d19b5-3a24-4dbe-a432-5be136211ddb', royal_legitimacy_requires_priestly_validation, conventional).
narrative_ontology:cs_reference_frame('eb3d19b5-3a24-4dbe-a432-5be136211ddb', theban_priestly_mediation_order).
narrative_ontology:cs_drift_state('eb3d19b5-3a24-4dbe-a432-5be136211ddb', late_dynasty_twenty, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb3d19b5-3a24-4dbe-a432-5be136211ddb', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood_thebes).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, temple_estates_economy).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_priesthoods).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, peasant_laborers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_tax_assessors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, maat_cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, royal_divine_sonship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the oracle of Amun at Karnak, maintains the liturgical calendar, and issues the interpretations by which royal acts are confirmed as willed by the god. High offices pass through a small number of Theban families; a priest who abandons the college forfeits rank, estate income, and burial provision together, and there is no comparable standing elsewhere. Collects offering shares and administers the largest temple estate in the country.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood_thebes, agenda_setter,
    institutional, generational, identity_locked, national).

% Holds tax-exempt farmland, herds, workshops, and assigned labor crews across the Nile Valley. Receives grain rents, cattle, and offering surpluses; stores and redistributes them as wages to its own dependents. Endowments are anchored to specific cult sites and cannot be moved or converted; the estate grows chiefly through new royal and private grants.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, temple_estates_economy, beneficiary,
    institutional, generational, constrained, national).

% Staff the provincial temples of Ptah at Memphis, Ra at Heliopolis, Osiris at Abydos, Hathor at Dendera, and local forms of Amun. Retain local revenues and day-to-day ritual autonomy while acknowledging Theban primacy in festival precedence and oracle hierarchy. Their accommodation inside the plural cosmos is what keeps provincial cults cooperative; their revenues are smaller and their titles less portable than the Theban college's.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_priesthoods, beneficiary,
    organized, generational, constrained, regional).

% Rules as the god's son and steward of cosmic order, but every major act — accession, campaign, building, appointment — requires confirmation through priestly interpretation and oracle. Funds endowments, exempts temple land from taxation, and concedes judicial privileges; in return receives the legitimation without which the throne has no warrant. Leaving the arrangement would mean renouncing the basis of kingship itself; the one ruler who attempted to replace it with a different theology lost throne, city, and memory.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer,
    powerful, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary).

% Work temple and crown fields, supply corvee crews for building and quarrying, and deliver tithes and festival obligations from village harvests. When grain allocations run short, as at the royal tomb-workers' village, they stop work and petition — the recorded strikes — but have no standing in the forums where obligations are set. Flight from villages is possible but means abandoning land, family tombs, and legal protection.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, peasant_laborers, payer,
    powerless, immediate, trapped, regional).

% Crown scribes and mayors responsible for delivering quota revenue to the treasury. Each new temple endowment removes land from the assessable base while the quota stays fixed, so shortfalls land on them personally. They memorialize the crown for relief but have no voice in the king-god negotiations that create the exemptions.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_tax_assessors, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_tax_assessors, excluded).

% Trained scribes attached to treasuries, tribunals, and temple archives who copy contracts, oracle records, endowment deeds, and ration lists. They see the full ledger of what flows in every direction and record disputes without deciding them; their analytical position comes from literacy and archival access rather than office.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, scribal_recorder_class, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood_thebes).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one authoritative account of cosmic order (maat) and of the king's place in it; coordinates the festival calendar, succession validation, flood-season ritual timing, and the integration of dozens of regional cults into a single workable theology, so that a large and regionally diverse population can act on shared assumptions about why authority is authority.
% TRANSFER_FUNCTION: Moves grain, livestock, labor, and land from the crown treasury and village households to temple estates and priestly colleges; moves legitimation — confirmation that royal acts are divinely willed — from the interpreting priesthoods to the throne.
% ABSENT_VOICES: Village cultivators who bear the corvee and tithe burden have no seat where obligation levels are set; provincial revenue officers watching the taxable base shrink under each new endowment memorialize but are never consulted; women serve the cults (the God's Wife of Amun) yet doctrinal interpretation remains a male college's prerogative; any would-be interpreter outside the established colleges — a lay prophet, a foreign cult — is by definition illegitimate.
% DISAPPEARANCE_RATIONALE: Without priestly validation the throne loses its warrant overnight: succession becomes openly military, temple estates lose their legal basis and unravel into private holdings, the festival calendar that structures the agricultural year lapses, and regional cults drift apart or into conflict. The political economy of the valley reorganizes around whoever can coerce instead of whoever the god confirms.
% FOUNDING_PROBLEM: Before secular administrative legitimacy existed, a newly unified river-valley state needed a way to make obedience to distant authority feel like participation in cosmic order rather than submission to men — and needed a stable answer to succession, flood failure, and plague that did not depend on any single king's person.
% FOUNDING_PROBLEM_CORROBORATION: Non-Theban sources attest it throughout: royal annals from Memphis and Heliopolis invoke the same validation need, administrative papyri record the transfers the arrangement requires, and the tomb-workers' strike minutes at Deir el-Medina show the governed party treating the obligations as real enough to halt work over. What no source outside the Theban college attests is that Amun's primacy specifically was necessary — provincial cult records accept the problem while disputing the monopoly, which corroborates the founding problem and registers dissent about its current administration.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim is tangled_rope because both halves are structurally present and neither is cover for the other: the mediation genuinely solves a coordination problem (one workable theology for a regionally diverse polity, validated succession, an integrated festival calendar), and the same structure moves a large, rising surplus to identifiable collectors. Enforcement is active rather than self-executing — oracle procedure, post-Amarna heresy prosecution, and archive-backed property law all require continuous institutional work, hence requires_active_enforcement. Metrics are authored independently of the claim: epsilon 0.62 reflects a surplus transfer that is heavy but bounded (the arrangement still delivers the legitimation it charges for); suppression 0.52 reflects enforcement that is real but deliberately lighter than a monopolist's, because the plural cosmos accommodates regional variation instead of extinguishing it; theater_ratio 0.30 reflects ritual that is functional within the belief system with a growing ceremonial surplus; accessibility_collapse 0.40 — alternatives (household rites, provincial emphases, private piety) remain reachable, which is precisely what distinguishes this reading from its exclusive sibling; resistance 0.48 — recorded strikes at the tomb-workers' village, memorializing scribes, occasional royal pushback against further grants. The measurement series share one grid (T0-T400 at 80-year steps) so no metric is sampled against another's gaps. Suppression_requirement is tracked because this story's enforcement history is a genuine dynamic — the post-Amarna prosecution machinery and the expanding oracle-state — not a static backdrop; its rise models enforcement infrastructure maturing and hardening across the interval.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the Theban college's chair the arrangement is the cosmos working: validation is service, collections are offering, and the estate's growth is piety made durable. From the peasant chair the same structure is a queue of obligations ending at a granary door. The pharaoh's seat is genuinely split — early in the interval the exchange favors him (cheap validation, willing grants); by T400 he has granted away so much assessable land that the priesthood he validates out-ranks him in Upper Egypt, and the payer half of his role dominates. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them. Identity-lock note: the Theban college's exit is identity_locked, not merely costly — office, lineage, estate income, and burial provision are one package, and a priest who walks away does not relocate his standing but annihilates it. If that fusion broke (if priestly office became purchasable or portable), the college's directionality would shift toward ordinary beneficiary and its resistance to reform would drop accordingly. Suppression mechanism: the 0.52 scalar mixes structural enforcement (oracle procedure, prosecution, archive-backed property law) with internalized compliance (conviction that obligations maintain maat); the maat_internalization omega carries the split rather than pretending the scalar separates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. The Theban college and the temple estates sit at the beneficiary end (d near 0.05-0.15): they collect and administer. Regional cults sit slightly higher (d approximately 0.2) as accommodated secondary beneficiaries who remit deference and precedence. Peasant laborers derive near the full-target end (d approximately 0.9): powerless, trapped, listed victims. Provincial assessors derive high as well (d approximately 0.75): they bear quota shortfalls with constrained exit. One override is authored: the pharaoh's power atom ('powerful') carries d = 0.58. The raw derivation from his victim listing plus trapped exit would push him toward the full-target end, but that misses the substantial legitimation receipts flowing back to him — he is the arrangement's largest customer as well as its largest payer, and by the interval's end the net position leans toward payer. The override encodes the dual position that the beneficiary/victim arrays alone cannot express. Scope amplification is modest here: the arrangement operates at national scale, where verification of claims is harder and effective extraction scales up somewhat for target-side seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite errors. Reading the arrangement as pure extraction (snare) would erase the coordination function doing real work — validated succession, integrated calendar, regional accommodation — and would predict collapse the record does not show until very late. Reading it as pure coordination (rope) would erase the accumulating transfer visible in the series: extraction climbs from 0.42 to 0.62 across the interval while the founding problem remains live, which is rent accretion on a functioning service, not decay. Mandatrophy is NOT resolved: the founding problem — legitimating authority without purely coercive means — persists throughout, so the arrangement has not outlived its mandate; it has thickened around it. The R5 mismatch consumer should find status=live crossed with verdict=world_rearranges, i.e., no zombie flag; the danger this story flags is accumulation, not obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Is the classification of divine-legitimation-through-priestly-mediation indexical to the amun_polytheistic_reading, or intrinsic to the divine_legitimacy_substrate kernel itself?',
    'Compile the two sibling stories and compare computed types: if the atenist reading computes as snare (concentrated crown beneficiary, violently suppressed alternatives) and the folk reading computes as rope or piton, the kernel''s classification is reading-relative and any cross-kernel verdict must condition on the reading instantiated.',
    'If reading-relative, any verdict about ''Egyptian divine legitimacy'' as such is ill-formed without naming the reading; if intrinsic, the three files should converge despite different beneficiary structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Whether classification travels across sibling readings of the same kernel.').

omega_variable(
    priestly_mediation_necessity,
    'Is priestly mediation a functional requirement of the legitimation problem, or a constructed rent position that a royal bureaucracy could have filled more cheaply?',
    'Compare intervals of strong royal control over priestly appointments with intervals of hereditary entrenchment: if validation quality and political stability hold under royal appointees, the hereditary college is a rent position rather than a functional necessity.',
    'If constructed, the share of the transfer attributable to mediation-as-service shrinks and the rent component grows, raising effective extraction; if necessary, part of the measured transfer is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_mediation_necessity, empirical, 'Functional necessity versus constructed rent in the mediation office.').

omega_variable(
    temple_surplus_capture_rate,
    'What share of net agricultural surplus did temple estates, above all the Karnak complex, actually capture at the interval''s end?',
    'Estate survey papyri of the Wilbour class, tax-exemption decrees, and granary accounts permit direct estimation of exempt acreage and collected rents against estimated gross yield.',
    'A capture rate above roughly a quarter of net surplus would justify raising epsilon toward 0.7+ and strengthen the accumulation reading of the series; a lower rate supports the bounded-transfer reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temple_surplus_capture_rate, empirical, 'Empirical ceiling on temple-economy surplus capture.').

omega_variable(
    pharaoh_net_position_temporal,
    'Does the pharaoh''s net position flip from net beneficiary to net payer across the interval, and where is the crossover?',
    'Reign-by-reign series of endowment grants and exemptions set against the political value of validations received; the crossover should fall near the long reigns of the later New Kingdom.',
    'If the flip is real, the pharaoh seat''s directionality is time-varying and the static override (d = 0.58) is an interval-average compromise; per-seat classifications early and late in the interval would differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaoh_net_position_temporal, empirical, 'Temporal flip in the crown''s net position under the arrangement.').

omega_variable(
    local_cult_absorption_valence,
    'Is the incorporation of regional deities into the Amun-centered cosmos accommodation or absorption by other means?',
    'Track provincial temple revenues and festival precedence before and after incorporation episodes: retained local control and revenue indicate accommodation; revenue redirection to Thebes and title subordination indicate absorption.',
    'If absorption dominates, the suppression scalar understates the arrangement''s suppressive force and the ''accommodates regional variation'' feature is partly cover; if accommodation dominates, the moderate suppression reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_cult_absorption_valence, empirical, 'Valence of regional-cult incorporation for the suppression metric.').

omega_variable(
    maat_internalization,
    'How much of observed compliance with temple obligations rests on enforcement machinery versus internalized conviction that the obligations maintain cosmic order?',
    'Compliance behavior during enforcement lapses (ration-driven strike waves, work stoppages during administrative breakdown): compliance persisting without enforcement indicates internalization; compliance tracking enforcement presence indicates structural dependence.',
    'If internalization carries much of the load, effective suppression exceeds the structural measure and would persist through institutional reform; if enforcement-dominated, reform of the machinery would release the pressure quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maat_internalization, empirical, 'Structural versus internalized share of compliance with temple obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t80, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement(divi_tr_t160, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 160, 0.25).
narrative_ontology:measurement(divi_tr_t240, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 240, 0.27).
narrative_ontology:measurement(divi_tr_t320, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 320, 0.29).
narrative_ontology:measurement(divi_tr_t400, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 400, 0.3).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(divi_be_t80, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 80, 0.46).
narrative_ontology:measurement(divi_be_t160, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 160, 0.5).
narrative_ontology:measurement(divi_be_t240, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 240, 0.55).
narrative_ontology:measurement(divi_be_t320, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 320, 0.59).
narrative_ontology:measurement(divi_be_t400, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 400, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(divi_su_t80, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 80, 0.39).
narrative_ontology:measurement(divi_su_t160, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 160, 0.44).
narrative_ontology:measurement(divi_su_t240, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 240, 0.49).
narrative_ontology:measurement(divi_su_t320, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 320, 0.51).
narrative_ontology:measurement(divi_su_t400, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 400, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Egyptian divine legitimacy' covers three structurally distinct claims with different epsilon values and different beneficiary sets, decomposed per the epsilon-invariance principle into a three-file family: this file (priestly mediation of a plural cosmos — distributed beneficiaries, moderate bounded transfer), the atenist file (sole royal revelation — concentrated crown beneficiary, high suppression), and the folk file (household pragmatism — diffuse beneficiaries, minimal transfer). Edges run from this file to both siblings: the Amun settlement is the long-lived baseline whose resources and precedence the atenist attempt tried to seize and whose categories folk practice inhabits. This reading is historically upstream of the atenist sibling (the attempt presupposed the establishment it attacked) and structurally upstream of the folk sibling (temple calendars and festival circuits shape household practice conditions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__amun_polytheistic_reading, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
