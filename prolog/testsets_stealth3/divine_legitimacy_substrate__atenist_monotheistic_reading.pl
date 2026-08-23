% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Atenist Revelatory Monopoly: Sole Divine Legitimacy Through Pharaonic Revelation
 *   domain: religious/political/economic (ancient Near East)
 *
 * SUMMARY:
 *   During roughly seventeen years in the mid-fourteenth century BCE, the
 *   Egyptian state was reorganized around a single doctrinal claim: divine
 *   legitimacy flows solely through the reigning king's revelation of the
 *   sun-disc Aten, the sole existing deity, with all other gods declared
 *   false. The temples of the established gods were closed, the name of Amun
 *   was chiseled out of monuments, the clergy were dispersed or dispossessed,
 *   a purpose-built capital was raised at Akhetaten on corvée labor, and
 *   temple landholdings and revenue streams were redirected to the royal
 *   house and its new foundations. This file instantiates the
 *   atenist_monotheistic_reading of the kernel divine_legitimacy_substrate;
 *   the amun_polytheistic_reading and folk_syncretistic_reading are separate
 *   constraint stories with their own epsilon, beneficiaries, and victims,
 *   linked through the network block. Epsilon's referent here is the standing
 *   Atenist arrangement as it operated (c. 1353-1336 BCE), assessed as the
 *   structure that actually seized, moved, and suppressed - not the
 *   theological order it proclaimed. KEY AGENTS (by structural relationship):
 *   - akhenaten_royal_household: Agenda setter (institutional/arbitrage) -
 *   decrees the doctrine, revises its formula, commands the enforcement
 *   apparatus, receives the transferred revenue - aten_court_priesthood:
 *   Beneficiary (organized/constrained) - collects offices, offerings, and
 *   wages from the new foundations; exists only at the palace's pleasure -
 *   amun_priesthood_of_karnak: Primary target (institutional/identity_locked)
 *   - hereditary clergy of the richest god, dispossessed and proscribed,
 *   unable to exit without self-annihilation - lesser_temple_priesthoods:
 *   Target (organized/constrained) - shrine clergy and temple artisans of the
 *   regional gods - village_practitioners: Target with promised benefits
 *   (powerless/trapped) - households whose protective deities were declared
 *   nonexistent, offered in exchange a direct solar universalism -
 *   royal_tomb_workforce_of_amarna: Target (powerless/trapped) - corvée and
 *   ration-dependent builders of the new capital - foreign_vassal_rulers:
 *   Excluded (powerful/arbitrage) - client kings whose own divine warrants
 *   the doctrine condemns, with no voice at court - memphite_theologians:
 *   Excluded (organized/constrained) - custodians of a rival learned
 *   cosmology, sidelined but preserving texts - retrospective_scholarship:
 *   Analytical observer (analytical/analytical) - reconstructs the full
 *   structure from archaeology, letters, and adversarial retrospective
 *   testimony
 *
 * KEY AGENTS:
 *   - akhenaten_royal_household: agenda setter, institutional power, arbitrage exit, continental scope - sole doctrinal authority and revenue recipient
 *   - aten_court_palace_establishment: beneficiary, organized power, constrained exit, national scope - new clergy and officials dependent on royal favor
 *   - amun_priesthood_of_karnak: payer, institutional power, identity_locked exit, national scope - proscribed hereditary clergy of the dispossessed god
 *   - lesser_temple_priesthoods: payer, organized power, constrained exit, regional scope - dismissed shrine clergy and temple artisans
 *   - village_practitioners: payer (secondary beneficiary), powerless, trapped, local scope - households losing domestic cults
 *   - royal_tomb_workforce_of_amarna: payer, powerless, trapped, local scope - ration-bound construction labor
 *   - foreign_vassal_rulers: excluded, powerful, arbitrage, continental scope - client kings with condemned warrants
 *   - memphite_theologians: excluded, organized, constrained, national scope - rival learned tradition
 *   - retrospective_scholarship: observer, analytical, analytical exit, global scope - sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.74).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.84).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, snare).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Revelatory Monopoly: Sole Divine Legitimacy Through Pharaonic Revelation").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "religious/political/economic (ancient Near East)").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, 'fdc49589-dbac-42c2-8626-499e74971ec2').
narrative_ontology:cs_kernel_codification('fdc49589-dbac-42c2-8626-499e74971ec2', formalized).
narrative_ontology:cs_authority_grounding('fdc49589-dbac-42c2-8626-499e74971ec2', extraction).
narrative_ontology:cs_reading_relation('fdc49589-dbac-42c2-8626-499e74971ec2', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('fdc49589-dbac-42c2-8626-499e74971ec2', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('fdc49589-dbac-42c2-8626-499e74971ec2', foundational, aten_exclusive_deity).
narrative_ontology:cs_axiom_status(aten_exclusive_deity, holdable).
narrative_ontology:cs_axiom_grounding('fdc49589-dbac-42c2-8626-499e74971ec2', aten_exclusive_deity, theological).
narrative_ontology:cs_axiom('fdc49589-dbac-42c2-8626-499e74971ec2', foundational, pharaonic_sole_interpretive_access).
narrative_ontology:cs_axiom_status(pharaonic_sole_interpretive_access, holdable).
narrative_ontology:cs_axiom_grounding('fdc49589-dbac-42c2-8626-499e74971ec2', pharaonic_sole_interpretive_access, conventional).
narrative_ontology:cs_reference_frame('fdc49589-dbac-42c2-8626-499e74971ec2', sole_revelatory_monarchy).
narrative_ontology:cs_drift_state('fdc49589-dbac-42c2-8626-499e74971ec2', reign_end_succession, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fdc49589-dbac-42c2-8626-499e74971ec2', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, akhenaten_royal_household).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_court_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood_of_karnak).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, lesser_temple_priesthoods).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, village_practitioners).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_tomb_workforce_of_amarna).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, village_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reigning king and his household decree the doctrinal name of the sole god, commission and personally revise its written formula (altering the official cartouche mid-reign), order the closure of the other gods' temples and the chiseling-out of the proscribed god's name, divert temple lands and incomes to new royal foundations, and appoint or dismiss the new clergy at will. He commissions the hymns and boundary stelae that state the doctrine. The only limit on revision is his own conviction; the arrangement's lifespan is bounded by his person.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, akhenaten_royal_household, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Newly elevated officials and priests staff the Great Aten Temple and the palace's estates at the new capital, receiving offerings, tithes, and wages disbursed from the palace rather than drawn from hereditary endowments. Rank, office, house plots, and rock-cut tombs exist only within the new foundation. When palace favor moves, position moves with it; there is no independent base to fall back on.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_court_priesthood, beneficiary,
    organized, immediate, constrained, national).

% Hereditary clergy of Egypt's wealthiest god hold offices, estates, festival calendars, and an oracular tradition centered on Karnak. The new doctrine declares their god false: temple gates close, processions and oracular consultations stop, revenues are diverted, and the god's name is gouged from temple walls including within their own titles. Remaining means dispossession and proscription; leaving the office would mean abandoning the estates, lineage standing, and the deity their families have served for generations - exit and self-annihilation are the same act.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood_of_karnak, payer,
    institutional, generational, identity_locked, national).

% Priests, scribes, and temple artisans serving Osiris, Ptah, Hathor, and the regional gods watch shrines shuttered, feast days cancelled, and endowment income stop. Some are redeployed onto Aten construction gangs; most are simply dismissed. Craft and scribal skills travel, but the patronage network that paid for them is gone, and re-employment depends on pleasing the very court that abolished their offices.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, lesser_temple_priesthoods, payer,
    organized, biographical, constrained, regional).

% Households up and down the Nile valley keep domestic shrines and protective figures - Bes, Taweret, Hathor amulets buried with children, painted on bedposts, kept in niches. Official doctrine declares these gods nonexistent, bans their images and festivals, and routes all legitimate worship through a distant purpose-built city and its single mediator. In practice many conceal household figures; attendance at official rites is compulsory in name and patchy in fact. The new teaching also offers what the great temples never did: a sun god whose light falls on every household equally, no priestly toll required - a promise that costs them their familiar protectors in exchange.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, village_practitioners, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, village_practitioners, beneficiary).

% Corvée levies and wage laborers cut the royal tombs into the eastern cliffs and raise the temples and palaces of the new capital, paid in bread and beer from palace stores. Ration dependency binds them to the site; the desert settlement's crowded, hastily furnished burials mark lives spent inside the project with little margin to refuse or leave.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_tomb_workforce_of_amarna, payer,
    powerless, immediate, trapped, local).

% Client kings in Syria and Canaan forward tribute and letters pleading for troops and gold, while continuing their own ancestral cults - cults the new doctrine condemns as worship of what does not exist. Correspondence shows mounting distress going unanswered and several vassals hedging toward Egypt's rivals. They have no channel to press the point at court beyond flattery; their exclusion from the arrangement's legitimacy logic is silent in the record the court keeps.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, foreign_vassal_rulers, excluded,
    powerful, biographical, arbitrage, continental).

% Scribal circles around Memphis maintain the older learned cosmology in which creation proceeds through the god Ptah's heart and tongue - a reflective tradition about divine ordering that the new doctrine displaces without engaging or refuting. They copy, preserve, and quietly adapt their texts through the regime and beyond it; their voice in official legitimation is gone, but their manuscripts outlast everyone involved.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, memphite_theologians, excluded,
    organized, civilizational, constrained, national).

% Later historians, archaeologists, and philologists reconstruct the arrangement from the excavated city, the erased and half-erased monuments, the diplomatic letters, tomb inscriptions, the great hymns, the restoration stelae of the reaction, and king lists that omit the reign entirely. They read the covert objects the court never saw and the adverse testimony the court could not suppress, and see the whole structure including what contemporaries could not say aloud.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, retrospective_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, akhenaten_royal_household).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates all legitimate worship in a single deity with a single human mediator at a single purpose-built center, replacing coordination among many temples, calendars, and cult cities with one royal liturgical schedule; standardizes the doctrinal name and image of the god across the realm.
% TRANSFER_FUNCTION: Moves temple landholdings, endowment income, offering streams, and craft output from the established priesthoods - above all Amun of Karnak - to the royal house and its new foundations; moves corvée labor from local projects to construction and provisioning at Akhetaten; moves interpretive authority from a distributed hereditary clergy to one man; moves the gods' festival economy from processional circulation among the towns to static royal donation.
% ABSENT_VOICES: The proscribed Amun clergy (silenced, their god's name erased), the household worshippers whose protective deities were declared nonexistent, the oracle traditions through which deities had spoken in legitimation disputes, and vassal rulers whose own divine warrants the doctrine condemns. All are absent from the record the court wrote; they speak only through covert objects, adverse retrospective testimony, and the speed of the restoration.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the status quo ante returns within months: Karnak's gates reopen, festival calendars resume, displaced clergy reclaim offices, Thebes repopulates, and legitimacy reverts to plural cultic mediation. This is not hypothetical - it happened: within a few years of the enforcer's death the court itself sponsored restoration, and later dynasties chiseled the reformer's own name out in turn. Every named seat's arrangements depended on the structure's persistence or its abolition; nothing continues as before.
% FOUNDING_PROBLEM: Two descriptions contend. As the court framed it: proclaim a revealed truth - one god, known to and interpretable by one man, ending the multiplicity of false gods. As the disrupted parties frame it: concentrate religious authority, revenue, and labor in the royal house while breaking the wealth and oracular independence of the Amun establishment. The arrangement's design serves both descriptions; the dispute between them is the kernel contest itself.
% FOUNDING_PROBLEM_CORROBORATION: Within the reign, no attestation from outside the benefiting circle survives: the court controlled the monumental record, and dissent left only covert traces. External corroboration is retrospective and adverse: the Restoration Stela of Tutankhamun - a source outside the Atenist court - describes temples fallen waste and gods who ignored the land, attesting the disruption's reality from an enemy witness; the Amarna letters attest vassal distress; later king lists omitting the reign attest its illegitimacy in successor memory; modern economic histories of temple landholding attest the scale of the transfer. No contemporary voice outside the benefiting parties corroborates the court's own framing of the founding problem - that absence is itself signal.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74, the interval-end value the scalar tracks) because the arrangement's operative content was transfer: temple endowments, offering streams, and labor moved from a distributed clerical class to one household, with the doctrinal exclusivity doing the work of justifying the seizure. Suppression is higher still (0.84) because persistence depended entirely on active machinery - temple closures, proscription of the Amun name, erasure campaigns, dispersal of clergy - and almost not at all on voluntary adherence; suppression is authored as a raw structural property and is not scaled by power or scope (the engine scales only extraction, by directionality and scope). Theater ratio reaches 0.50 by interval end: as administrative capacity decayed, production of hymns, stelae, boundary declarations, and spectacle ceremonies expanded - performative devotion inflating atop a shrinking functional base. Accessibility collapse is 0.68: physical alternatives were destroyed in the core (closed temples, cancelled calendars, confiscated images) but cognitive and covert alternatives persisted everywhere, and the imperial periphery was never seriously converted. Resistance is 0.60: open defiance was lethal, so resistance took covert forms (hidden household figures, foot-dragging, vassal hedging, elite silence) plus the decisive counter-move at succession. The measurement series run on ONE shared time grid ({0, 2, 5, 9, 13, 16}) with every tracked metric authored at every point; the suppression_requirement series is authored deliberately because enforcement build-up is the dynamic this story traces (ratchet from initial closures to totalizing proscription), and the trajectory is monotonic - a state-driven enforcement ratchet, not an intermittent-reinforcement cycle. Coalition check: the powerless victim classes (villagers, tomb workers, dismissed minor clergy) shared grievances but had no coordinating institution left - the very temples that organized them were the first things closed - so cross-class coalition was structurally blocked; the only effective coalition formed retrospectively among elites at succession. Claim and metrics are authored independently: snare is my structural judgment (the universal-beneficence theology functions as cover over a wealth-and-authority seizure requiring totalizing suppression), the metrics describe observed operation, and the engine computes per-seat types from the structural data without regard to my reconciliation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically differently. From the throne, the arrangement is revealed duty and rightful concentration: the king experiences no extraction at all - he sits at the beneficiary pole with rule-setting power. From the Karnak clergy's seat, the identical structure is annihilation of a world: office, estate, lineage, and god taken together, with identity_locked exit amplifying experienced extraction toward the full-target pole. Two seats share the institutional power atom yet face opposite directionalities - the differentiation is constraint-specific: the throne controls enforcement while Karnak controls tradition, and the arrangement pitted exactly those two capitals against each other. The court-clergy seat is gilded precarity: near-beneficiary directionality but zero autonomy and immediate-horizon dependence. The villager seat is a double bind: formally a payer whose gods were abolished, yet offered a theology that promises direct universal access without priestly toll - the promised benefit never materialized institutionally, which is why the secondary beneficiary role damps but does not invert their directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: the royal household is declared beneficiary and holds agenda-setter power with arbitrage-grade exit, placing it near d=0.0 (subsidized by the arrangement it writes). The new court establishment collects from it but is wholly dependent, sitting low-d with constrained exit. The Karnak clergy, the lesser priesthoods, the village practitioners, and the tomb workforce are declared victims; their d values rise with proximity to the extraction and fall with exit quality - the identity_locked Karnak seat sits nearest the full-target pole, the mobile-skills-but-no-patronage lesser clergy slightly below, trapped villagers and ration-bound workers near it. Vassal rulers and Memphite theologians are excluded rather than allocated: they bear the doctrine's condemnation without a seat in its economy. I deliberately author NO directionality_overrides: the override mechanism keys on power_atom, and this story contains two opposed institutional seats (throne and Karnak) that no single atom-level override could separate - overriding 'institutional' would distort both. The structural derivation from role plus exit options separates them correctly, which is the derivation chain working as designed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. First, the romantic mislabel: treating the arrangement as a mountain-like eruption of timeless monotheistic truth - but it was contingent, constructed, enforced, and dead within a generation; emerges_naturally stays false and no natural-law claim is made. Second, the mockery mislabel: dismissing it as mere eccentric persecution with no structure - but the receipt surface shows precise capture (gains accrue to a named seat) and the temporal record shows a disciplined enforcement ratchet. On mandatrophy: I declare no residual mandate outliving function, because unlike a piton the arrangement did not atrophy into a maintained shell - it was overthrown whole within years of its enforcer's death. The late-reign theater rise is a pre-collapse symptom, not a zombie phase. The cost asymmetry is instructively inverted from the piton signature: fixing was prohibitive only for the one man fused with the arrangement (his revelation claim WAS the arrangement), and cheap for everyone else - which is exactly why removal happened instantly at succession. Mandatrophy analysis thus explains the lifespan: person-bound mandate, no succession mechanism, prohibitive-fixing cost dying with its holder.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the atenist_monotheistic_reading of the kernel divine_legitimacy_substrate. How would instantiating the sibling readings (amun_polytheistic_reading, folk_syncretistic_reading) change the structural classification of the same substrate?',
    'Compile the sibling stories as separate files and compare computed per-seat classifications. The disagreement between readings is located at a single structural element: WHO holds interpretive authority (sole king vs. priestly college vs. distributed household practice), which determines the entire beneficiary/victim topology.',
    'Under the polytheistic reading the substrate computes nearer a coordination arrangement with distributed clerical rents; under the folk reading it computes as a persistent low-intensity practice layer; only the exclusivity reading yields a capture structure with totalizing suppression. Epsilon is reading-indexed over the fixed referent: same substrate, different epsilon per reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    conviction_vs_rent_founders_motive,
    'Was the arrangement driven by sincere theological conviction (a genuine revelation claim held by its founder) or by rent-seeking against the Amun establishment, whose Karnak economy and oracular independence rivaled the crown?',
    'Compare the timing of doctrinal innovation against the timing of land and revenue transfers; weigh biographical evidence of conviction (hymn composition, early patronage of Aten within Amun precincts before any seizure) against the fiscal record of temple disendowment.',
    'If rent-driven, the capture reading of the arrangement strengthens. If conviction-driven, part of the measured suppression reflects zealous persecution dynamics rather than extraction design, and the excess-over-coordination-cost component splits between predation and ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conviction_vs_rent_founders_motive, empirical, 'Historiographical dispute over the founder''s motive: idealist revolution vs. political-economy seizure.').

omega_variable(
    covert_practice_internalization,
    'How much of the old religious practice survived covertly versus genuinely converting, i.e., did exclusive Atenism internalize anywhere, or did enforcement carry the entire load of conformity?',
    'Distribution analysis of devotional objects (Bes, Taweret, Hathor amulets) in the Amarna workmen''s village and domestic contexts; Deir el-Medina continuity records; the speed and completeness of the restoration after the founder''s death.',
    'Near-total covert survival means suppression was almost purely structural and the arrangement carried no internalized component, explaining instantaneous collapse once enforcement capacity lapsed. Any genuine internalization would predict pockets of persistence, which the record does not show.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_practice_internalization, empirical, 'Structural vs. internalized conformity: whether the prohibition lived in enforcement alone or in belief.').

omega_variable(
    foreclosure_enforcement_asymmetry,
    'This reading doctrinally forecloses both sibling readings, but achieved foreclosure differed radically: complete against the institutional sibling within the Nile valley core, partial against household practice, and marginal in the imperial periphery. Where was foreclosure actually achieved?',
    'Map enforcement incidents and surviving contrary practice geographically and socially: Karnak erasure campaigns vs. village amulet deposits vs. vassal correspondence showing untouched native cults.',
    'Effective extraction concentrated where foreclosure was achieved (the core valley and the dispossessed clergy); measuring on achieved rather than doctrinal foreclosure narrows the constraint''s real radius and raises per-capita intensity in the core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_enforcement_asymmetry, conceptual, 'Gap between doctrinal foreclosure and achieved foreclosure across social levels and geography.').

omega_variable(
    coregency_continuity_contingency,
    'Did late-reign co-regency arrangements (Smenkhkare/Neferneferuaten) aim to perpetuate the arrangement beyond the founder''s person, or was continuity always person-bound and therefore structurally doomed?',
    'Resolution of the coregency evidence: succession stelae, name-form analysis, and the sequence of short reigns between the founder and Tutankhamun.',
    'If a durable succession mechanism was intended and failed accidentally, the collapse is contingency rather than structural brittleness; if no mechanism existed, the arrangement''s person-boundarity is a structural property that explains its lifespan independently of enforcement strength.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coregency_continuity_contingency, empirical, 'Whether the arrangement ever acquired a succession mechanism separating it from the founder''s person.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(divi_tr_t2, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 2, 0.15).
narrative_ontology:measurement(divi_tr_t5, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(divi_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(divi_tr_t13, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 13, 0.4).
narrative_ontology:measurement(divi_tr_t16, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 16, 0.5).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(divi_be_t2, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(divi_be_t5, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(divi_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.72).
narrative_ontology:measurement(divi_be_t13, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 13, 0.78).
narrative_ontology:measurement(divi_be_t16, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 16, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(divi_su_t2, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 2, 0.32).
narrative_ontology:measurement(divi_su_t5, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(divi_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.72).
narrative_ontology:measurement(divi_su_t13, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 13, 0.84).
narrative_ontology:measurement(divi_su_t16, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 16, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, resource_allocation).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'ancient Egyptian divine legitimacy'. The single label conflates three structurally distinct arrangements sharing one kernel: the incumbent priestly-mediation establishment (upstream; established coordination with distributed clerical rents; its infrastructure is what this reading dismantled and what the restoration rebuilt), the royal revelatory monopoly (this file; concentrated capture, totalizing suppression, seventeen-year life), and the distributed household-practice substrate (downstream; persists beneath every doctrinal regime and outlived both). Each member links to the others via affects_constraints; epsilon is authored per reading over the fixed referent and differs sharply across the family - low for the polytheistic establishment as its adherents experience it, high (0.74) for the Atenist arrangement as operated here, low for folk practice. Upstream/downstream citation structure mirrors BGS: the established reading's longevity was cited as self-evidence by all parties; this reading's novelty was justified against it; the folk substrate silently carried continuity that made the restoration instant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
