% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Legal Personhood Boundary — Functional Capacity Reading
 *   domain: legal/political
 *
 * SUMMARY:
 *   The legal personhood boundary allocates the most consequential status in
 *   private law: who may bear rights and duties, and who may be owned. This
 *   file instantiates the functional-capacity READING of that boundary — the
 *   position that standing follows demonstrated cognitive capacity
 *   (rationality, sentience, self-awareness) regardless of species. Per the
 *   kernel-reading epsilon rule, the referent of every metric here is the
 *   STANDING ARRANGEMENT under contest — the current species-and-birth-based
 *   allocation — assessed by this reading's own lights, which is why epsilon
 *   is authored high: this seat sees the near-total expropriation of
 *   demonstrably sentient beings, licensed by the boundary and collected by
 *   identifiable industries. The reading's own endorsed alternative
 *   (capacity-indexed standing) is NOT the referent and contributes nothing
 *   to the scores. Ecosystems are deliberately NOT represented: a collective
 *   entity has no single demonstrable cognitive capacity, so admitting them
 *   would blur this reading's criterion and violate epsilon invariance;
 *   prospective artificial systems are routed to an omega rather than
 *   fabricated as a present party. KEY AGENTS (by structural relationship): -
 *   legal_system_institutions: agenda-setting administrator
 *   (institutional/constrained) — administers and defends the line -
 *   commercial_animal_use_industries: primary collector
 *   (powerful/constrained) — converts thing-status into revenue -
 *   born_human_persons: class-wide beneficiary (organized/identity_locked) —
 *   holds personhood unconditionally - humans_without_demonstrable_capacity:
 *   over-included beneficiary (powerless/trapped) — secured by the standing
 *   line, exposed by the reading - sentient_nonhuman_animals: primary target
 *   (powerless/trapped) — capacity-bearing, classified as property -
 *   mass_farmed_animals: mass target (powerless/trapped) — bears the largest
 *   single flow - capacity_evidence_producers: excluded evidentiary seat
 *   (moderate/mobile) — produces what adjudication declines to weigh -
 *   jurisdictional_comparativists: analytical observer
 *   (analytical/analytical) — maps where the line bends
 *
 * KEY AGENTS:
 *   - legal_system_institutions — agenda_setter (institutional/constrained): administers the boundary through courts, statutes, and licensing; revision possible but precedent-bound
 *   - commercial_animal_use_industries — beneficiary (powerful/constrained): agriculture, research, and entertainment operators whose asset base requires animals to remain ownable
 *   - born_human_persons — beneficiary (organized/identity_locked): the class that receives unconditional standing; collectively sets the boundary's terms
 *   - humans_without_demonstrable_capacity — beneficiary (powerless/trapped): protected today by the species line; their status is the reading's hardest edge
 *   - sentient_nonhuman_animals — payer (powerless/trapped): great apes, cetaceans, elephants; documented capacities, thing-status under law
 *   - mass_farmed_animals — payer (powerless/trapped): tens of billions annually; the largest extraction flow the boundary licenses
 *   - capacity_evidence_producers — excluded (moderate/mobile): scientists and litigators whose capacity record adjudication refuses to admit
 *   - jurisdictional_comparativists — observer (analytical/analytical): track the cross-jurisdictional variance of the line
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.82).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.83).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary — Functional Capacity Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal/political").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '57047397-59d2-4956-84be-79b2f39e9150').
narrative_ontology:cs_kernel_codification('57047397-59d2-4956-84be-79b2f39e9150', fixed_text).
narrative_ontology:cs_authority_grounding('57047397-59d2-4956-84be-79b2f39e9150', lineage).
narrative_ontology:cs_interpretation_layer_present('57047397-59d2-4956-84be-79b2f39e9150').
narrative_ontology:cs_reading_relation('57047397-59d2-4956-84be-79b2f39e9150', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('57047397-59d2-4956-84be-79b2f39e9150', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_axiom('57047397-59d2-4956-84be-79b2f39e9150', foundational, moral_standing_tracks_demonstrated_capacity).
narrative_ontology:cs_axiom_status(moral_standing_tracks_demonstrated_capacity, holdable).
narrative_ontology:cs_axiom_grounding('57047397-59d2-4956-84be-79b2f39e9150', moral_standing_tracks_demonstrated_capacity, deontological).
narrative_ontology:cs_axiom('57047397-59d2-4956-84be-79b2f39e9150', foundational, species_membership_has_zero_standing_weight).
narrative_ontology:cs_axiom_status(species_membership_has_zero_standing_weight, holdable).
narrative_ontology:cs_axiom_grounding('57047397-59d2-4956-84be-79b2f39e9150', species_membership_has_zero_standing_weight, deontological).
narrative_ontology:cs_reference_frame('57047397-59d2-4956-84be-79b2f39e9150', demonstrated_capacity_standing_order).
narrative_ontology:cs_drift_state('57047397-59d2-4956-84be-79b2f39e9150', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('57047397-59d2-4956-84be-79b2f39e9150', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, born_human_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, humans_without_demonstrable_capacity).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, commercial_animal_use_industries).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, sentient_nonhuman_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, mass_farmed_animals).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, species_bound_personhood_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, animals_as_property_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts hear and decide who may hold rights and who may be owned; legislatures write the statutes that fix the categories; agencies register, inspect, and license. The boundary reaches them as inherited doctrine carried in constitutions, codes, and centuries of precedent. Moving the line is possible in principle — a statute or landmark ruling could redraw it — but each institution acts incrementally under precedent, and no single office owns the whole line.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_system_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Intensive agriculture, biomedical research, pharmaceutical testing, zoos, aquaria, and entertainment operations hold non-human animals as inventory, equipment, or breeding stock. Valuations, insurance contracts, depreciation schedules, and disposal decisions all assume the animals held are ownable assets. Capital is sunk in facilities and bloodlines that pay out only while the classification holds, so the sector funds lobbying, litigation, and model legislation aimed at keeping the categories stable rather than exiting them.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, commercial_animal_use_industries, beneficiary,
    powerful, biographical, constrained, global).

% Every born human enters the legal world as a rights-holder — able to own, inherit, marry, contract, sue, and claim protection — without any individual test of capacity. The class as a whole holds overwhelming collective power over the boundary's terms through voting, markets, and culture, though almost no individual experiences the arrangement as a choice they made; membership is simply what they are.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, born_human_persons, beneficiary,
    organized, generational, identity_locked, global).

% Newborns before language, people with profound congenital impairments, and those in permanent vegetative states hold full legal personhood today without ever demonstrating the capacities this debate turns on. Guardians, families, clinicians, and courts speak and decide on their behalf. Their position is secure under the present line, and every proposal tying standing to demonstrated performance places their status back on the table — which is why their representatives watch capacity-based proposals with more alarm than any industry does.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, humans_without_demonstrable_capacity, beneficiary,
    powerless, biographical, trapped, global).

% Great apes, dolphins and whales, elephants, magpies, and similar animals pass mirror tests, plan ahead, grieve, use tools, and maintain traditions — capacities documented across decades of field and laboratory work. Law classifies all of them as property: they cannot own, contract, testify, or petition; their custody, breeding, transfer, and killing are decisions their owners make. Physical escape ends at enclosure walls or habitat edges, and repeated habeas-style attempts to open a legal channel out of thing-status have been rejected on doctrinal grounds.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_nonhuman_animals, payer,
    powerless, biographical, trapped, global).

% Tens of billions of pigs, cattle, chickens, and fish pass through intensive production each year — bred by selection programs, housed by design specifications, transported and slaughtered on schedules set in contracts they cannot be party to. Individual lifespans run from weeks to months. Their interests reach the legal system only indirectly, through welfare statutes that regulate how owners may treat their property.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, mass_farmed_animals, payer,
    powerless, immediate, trapped, global).

% Comparative psychologists, ethologists, neuroscientists, and animal-law litigators produce the record — mirror studies, spindle-neuron anatomy, communication decoding, habeas filings — that a capacity criterion would treat as decisive. Courts have consistently ruled such evidence categorically irrelevant to personhood questions as a matter of doctrine, so the producers publish, testify, and litigate from outside the adjudicative room, seeking admission to a hearing that keeps declining to weigh their exhibit.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, capacity_evidence_producers, excluded,
    moderate, biographical, mobile, global).

% Legal scholars track the places the line has bent — parliamentary declarations concerning great apes, national animal-sentience statutes, a contested habeas ruling for a captive orangutan — against the places it has held, mapping which arguments moved which institutions and which met categorical refusal. They take no side administratively; their product is the comparative map itself.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, jurisdictional_comparativists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__functional_capacity_reading, commercial_animal_use_industries).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__functional_capacity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single stable criterion separating entities that can bear rights and duties from entities that can be owned, transferred, modified, and destroyed at an owner's discretion — letting property law, liability, inheritance, contract, and clinical decision-making operate at scale without relitigating the person/thing line transaction by transaction.
% TRANSFER_FUNCTION: Moves liberty, bodily integrity, reproductive control, social bonds, and life itself from beings classified as things (all non-human animals) into the human economy as meat, labor, data, spectacle, and research material; simultaneously confers unconditional legal standing on every born human regardless of individual capacity.
% ABSENT_VOICES: The subjects themselves are absent by construction: non-human animals cannot testify, and the legal channel that could substitute for voice (habeas petitions) has been closed on doctrinal grounds. Capacity scientists and litigators stand outside the adjudicative forum their evidence concerns. Prospective artificial systems have no seat and no advocate of record.
% DISAPPEARANCE_RATIONALE: If the species-boundary allocation vanished overnight, property law would lose its object classes wholesale — no livestock collateral, no research-animal procurement, no pet ownership as currently structured — while food systems, medicine, and biomedical research would reorganize around whatever successor criterion replaced it. Billions of beings would change legal category at once; nothing about the current arrangement survives neutral deletion.
% FOUNDING_PROBLEM: Early law needed a workable line between entities that could owe and be owed obligations and entities that could be owned and used — a bright line for property, contract, liability, and inheritance. Theology and philosophy supplied species-membership-plus-rationality as the available criterion, and law codified it.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problem — that some criterion is needed — is attested by every participant including this reading's opponents; what is contested is whether the founding answer (species membership) remains adequate. Corroboration from outside the benefiting parties: constitutional scholarship on the boundary's doctrinal history, government-commissioned sentience reviews (cephalopod and decapod assessments in the UK framework), and cross-jurisdictional legislative movement all treat the criterion question as open. The using industries attest it is settled — an attestation from inside the beneficiary set, weighed accordingly.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.82 at interval end) because the transfer this seat observes is total within its domain: liberty, reproduction, bodily integrity, and life itself move from classified-things to the human economy, with the rate set entirely by the receiving side. Suppression is higher still (0.83) because persistence depends on actively closing recognition channels — habeas petitions rejected as doctrinally inadmissible, ag-gag statutes criminalizing documentation, sentience language preempted in model legislation — not on voluntary acceptance. Theater is 0.50: as capacity evidence accumulated, the boundary's stated justifications (rationality, dignity, unique human mind) became unsupportable as descriptions, and an increasing share of defensive activity shifted to procedural and economic arguments ('settled law', 'administrative certainty') that do not attempt descriptive truth — classic Goodhart drift of justification away from function, visible in the rising theater series. Enforcement intensification is the traced dynamic, hence suppression_requirement carries the series: doctrinal fortification hardened in response to litigation pressure across the interval. Accessibility_collapse is moderate (0.60) — the dichotomy is nearly inescapable inside any single legal system, but not a natural law: several jurisdictions have bent it at the margins, proving alternatives remain live. Resistance (0.62) reflects a maturing animal-law movement with real wins at the edges. Coordination type is declared identity_coordination: the boundary maintains membership in the legal-moral community. The gaming risk flagged for this type applies squarely here — 'this is who we are' identity framing is the boundary's principal cover story, and the coupling shape (extraction concentrated on powerless agents at global scope) is exactly the pattern the complexity offset must not excuse. All three series share one time grid; the 2025 endpoints equal the base_properties scalars by construction.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently and should. The agenda-setter seat experiences the boundary as load-bearing infrastructure: courts collect no commission from it, absorb real enforcement costs, and see chiefly the coordination function — from inside, the arrangement computes rope-flavored. The industry seat sees indispensable legal certainty protecting an asset base — pure benefit. The payer seats experience the same structure as total appropriation with zero exit — snare-shaped from inside. The excluded evidentiary seat experiences something neither of those: suppression of its input, the enforcement object being precisely the evidence it produces. The over-included beneficiary seat (humans_without_demonstrable_capacity) diverges ACROSS arrangements rather than within one: secured under the standing line, exposed under the reading's endorsed alternative — which is why this story's omegas treat their entailment as unresolved rather than assumed. Coalition dynamics deserve explicit note: the non-human payers' powerlessness is partly constitutive, not circumstantial — lacking standing, they lack every vehicle (litigation, voting, contracting) through which coalition power forms, so the usual remedy for powerless-agent extraction is structurally unavailable to them. Identity-lock dynamics bind the born-human seat: species membership is constitutive identity ('this is what we are'), making the arrangement invisible as a choice; if that identity frame broke — if membership came to be seen as assigned by criterion rather than conferred by birth — the beneficiary seat's resistance to revision would drop sharply and the classification landscape would move with it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation. commercial_animal_use_industries and born_human_persons are declared beneficiaries: the boundary subsidizes them (thing-status liquidity for one, unconditional standing for the other), so their d sits near the beneficiary end and effective extraction inverts toward subsidy. humans_without_demonstrable_capacity is also declared a beneficiary of the standing arrangement — over-inclusion is protection, not extraction, under this referent. sentient_nonhuman_animals and mass_farmed_animals are declared victims with trapped exit and no power: they sit at the full-target end, where effective extraction amplifies to its ceiling. legal_system_institutions are agenda-setters rather than collectors: they spend enforcement resources and gain doctrinal authority, landing mid-range-low rather than at either pole. capacity_evidence_producers bear suppression of their input without material extraction — mild target-side displacement, mid-range. No directionality overrides are needed: role plus exit plus power differentiate every seat the derivation would otherwise conflate (note the same-power-atom contrast that role resolves: powerless beneficiaries and powerless payers coexist in this story, separated by the declaration layer, not by power).
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is resolved here: the founding problem — that any legal order needs a person/thing criterion — remains live even as the founding answer is contested, so status is authored 'contested' rather than 'dead'. The classification discipline earns its keep in both error directions. Calling the standing arrangement a snare would erase the genuine coordination function any successor must replicate (property law, liability, clinical authority all need the line somewhere); calling it a rope would erase the asymmetric extraction and active enforcement this seat documents. Tangled rope holds both halves: real coordination, real victims, enforcement required. The rising theater series is the early-warning signature to watch — it indicates justification drift (the stated reasons no longer describe the operation) without indicating functional atrophy; if theater continued climbing while extraction fell, the structure would be migrating toward piton territory (defended by habit rather than interest), which the temporal record does not yet show.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint is the functional-capacity reading of the legal_personhood_boundary kernel; how would the sibling readings restructure the victim and beneficiary partitions?',
    'Comparative compilation of the three sibling stories: the developmental_potentiality_reading extends the beneficiary set backward to conception (expanding protected class to embryos and fetuses while keeping species privilege); the restrictive_anthropocentric_reading retains species limitation while adding an individual-capacity filter. The disagreement is located entirely in the criterion clause — origin versus birth-plus-capacity versus demonstrated capacity.',
    'Each sibling produces a different victim set and a different directionality profile; classifications computed on this file are valid only for the capacity criterion, not for the boundary as such.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Kernel-level partition differences across the three readings of the personhood boundary.').

omega_variable(
    capacity_threshold_placement,
    'Where exactly does ''demonstrable'' sit — which tests, at what confidence, applied at what age or state?',
    'Deliberate criterion specification: validated behavioral and anatomical markers, margin-of-error handling, and treatment of marginal performers (infants, advanced dementia, locked-in states).',
    'Threshold placement swings the partition dramatically: a strict threshold excludes human infants and severely impaired humans alongside most animals; a generous one admits a wide phylogenetic range. Every downstream classification inherits this placement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_threshold_placement, conceptual, 'Operational indeterminacy of the demonstrability threshold inside this reading''s own criterion.').

omega_variable(
    impaired_human_entailment,
    'Does the capacity criterion entail withdrawing standing from humans who never demonstrate or irreversibly lose capacity, or does the reading admit graduated or guardianship-mediated statuses?',
    'Internal theoretical development: whether the reading''s proponents construct sliding-scale standing, proxy-rights structures, or accept binary exclusion; observe which position the animal-law and disability-rights literatures converge on.',
    'If exclusion is entailed, the reading faces a coalition-killing objection and its effective resistance rises sharply; if graduated statuses are admissible, the reading''s victim set stays non-human and its political viability improves substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impaired_human_entailment, conceptual, 'Whether this reading necessarily unsettles the standing of capacity-less humans — the pivot on which sibling attacks turn.').

omega_variable(
    machine_sentience_admission,
    'When an artificial system demonstrates sentience and self-awareness by whatever tests the criterion adopts, does the reading''s species-neutrality commit it to personhood for that system?',
    'Eventual empirical: detection of machine candidates passing agreed capacity batteries; near-term conceptual: whether the reading''s theorists pre-commit to admission or introduce substrate carve-outs (which would contradict the species-neutrality axiom).',
    'Admission is the consistent application of this reading''s foundational axiom and would expand the victim set catastrophically fast if substrate carve-outs are refused; refusal breaks the reading''s own neutrality principle and hands the restrictive sibling its strongest argument.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(machine_sentience_admission, empirical, 'Prospective extension of the victim set to artificial systems under the capacity criterion.').

omega_variable(
    boundary_naturalization,
    'Is the species line experienced by its holders as a natural fact (a discovered feature of the moral order) or as a constructed convention awaiting revision?',
    'Discourse analysis of legal and popular justifications: appeals to inherent human dignity and the natural order indicate naturalization; appeals to legal certainty and administrative convenience indicate acknowledged construction.',
    'Naturalized presentation suppresses perceived alternatives and dampens resistance measurement; if the line is widely recognized as constructed, accessibility_collapse falls and reform pressure compounds faster than the scalar series records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_naturalization, conceptual, 'Whether the boundary''s quasi-natural self-presentation is believed or merely deployed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1975, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement_basis(lega_tr_t1975, observed).
narrative_ontology:measurement(lega_tr_t1985, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1985, 0.33).
narrative_ontology:measurement_basis(lega_tr_t1985, observed).
narrative_ontology:measurement(lega_tr_t1995, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1995, 0.37).
narrative_ontology:measurement_basis(lega_tr_t1995, observed).
narrative_ontology:measurement(lega_tr_t2005, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement_basis(lega_tr_t2005, observed).
narrative_ontology:measurement(lega_tr_t2015, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2015, 0.46).
narrative_ontology:measurement_basis(lega_tr_t2015, observed).
narrative_ontology:measurement(lega_tr_t2025, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2025, 0.5).
narrative_ontology:measurement_basis(lega_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(lega_be_t1975, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement_basis(lega_be_t1975, observed).
narrative_ontology:measurement(lega_be_t1985, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1985, 0.64).
narrative_ontology:measurement_basis(lega_be_t1985, observed).
narrative_ontology:measurement(lega_be_t1995, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1995, 0.69).
narrative_ontology:measurement_basis(lega_be_t1995, observed).
narrative_ontology:measurement(lega_be_t2005, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2005, 0.73).
narrative_ontology:measurement_basis(lega_be_t2005, observed).
narrative_ontology:measurement(lega_be_t2015, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement_basis(lega_be_t2015, observed).
narrative_ontology:measurement(lega_be_t2025, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2025, 0.82).
narrative_ontology:measurement_basis(lega_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1975, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement_basis(lega_su_t1975, observed).
narrative_ontology:measurement(lega_su_t1985, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1985, 0.61).
narrative_ontology:measurement_basis(lega_su_t1985, observed).
narrative_ontology:measurement(lega_su_t1995, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1995, 0.66).
narrative_ontology:measurement_basis(lega_su_t1995, observed).
narrative_ontology:measurement(lega_su_t2005, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement_basis(lega_su_t2005, observed).
narrative_ontology:measurement(lega_su_t2015, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement_basis(lega_su_t2015, observed).
narrative_ontology:measurement(lega_su_t2025, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2025, 0.83).
narrative_ontology:measurement_basis(lega_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'legal personhood'. The label conflates three structurally distinct claims about WHERE standing begins: at human conception (developmental_potentiality_reading), at birth within the species subject to an individual-capacity filter (restrictive_anthropocentric_reading), and at demonstrated cognitive capacity regardless of species (this file). Each reading yields a different victim set, a different epsilon over the same standing arrangement, and different failure modes; per the epsilon-invariance principle they are modeled as three linked stories, not one story with a measurement parameter. This file links to both siblings; upstream/downstream ordering is symmetric-contest rather than evidentiary, since each reading cites the boundary's own text as authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
