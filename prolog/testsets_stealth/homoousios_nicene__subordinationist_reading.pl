% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios Under the Subordinationist Reading: Derived Being Within Shared Divinity
 *   domain: historical theology/ecclesiastical history/philosophy of religion
 *
 * SUMMARY:
 *   This story instantiates the subordinationist reading of the homoousios
 *   kernel: the claim that the creedal term is compatible with the Son's
 *   deriving his being from the Father — shared divinity, not equality. The
 *   standing arrangement under contest is the fourth-century enforcement
 *   regime around the creed, read from the seat of the communities whose
 *   exegesis the machinery anathematized when they were out of favor and
 *   sheltered when they were in it. The colloquial label 'what Nicaea
 *   decided' decomposes into three structurally distinct constraints (this
 *   reading, the metaphysical-equality reading, the honorific-similarity
 *   reading), each with its own epsilon over the same referent; they are
 *   linked as a constraint family. The claim/metric gap is deliberate: the
 *   reading is CLAIMED as tangled_rope (coordination of a single confession
 *   across divergent exegeses, with asymmetric transfer riding on it) while
 *   the metrics describe substantially extractive, actively enforced
 *   operation whose direction reverses with enforcement phases — the engine
 *   measures that divergence; nothing is reconciled.
 *
 * KEY AGENTS:
 *   - subordinationist_communities: primary beneficiary (organized/constrained) — mass constituency whose exegesis the reading keeps admissible
 *   - semi_arian_moderates: dual-positioned beneficiary-payer (organized/constrained) — sheltered middle, taxed from both flanks
 *   - homoian_episcopal_hierarchy: agenda-setter and collector (institutional/mobile) — administers the formulae, receives vacated sees and patronage
 *   - imperial_doctrinal_administration: agenda-setter (institutional/arbitrage) — enforcement source whose commitment lasts one reign
 *   - pro_nicene_confessors: primary payer (institutional/trapped) — bears exile and coerced subscription, anchored to flocks
 *   - metaphysical_equality_tradition: payer (institutional/identity_locked) — cannot concede compatibility without unraveling its own identity
 *   - modern_historical_theologians: analytical observer (analytical/analytical) — sees the full semantic range from no confessional seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.62).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.55).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios Under the Subordinationist Reading: Derived Being Within Shared Divinity").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical theology/ecclesiastical history/philosophy of religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, '19ab6598-b0a0-4395-9757-a1a7f6fa17f7').
narrative_ontology:cs_kernel_codification('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', fixed_text).
narrative_ontology:cs_authority_grounding('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', distributed).
narrative_ontology:cs_reading_relation('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', foundational, derived_being_compatible_with_consubstantiality).
narrative_ontology:cs_axiom_status(derived_being_compatible_with_consubstantiality, holdable).
narrative_ontology:cs_axiom_grounding('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', derived_being_compatible_with_consubstantiality, theological).
narrative_ontology:cs_axiom('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', secondary, scriptural_witness_governs_conciliar_metaphysics).
narrative_ontology:cs_axiom_status(scriptural_witness_governs_conciliar_metaphysics, holdable).
narrative_ontology:cs_axiom_grounding('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', scriptural_witness_governs_conciliar_metaphysics, theological).
narrative_ontology:cs_reference_frame('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', son_derived_being_within_shared_divinity).
narrative_ontology:cs_drift_state('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', contemporary_historical_scholarship, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('19ab6598-b0a0-4395-9757-a1a7f6fa17f7', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, semi_arian_moderates).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, homoian_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, imperial_doctrinal_administration).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, pro_nicene_confessors).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, metaphysical_equality_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, semi_arian_moderates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congregations and clergy across the eastern empire and later the Gothic kingdoms whose inherited exegesis reads the Father as the fount of the Son's being. The reading keeps their baptismal confession inside the creed's vocabulary; when imperial policy turns against them they lose buildings, offices, and sometimes their homes, and leaving means abandoning the communion of their grandparents.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_communities, beneficiary,
    organized, generational, constrained, continental).

% Homoiousian bishops and teachers who confess a real common essence while insisting the Father is prior as source. The reading shelters their middle position between the equality party and the radicals; they also absorb pressure from both flanks and were compelled to subscribe formulae they did not write whenever a court demanded uniformity.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, semi_arian_moderates, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, semi_arian_moderates, payer).

% Court-aligned bishops who drafted, promulgated, and administered the succession of subordinationist settlements between the Dedication Council of 341 and the fall of Valens. They received vacated sees, imperial patronage, and disciplinary power over non-subscribing colleagues; their standing depended on continued court favor, and they moved between imperial courts as favor shifted.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, homoian_episcopal_hierarchy, agenda_setter,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, homoian_episcopal_hierarchy, beneficiary).

% The emperor and his commissioners, who required a single confession for administrative unity: convening councils, exiling non-subscribers, and reversing alignment when a new reign preferred a different settlement. Constantine recalled Arius, Constantius enforced the court formulae, Gratian and Theodosius withdrew support. For this seat the doctrine is an instrument of governance, and commitment lasts one reign.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, imperial_doctrinal_administration, agenda_setter,
    institutional, immediate, arbitrage, continental).

% Bishops, monks, and their circles (the Athanasius-Hosius-Liberius-Basil line) who hold that the Son is equal in being. They bore repeated exile, confiscation, and coerced subscription attempts. Their sees and flocks anchored them: flight abandoned the communities that looked to them, and signing a subordinationist formula forfeited the very claim they existed to hold.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, pro_nicene_confessors, payer,
    institutional, generational, trapped, continental).

% The theological school centered on Alexandria and carried through the Cappadocians, for whom consubstantiality just is full equality of being. Conceding that the term leaves subordination open would unravel the school's own identity, so its members carry the permanent cost of defending the term's univocity and living with the possibility that their settlement is one interpretation among several.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, metaphysical_equality_tradition, payer,
    institutional, civilizational, identity_locked, continental).

% Scholars of doctrine, from the nineteenth-century histories through the Hanson-Ayres generation, who reconstruct the semantic range of the creedal term from conciliar acta, imperial letters, and surviving fragments. They hold no confessional stake in either settlement and can compare all readings precisely because they belong to none.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, modern_historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, homoian_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_nicene__subordinationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a single creedal vocabulary across communities whose exegesis assigns the Son a derived being: one baptismal confession, one liturgical formula, one standard for recognizing orders and baptisms, so that dioceses from Alexandria to the Danube can acknowledge one another.
% TRANSFER_FUNCTION: Moves interpretive authority and ecclesial office. When the reading is ascendant, authority flows from conciliar metaphysics toward scriptural exegesis and offices flow to subscribing clergy; when the reading is out of favor, the same machinery moves security of office from subordinationist clergy to conforming clergy. Throughout, discretion over the term's meaning migrates toward whoever holds imperial favor.
% ABSENT_VOICES: Radical Anomoian teachers (Eunomius' circle) were excluded even by fellow subordinationists, and the lay congregations whose acclamations decided streets and councils held no formal seat in episcopal deliberation; both stood outside the synodal rooms where the formulae were drafted.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the equality settlement would face no live internal rival and the term would harden toward a single meaning — but the scriptural texts that ground the reading would immediately reopen the question, the Gothic churches organized around the reading would lose their confession, and the modern debates descended from it would lose their genealogy.
% FOUNDING_PROBLEM: How to confess the Son as truly divine — against readings that made him a creature among creatures — without contradicting the scriptural witness to the Father's priority and the Son's derived being.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside all confessional parties: historians of doctrine working from conciliar acta, imperial correspondence, and surviving fragments (the Hanson-Ayres line of scholarship) attest both that the founding problem was real and that the formula's semantics stayed unsettled through the fourth century. No party's self-attestation is relied on.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.62 at interval end) because the reading's enforcement transferred offices, sees, and interpretive authority asymmetrically wherever it held power. Suppression (0.55 scalar) is a raw structural property, unscaled by power or scope — the engine scales only extractiveness; the scalar reflects the enforcement machinery (edicts, exile, coerced subscriptions) at interval end, after the reading lost its enforcement arm. Theater ratio peaks mid-interval (0.36 at T40): councils were convened to manufacture consent — Ariminum's western session produced subscriptions under threat, and contemporaries remarked that the world groaned to find itself Arian — so a growing share of activity was performative ratification of decisions already made by courts. Accessibility collapse is moderate (0.50): once the enforcement regime locked in, alternative readings became costly but the scriptural texts stayed legible and exegesis persisted. Resistance is high (0.70): the controversy itself is the resistance record — decades of synods, the Alexandrian riots, five exiles of Athanasius, and subordinationist persistence among the Goths for centuries after 381. All three series run on one shared nine-point grid (T0-T64, roughly 317-381 CE) so every metric is authored at every examined time point; the shape is a rise-and-fall, not an oscillation: enforcement capacity ratchets up through the Homoian ascendancy and collapses at Constantinople I, which dates the reading's transition from enforced settlement to suppressed persistence.
 *
 * PERSPECTIVAL GAP:
 *   Two institutional seats at nominally equal power compute opposite experiences: the Homoian hierarchy (mobile exit, agenda-setting role) versus the pro-Nicene confessors (trapped exit, payer role) — same episcopal rank, different structural relationship, because exit and role differentiate what the same enforcement event does to each. The identity-locked equality tradition differs from the trapped confessors in kind: the confessors could in principle sign and survive at the cost of their claim; the tradition cannot concede compatibility at all without ceasing to be itself. The imperial seat experiences the entire dispute as administrative friction with arbitrage-grade exit — it can and did switch sides between reigns. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: subordinationist communities gain admissibility for their inherited exegesis; the Homoian hierarchy collects offices and patronage; the imperial administration gains administrative uniformity at negligible doctrinal cost. Payers sit near the target end: pro-Nicene confessors bear the enforcement's direct costs with trapped exit, amplifying their effective extraction; the equality tradition bears the permanent defensive burden with identity-locked exit, sitting nearest the full-target end despite never facing the sword directly. Semi-Arian moderates are the genuinely mixed seat — the reading shelters them and the same machinery forced their subscriptions — which is why they carry a secondary payer role rather than a directionality override: the dual role declaration expresses the mixture without flattening it onto a single derived value.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two mislabels. Reading the story purely from the ascendancy phase (340-378) yields a pure-extraction picture — enforced formulae, exiled dissenters, manufactured consent — and misses the genuine coordination function: one confession spanning communities whose exegeses diverged irreconcilably, which is why the reading survived its own defeat among the Goths for centuries. Reading it purely from the persistence phase yields a benign pluralism picture and misses that its enforcement transferred real goods (sees, offices, authority) asymmetrically throughout. Mandatrophy is NOT declared: the founding problem (confessing full divinity without contradicting the Father's priority) is contested rather than dead — the equality tradition attests it was solved at Constantinople, the subordinationist lineage and the external historiography attest it was not — so the arrangement's function shifted with enforcement direction rather than atrophying behind a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the subordinationist_reading of the homoousios_nicene kernel; what changes structurally if a sibling reading governs instead?',
    'Compare the compiled sibling stories (homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__honorific_similarity_reading): shifts in victim set, beneficiary set, and epsilon locate where the readings actually disagree.',
    'Under the metaphysical_equality_reading the victim set empties of Nicene parties and the enforcement story inverts; under the honorific_similarity_reading the term''s asserted semantic force drops and measured extraction falls with it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    enforcement_phase_flip,
    'Does the constraint carry one stable epsilon across enforcement phases, or does its character depend on whether the reading is ascendant or suppressed?',
    'Phase-resolved analysis of the measurement series: compare extraction directed at Nicene seats during the ascendancy (T24-T56) against extraction directed at subordinationist seats after 381, beyond this story''s interval.',
    'If the phases are weighted separately, the constraint computes nearer a pure-extraction profile during ascendancy and nearer a coordination profile in persistence; a single-phase reading risks dating the type transition wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_phase_flip, empirical, 'Whether the reversal of extraction direction across enforcement phases breaks epsilon stability.').

omega_variable(
    semantic_underdetermination_of_term,
    'Was homoousios ever semantically determinate enough that ''compatible with subordination'' is a deviation rather than a co-original meaning?',
    'Philological reconstruction of the term''s pre-Nicaea usage (the Origenist chain, the Dionysian controversy, the Paul of Samosata condemnation) independent of later polemical framing.',
    'If the term was underdetermined from insertion, the subordinationist reading is not a dissident overlay on a settled truth, and the establishment''s enforcement looks less like defense of a fixed meaning and more like closure of an open one — raising effective extraction for the enforcing seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_underdetermination_of_term, empirical, 'Whether the kernel term was semantically determinate at the moment of its creedal insertion.').

omega_variable(
    identity_lock_reversibility,
    'Is the equality tradition''s inability to concede compatibility a stable identity fusion or a contingent institutional posture?',
    'Observe whether any recognized authority within the tradition concedes compatibility without schism — for instance, the reception of twentieth-century eternal-relations-of-authority proposals inside the equality tradition.',
    'If the lock breaks, the identity-locked payer seat converts toward symmetry, and the constraint''s remaining asymmetry rests only on the enforcement history rather than on the structure of the tradition itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, conceptual, 'Reversibility of the metaphysical-equality tradition''s identity lock against subordinationist compatibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_nicene__subordinationist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(homo_tr_t8, homoousios_nicene__subordinationist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(homo_tr_t16, homoousios_nicene__subordinationist_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(homo_tr_t24, homoousios_nicene__subordinationist_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(homo_tr_t32, homoousios_nicene__subordinationist_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement(homo_tr_t40, homoousios_nicene__subordinationist_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(homo_tr_t48, homoousios_nicene__subordinationist_reading, theater_ratio, 48, 0.34).
narrative_ontology:measurement(homo_tr_t56, homoousios_nicene__subordinationist_reading, theater_ratio, 56, 0.31).
narrative_ontology:measurement(homo_tr_t64, homoousios_nicene__subordinationist_reading, theater_ratio, 64, 0.27).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_nicene__subordinationist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(homo_be_t8, homoousios_nicene__subordinationist_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(homo_be_t16, homoousios_nicene__subordinationist_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(homo_be_t24, homoousios_nicene__subordinationist_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(homo_be_t32, homoousios_nicene__subordinationist_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(homo_be_t40, homoousios_nicene__subordinationist_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(homo_be_t48, homoousios_nicene__subordinationist_reading, base_extractiveness, 48, 0.72).
narrative_ontology:measurement(homo_be_t56, homoousios_nicene__subordinationist_reading, base_extractiveness, 56, 0.74).
narrative_ontology:measurement(homo_be_t64, homoousios_nicene__subordinationist_reading, base_extractiveness, 64, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_nicene__subordinationist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(homo_su_t8, homoousios_nicene__subordinationist_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(homo_su_t16, homoousios_nicene__subordinationist_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(homo_su_t24, homoousios_nicene__subordinationist_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(homo_su_t32, homoousios_nicene__subordinationist_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(homo_su_t40, homoousios_nicene__subordinationist_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(homo_su_t48, homoousios_nicene__subordinationist_reading, suppression_requirement, 48, 0.78).
narrative_ontology:measurement(homo_su_t56, homoousios_nicene__subordinationist_reading, suppression_requirement, 56, 0.8).
narrative_ontology:measurement(homo_su_t64, homoousios_nicene__subordinationist_reading, suppression_requirement, 64, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what Nicaea decided' decomposes into three readings of one kernel, each with its own epsilon over the shared referent (the standing creedal-enforcement arrangement). This reading is historically upstream — pre-Nicene subordinationism is the background against which the equality reading defined itself — while the metaphysical_equality_reading is downstream and institutionally dominant after 381. The honorific_similarity_reading sits adjacent, blurring into this one historically through the Homoiousian middle party. Each member links the others via affects_constraints; orphan stories would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
