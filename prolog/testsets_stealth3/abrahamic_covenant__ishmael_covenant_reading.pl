% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__ishmael_covenant_reading, []).

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
 *   constraint_id: abrahamic_covenant__ishmael_covenant_reading
 *   human_readable: Ishmael-Line Covenant Reading of the Abrahamic Promise
 *   domain: religious/comparative-theology/institutional-authority
 *
 * SUMMARY:
 *   Within the Islamic tradition the Abrahamic covenant is read as an
 *   inclusive promise: God's pledge to Abraham runs through all his lines,
 *   with Ishmael — elder son, co-builder of the Meccan sanctuary, ancestor of
 *   the Arabs — carrying the prophetic line that culminates in Muhammad.
 *   Genesis's apparently exclusive statements (covenant 'established with
 *   Isaac'; 'through Isaac shall your offspring be named') are reread as
 *   marking one thread of a wider promise rather than its limit, with the
 *   Qur'an supplying the authoritative retelling that restores the inclusive
 *   scope. This file authors ONE reading of the shared abrahamic_covenant
 *   kernel: the ishmael_covenant_reading. The sibling readings —
 *   isaac_covenant_reading (transmission exclusively through Isaac) and
 *   christian_supersessionist_reading (covenant fulfilled and transferred to
 *   the Church) — are separate constraints with their own beneficiary sets,
 *   epsilon values, and types; they are linked, not averaged, here. The
 *   epsilon referent is the standing arrangement this reading itself
 *   institutes and administers — covenant-membership allocation under the
 *   Ishmael-succession claim — assessed by the reading's own lights.
 *
 * KEY AGENTS:
 *   - islamic_religious_authorities: agenda-setting custodians (institutional/identity_locked) — administer and transmit the succession claim; standing and jurisdiction accrue to them disproportionately
 *   - islamic_ummah: primary beneficiary with conformity costs (organized/identity_locked) — receives Abrahamic identity, owes affirmation of the succession account
 *   - descendants_of_ishmael: genealogical beneficiaries (organized/constrained) — honored descent, bound to the administering community
 *   - jewish_covenantal_exclusivists: primary targets (moderate/identity_locked) — bear the delegitimization of their exclusive claim and the polemical burden of defending it
 *   - muslim_sectarian_dissenters: internal targets (powerless/trapped) — bear orthodoxy enforcement from inside a community they cannot exit
 *   - christian_supersessionist_theologians: excluded rival claimants (institutional/mobile) — hold a replacement reading that answers this one only from outside
 *   - academic_biblical_scholars: analytical observers (institutional/analytical) — see the full textual structure and press it against the reading's exegetical foundations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.46).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.45).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Ishmael-Line Covenant Reading of the Abrahamic Promise").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious/comparative-theology/institutional-authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, '859757c8-afa1-40ff-996c-ae9611941856').
narrative_ontology:cs_kernel_codification('859757c8-afa1-40ff-996c-ae9611941856', fixed_text).
narrative_ontology:cs_authority_grounding('859757c8-afa1-40ff-996c-ae9611941856', lineage).
narrative_ontology:cs_interpretation_layer_present('859757c8-afa1-40ff-996c-ae9611941856').
narrative_ontology:cs_reading_relation('859757c8-afa1-40ff-996c-ae9611941856', abrahamic_covenant__isaac_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('859757c8-afa1-40ff-996c-ae9611941856', abrahamic_covenant__christian_supersessionist_reading, forecloses).
narrative_ontology:cs_axiom('859757c8-afa1-40ff-996c-ae9611941856', foundational, genesis_promise_inclusive_all_lines).
narrative_ontology:cs_axiom_status(genesis_promise_inclusive_all_lines, holdable).
narrative_ontology:cs_axiom_grounding('859757c8-afa1-40ff-996c-ae9611941856', genesis_promise_inclusive_all_lines, theological).
narrative_ontology:cs_axiom('859757c8-afa1-40ff-996c-ae9611941856', foundational, muhammad_seals_abrahamic_prophetic_succession).
narrative_ontology:cs_axiom_status(muhammad_seals_abrahamic_prophetic_succession, holdable).
narrative_ontology:cs_axiom_grounding('859757c8-afa1-40ff-996c-ae9611941856', muhammad_seals_abrahamic_prophetic_succession, theological).
narrative_ontology:cs_reference_frame('859757c8-afa1-40ff-996c-ae9611941856', inclusive_abrahamic_continuity).
narrative_ontology:cs_drift_state('859757c8-afa1-40ff-996c-ae9611941856', contemporary_critical_biblical_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('859757c8-afa1-40ff-996c-ae9611941856', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_ummah).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, descendants_of_ishmael).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_religious_authorities).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, jewish_covenantal_exclusivists).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, muslim_sectarian_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, islamic_ummah).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, quran_as_confirmation_of_prior_revelation).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, finality_of_prophethood_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, ismaeli_sanctuary_custody_claims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars, jurists, and preaching institutions who transmit the account that God's promise to Abraham continued through Ishmael's line and was sealed by Muhammad's prophethood. They author commentaries, set curricula, rule on who may speak for the covenant community, and train each generation of transmitters. Their standing, livelihoods, and jurisdictional authority flow from custodianship of the succession claim; abandoning it would dissolve the warrant of their office.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_religious_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Believers who inherit Abrahamic identity through this reading: pilgrimage rites at Mecca reenact Hagar and Ishmael's story, daily liturgy invokes Abraham, and joining the community counts as entering Abraham's household regardless of ethnicity. The costs arrive as conformity: affirming the finality of Muhammad's prophethood, accepting the transmitted succession account, and absorbing the social consequences of challenging either.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_ummah, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, islamic_ummah, payer).

% Arab peoples as traditionally genealogized from Abraham's elder son. The reading grants their ancestry a central, honored place in sacred history: their forefather raised the sanctuary's walls with Abraham, and their language carries the final scripture. The honor arrives through descent rather than choice and binds them to the community that administers the claim.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, descendants_of_ishmael, beneficiary,
    organized, generational, constrained, global).

% Traditional Jewish readers for whom Genesis states plainly that God establishes His covenant with Isaac and directs Abraham to send Ishmael away. They bear a rival claim that rereads their own scripture against its apparent sense, must answer polemics that cast their exclusivity as parochialism, and historically lived as subordinate minorities under polities governed in the successor community's name. Leaving the contest would mean surrendering the covenantal identity the claim disputes.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, jewish_covenantal_exclusivists, payer,
    moderate, civilizational, identity_locked, global).

% Movements at the community's edge — historic heterodox sects, Qur'an-alone reformers, rationalists questioning the succession chain — who deny or redefine the prophetic-succession account. They face charges of innovation or unbelief, social ostracism, and in some periods and jurisdictions imprisonment or death, while remaining unable to exit the community without losing family, belonging, and legal protection.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, muslim_sectarian_dissenters, payer,
    powerless, biographical, trapped, global).

% Theologians and church bodies holding that the covenant passed to the Church in Christ and concluded there. They would contest the succession claim's use of their scriptures and its placement of Muhammad in the prophetic line, but they stand outside the interpretive conversation this reading conducts with itself; their objections enter only as external polemic.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_theologians, excluded,
    institutional, generational, mobile, global).

% University-based scholars of the Hebrew Bible and late antiquity who assess what Genesis's composition history, ancient Near Eastern parallels, and redaction layers suggest about the promise's original scope. Their publications supply the evidentiary pressure the reading's exegetical foundations must withstand, and their analyses circulate independently of any confession's interests.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, academic_biblical_scholars, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__ishmael_covenant_reading, islamic_religious_authorities).
narrative_ontology:fixing_cost_class(abrahamic_covenant__ishmael_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous Abrahamic identity narrative binding a global religious community to the patriarchal stories: settles who inherits the covenant, integrates the Meccan pilgrimage rites, liturgical invocations of Abraham, and genealogical dignity into one account, and provides a universal route by which any convert enters Abraham's household.
% TRANSFER_FUNCTION: Moves covenantal legitimacy and religious standing: transfers the standing of heir-to-Abraham from exclusive possession by one line to inclusive possession routed through the Ishmael-to-Muhammad succession; shifts interpretive deference from plain-sense readings of Genesis toward Islamic exegetical authority; draws conformity and affirmation of final prophethood from every community member toward the custodial institutions.
% ABSENT_VOICES: Traditional Jewish readers of Genesis, who hold the plain-sense exclusive statements, sit outside this reading's interpretive conversation; their strongest objection (that Genesis itself names Isaac as the covenant bearer) is answered only through exegetical categories they do not share. Pre-Islamic Arabian religious practitioners whose cultic sites and narratives were absorbed into the succession account left no continuing voice. Internal dissenters participate only under enforcement conditions that price their speech.
% DISAPPEARANCE_RATIONALE: If the succession claim vanished overnight, the community's self-description would fail at load-bearing points: the Qur'an presents itself as confirming the earlier revelations, the pilgrimage reenacts Hagar and Ishmael, the creed's prophetic clause names Muhammad in the messenger line, and a fifth of humanity's Abrahamic self-understanding would need wholesale reconstruction. Rival readings would also rearrange, losing the competitor that disciplines their own boundaries.
% FOUNDING_PROBLEM: In the seventh-century Arabian context, a new revelation addressed to peoples outside the recognized covenantal religions needed connection to the sacred history of the People of the Book; without it the community stood outside salvation history entirely. The Ishmael-succession reading solved this: Abraham and Ishmael together raised the Meccan sanctuary, the Arabs were never outside the promise, and Muhammad sealed the prophetic chain.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by academic historiography of late antiquity and early Islam, where the legitimacy problem facing an Arabian revelation is a standard research finding rather than a confessional assertion, and by the polemical literature of rival communities: medieval Jewish and Christian responses to Islamic covenant claims attest the challenge was experienced as real. No attestation rests on the benefiting parties alone.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__ishmael_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__ishmael_covenant_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__ishmael_covenant_reading_tests).
:- end_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.46 because the reading's costs are real but bounded: conformity owed to the succession claim, the polemical burden pushed onto rival claimants, and the historical subordination costs borne under polities ruled in the successor community's name — set against a coordination yield (shared Abrahamic identity for roughly a quarter of humanity, integrated ritual, a universal route into the covenant household) that is genuinely delivered. Suppression is authored at 0.45 as a RAW structural property; it is not scaled by power or scope in the engine's arithmetic. It reflects orthodoxy machinery (unbelief-charges jurisprudence, apostasy sanctions in some jurisdictions, social enforcement) that is material but not the claim's primary support — identity and scriptural authority carry most of the load. Theater ratio is low (0.22): pilgrimage, liturgy, and genealogical teaching are lived practices rather than hollow performance, though modern ceremonial performances of Abrahamic fraternity add a slowly thickening performative layer, visible in the rising series. Accessibility collapse is mid-range (0.55): inside the community, alternative covenant readings collapse quickly because dissent is priced, but globally the rival readings remain fully live — the sibling files exist and recruit. Resistance is high (0.62): plain-sense Jewish objection, supersessionist counter-claims, sectarian denial, and academic criticism press continuously, which is why the reading maintains an interpretive layer at all. The measurement series run on one shared time grid — every tracked metric is authored at every examined time point — and the suppression series is included because the story genuinely traces enforcement-capacity change: orthodoxy machinery built up through the classical period and decayed after the caliphate's abolition.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the agenda-setter seat the reading is custodial duty: preserving an inherited trust, answering challengers, transmitting to the next generation. From the ummah seat it is gracious inclusion: the covenant household opened to all comers. From the target seats the same structure reads as enforced displacement — a rival's reinterpretation overriding one's own scripture (Jewish exclusivists), or a succession account enforced against conscience (sectarian dissenters). The excluded supersessionist theologians experience a third gap: their replacement logic is not engaged but merely answered from outside the conversation. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The ummah and descendants_of_ishmael seats derive low directionality from their beneficiary declarations — the claim subsidizes their identity — while their conformity costs and identity-locked exit pull them somewhat off the pure-beneficiary end. The religious authorities combine administration with concentrated receipt: the claim's yields (custodial office, teaching authority, endowment income) accrue to them disproportionately, which is why gain_flow names that seat rather than the ummah — receipt-of-gain is not the same fact as beneficiary-role, and here the two come apart deliberately. The victim seats derive high directionality: jewish_covenantal_exclusivists pay through delegitimization of their scriptural plain sense, muslim_sectarian_dissenters pay through enforcement, and both pay through the same structure that funds the beneficiaries. No directionality overrides were needed: beneficiary/victim declarations plus exit data already separate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Reading the inclusive rhetoric alone would produce a rope verdict — pure gracious expansion, no losers — and miss the costs borne by dissenters and rivals through the same structure. Reading the competition alone would produce a snare verdict — usurpation dressed as inclusion — and erase the coordination actually delivered: a shared identity narrative integrating ritual, scripture, and genealogy across every continent, plus a universal entry into Abrahamic standing that the exclusive readings structurally withhold. Tangled rope holds both facts: genuine coordination and asymmetric payment, held together by active enforcement. Mandatrophy is not resolved — the founding problem (connecting a new Arabian revelation to the older covenantal history) remains live and load-bearing — so the arrangement persists by function, not by inertia, and the rising theater series tracks added ceremony rather than substitution of performance for function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does this classification describe the Ishmael-succession reading itself, or the whole Abrahamic-covenant kernel?',
    'Treat this file as one reading only: sibling files (isaac_covenant_reading, christian_supersessionist_reading) carry their own epsilon, beneficiaries, and types. Compare across files; never average readings into a single kernel-level verdict.',
    'Collapsing the readings would misstate the beneficiary set (the ummah is beneficiary here, outsider under the Isaac reading) and mask the distinct enforcement profiles each reading carries separately.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This constraint is one reading of the abrahamic_covenant kernel; classification is indexical to that reading.').

omega_variable(
    sibling_structural_delta,
    'How would the structural picture shift under each sibling reading?',
    'Author and diff the sibling files: the Isaac reading produces a smaller exclusive beneficiary set with higher exclusion costs; the supersessionist reading dissolves biological-line claims entirely, removing descendants_of_ishmael from the beneficiary set altogether.',
    'Under the Isaac reading the ummah and descendants move from beneficiary to excluded outsider; under supersessionism every biological-line seat loses standing — the victim/beneficiary topology inverts across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta, conceptual, 'Sibling readings instantiate different constraints with inverted beneficiary/victim topologies.').

omega_variable(
    dispute_location_transmission_channel,
    'Where exactly is the disagreement between this reading and its siblings located?',
    'Locate the contest in the hermeneutical status of Genesis 17:19-21 and 21:12 versus the Qur''anic retelling of Abraham, Ishmael, and the sanctuary, plus the authority ordering between the two scriptural corpora.',
    'If the dispute sits in textual warrant, philology can in principle move it; if it sits in revelation-rank authority, it is structurally irresolvable between confessions and the competition persists indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dispute_location_transmission_channel, conceptual, 'The readings collide on the transmission-channel assignment and on which corpus adjudicates it.').

omega_variable(
    inclusive_exegesis_textual_warrant,
    'Does the plain sense and compositional history of Genesis itself support an inclusive promise, or does the inclusive reading rest wholly on Qur''anic reinterpretation?',
    'Comparative philology, source criticism of the patriarchal narratives, and ancient Near Eastern covenant-form analysis conducted without confessional constraint — the academic_biblical_scholars seat supplies exactly this.',
    'If Genesis is irreducibly particularist, the reading carries a permanent enforcement burden against its own proof-text, sustaining higher suppression and long-run drift toward performative maintenance; if the inclusive sense is defensible, the reading stabilizes as durable identity coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusive_exegesis_textual_warrant, empirical, 'Whether the reading''s exegetical foundation is self-supporting or borrowed from a rival corpus.').

omega_variable(
    legitimacy_rent_capture_degree,
    'Do religious authorities capture disproportionate yields from the succession claim relative to the diffuse identity benefit reaching ordinary believers?',
    'Historical fiscal analysis of endowment incomes, custodial-office revenues, and juristic stipends tied to administering the succession account, compared against measurable lay-side benefits.',
    'High capture would raise the authority seat''s effective extraction and sharpen the extractive edge of the classification; broadly diffuse receipt would support the coordination reading and weaken the case that any seat monopolizes the gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_rent_capture_degree, empirical, 'Degree to which the claim''s yields concentrate in the custodial class rather than the believer body.').

omega_variable(
    enforcement_modernity_trajectory,
    'Will orthodoxy enforcement over the succession claim continue decaying, stabilize at social-sanction levels, or revive under renewed sacral politics?',
    'Track apostasy-law incidence, official unbelief-charge rhetoric from fatwa bodies, and state treatment of Qur''an-alone and heterodox publishers across jurisdictions over the coming decades.',
    'Revival would steepen the suppression_requirement series upward and date a transition toward harder extraction; continued decay supports the current falling tail and keeps the enforcement picture secondary to identity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_modernity_trajectory, empirical, 'Future trajectory of the enforcement machinery the succession claim depends on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 610, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t610, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 610, 0.08).
narrative_ontology:measurement_basis(abra_tr_t610, observed).
narrative_ontology:measurement(abra_tr_t750, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 750, 0.1).
narrative_ontology:measurement_basis(abra_tr_t750, observed).
narrative_ontology:measurement(abra_tr_t1258, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1258, 0.13).
narrative_ontology:measurement_basis(abra_tr_t1258, observed).
narrative_ontology:measurement(abra_tr_t1550, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1550, 0.16).
narrative_ontology:measurement_basis(abra_tr_t1550, observed).
narrative_ontology:measurement(abra_tr_t1924, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1924, 0.19).
narrative_ontology:measurement_basis(abra_tr_t1924, observed).
narrative_ontology:measurement(abra_tr_t2026, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(abra_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t610, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 610, 0.5).
narrative_ontology:measurement_basis(abra_be_t610, observed).
narrative_ontology:measurement(abra_be_t750, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 750, 0.53).
narrative_ontology:measurement_basis(abra_be_t750, observed).
narrative_ontology:measurement(abra_be_t1258, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1258, 0.51).
narrative_ontology:measurement_basis(abra_be_t1258, observed).
narrative_ontology:measurement(abra_be_t1550, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1550, 0.49).
narrative_ontology:measurement_basis(abra_be_t1550, observed).
narrative_ontology:measurement(abra_be_t1924, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1924, 0.44).
narrative_ontology:measurement_basis(abra_be_t1924, observed).
narrative_ontology:measurement(abra_be_t2026, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 2026, 0.46).
narrative_ontology:measurement_basis(abra_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t610, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 610, 0.5).
narrative_ontology:measurement_basis(abra_su_t610, observed).
narrative_ontology:measurement(abra_su_t750, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 750, 0.6).
narrative_ontology:measurement_basis(abra_su_t750, observed).
narrative_ontology:measurement(abra_su_t1258, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1258, 0.62).
narrative_ontology:measurement_basis(abra_su_t1258, observed).
narrative_ontology:measurement(abra_su_t1550, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1550, 0.56).
narrative_ontology:measurement_basis(abra_su_t1550, observed).
narrative_ontology:measurement(abra_su_t1924, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1924, 0.46).
narrative_ontology:measurement_basis(abra_su_t1924, observed).
narrative_ontology:measurement(abra_su_t2026, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 2026, 0.45).
narrative_ontology:measurement_basis(abra_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the Abrahamic covenant.' The label conflates structurally distinct claims — through whom the covenant transmits (isaac_covenant_reading vs this ishmael_covenant_reading), whether transmission terminates in a replacement community (christian_supersessionist_reading), and whether the promise carries a territorial grant (land_promise_constraint). Each member gets its own epsilon, beneficiary set, and type; members are linked here rather than averaged. The Isaac reading is upstream in textual authority: its plain-sense claim is the one this reading must spend interpretive labor answering, so influence flows from it toward this file and toward the territorial reading, while the supersessionist reading presses on both from its own replacement logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
