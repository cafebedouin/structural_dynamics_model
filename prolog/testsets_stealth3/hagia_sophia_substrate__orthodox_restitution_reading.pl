% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Orthodox Restitution Claim on the Hagia Sophia (Byzantine-Origin Reading)
 *   domain: cultural heritage / sovereignty / religious authority
 *
 * SUMMARY:
 *   A standing normative claim, held outside Turkey and maintained
 *   principally by Eastern Orthodox memory-institutions, Greek cultural
 *   diplomacy, and diaspora and irredentist constituencies, asserts that the
 *   Hagia Sophia's legitimacy derives from its 537 consecration as the Great
 *   Church of Constantinople and that the site therefore owes either
 *   restoration to Orthodox ecclesiastical hands or reverent neutrality
 *   honoring its Byzantine origin. The claim commands no enforcement anywhere
 *   — no court, treasury, or armed patron — and has not come closer to
 *   implementation since the 1923 Treaty of Lausanne sealed the sovereignty
 *   settlement and removed the local Greek Orthodox population that a
 *   restored parish would have required. What the claim does continuously and
 *   cheaply is coordinate a globally dispersed Orthodox identity around a
 *   shared lost center, supply Greek diplomacy with an episodic
 *   cultural-leverage instrument, and impose a standing symbolic charge on
 *   Turkish sovereignty discourse together with a dormant displacement threat
 *   on the building's Muslim worshippers. The claim/metric split is
 *   deliberate: the constraint is CLAIMED as rope (voluntary identity
 *   coordination, no coercion, alternatives open) while the authored metrics
 *   report what the claim's operation actually looks like — low material
 *   extraction, negligible suppression, a majority-performative activity
 *   profile — and the engine measures whatever divergence follows. KEY AGENTS
 *   (by structural relationship): - eastern_orthodox_diaspora: principal
 *   beneficiary (organized/identity_locked) — draws communal identity
 *   coherence from the claim; cannot abandon it without dissolving the
 *   community's self-understanding - greek_state: episodic beneficiary
 *   (institutional/mobile) — converts the claim into diplomatic leverage at
 *   will; bound to nothing - ecumenical_patriarchate: titular authority and
 *   reluctant steward (moderate/identity_locked) — embodies the claim's
 *   lineage authority while withholding its demand -
 *   greek_irredentist_factions: active maintainer (organized/identity_locked)
 *   — keeps the demand publicly alive as mobilizing grievance -
 *   republic_of_turkey: principal target (institutional/constrained) —
 *   absorbs the claim as a standing external charge on a sovereignty it
 *   indisputably exercises - istanbul_muslim_worship_community: prospective
 *   target (moderate/constrained) — worships in the building and would be
 *   displaced if the demand were ever executed; party to no discussion of it
 *   - universal_heritage_bodies: excluded counter-frame
 *   (institutional/analytical) — hold that the monument's value outruns any
 *   confessional title; unheard inside restitution discourse -
 *   byzantine_studies_scholarship: analytical observer
 *   (analytical/analytical) — supplies the disinterested historical record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.22).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.06).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, rope).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Claim on the Hagia Sophia (Byzantine-Origin Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural heritage / sovereignty / religious authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '3de61d5c-58d4-43ed-9561-c766a760df4a').
narrative_ontology:cs_kernel_codification('3de61d5c-58d4-43ed-9561-c766a760df4a', distributed).
narrative_ontology:cs_authority_grounding('3de61d5c-58d4-43ed-9561-c766a760df4a', lineage).
narrative_ontology:cs_interpretation_layer_present('3de61d5c-58d4-43ed-9561-c766a760df4a').
narrative_ontology:cs_reading_relation('3de61d5c-58d4-43ed-9561-c766a760df4a', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('3de61d5c-58d4-43ed-9561-c766a760df4a', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('3de61d5c-58d4-43ed-9561-c766a760df4a', foundational, founding_consecration_confers_enduring_title).
narrative_ontology:cs_axiom_status(founding_consecration_confers_enduring_title, holdable).
narrative_ontology:cs_axiom_grounding('3de61d5c-58d4-43ed-9561-c766a760df4a', founding_consecration_confers_enduring_title, theological).
narrative_ontology:cs_axiom('3de61d5c-58d4-43ed-9561-c766a760df4a', secondary, converted_church_demands_correction).
narrative_ontology:cs_axiom_status(converted_church_demands_correction, holdable).
narrative_ontology:cs_axiom_grounding('3de61d5c-58d4-43ed-9561-c766a760df4a', converted_church_demands_correction, theological).
narrative_ontology:cs_reference_frame('3de61d5c-58d4-43ed-9561-c766a760df4a', byzantine_consecrated_cathedral).
narrative_ontology:cs_drift_state('3de61d5c-58d4-43ed-9561-c766a760df4a', post_2020_reconversion, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3de61d5c-58d4-43ed-9561-c766a760df4a', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, republic_of_turkey).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, istanbul_muslim_worship_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scattered across North America, Europe, Australia, and Greece itself after the 1923 expulsion of Anatolian Greeks, the diaspora organizes its memory around the Great Church: May 29 commemorations, pilgrimage when travel permits, iconography and hymnography centered on Holy Wisdom. The restitution claim gives this dispersed community a single shared reference point; abandoning it would leave the community's historical self-understanding without its center. Members pass the claim to children as heritage rather than as an actionable program.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, generational, identity_locked, global).

% Keeps the Byzantine patrimony theme available for cultural diplomacy and deploys it episodically in bilateral friction with Turkey — museum partnerships, EU-level cultural resolutions, rhetorical backing for patriarchal requests. Successive governments decline to adopt restitution as policy, treating the theme as negotiable background rather than commitment; it can be amplified or dropped according to diplomatic conditions at no structural cost.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, generational, mobile, regional).

% Exercises complete legal and physical control of the monument, most recently restoring it to mosque use by presidential decree in 2020. It absorbs the restitution claim as a standing symbolic challenge to the national-sovereignty narrative — a claim issued from abroad about territory Turkey governs — and answers it with counter-assertions of conquest legitimacy and diplomatic rebuttal. The claim costs Turkey nothing material; no government, court, or body anywhere can compel anything.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, republic_of_turkey, payer,
    institutional, generational, constrained, national).

% Worships in the building under the current dispensation, resumed in 2020 after eighty-six years as a museum. Congregants inherited the mosque status rather than choosing it, and their access depends entirely on state decisions. The restitution reading, taken seriously, implies their worship would end or move; no member of this community participates in any forum where that implication is discussed.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, istanbul_muslim_worship_community, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, istanbul_muslim_worship_community, excluded).

% The surviving institutional successor of the see whose cathedral the building was. It maintains liturgical commemoration of the 1453 loss, receives pilgrimage devotion oriented toward the inaccessible church, and periodically petitions for religious-liberty concessions — but has never programmatically pursued restitution, which would be politically impossible and would endanger its remaining standing in Turkey. It embodies the claim's authority while withholding the claim's demand.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, beneficiary,
    moderate, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, agenda_setter).

% Nationalist movements, parts of the Church of Greece hierarchy, and diaspora lobbying organizations keep the restitution demand publicly alive through anniversary events, publications, and parliamentary gestures. The demand supplies these movements with a founding grievance and a mobilizing narrative; none possesses, or seeks, any operational pathway toward implementation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_irredentist_factions, agenda_setter,
    organized, generational, identity_locked, national).

% UNESCO and allied heritage institutions hold the monument on the World Heritage List and frame its value as belonging to humanity beyond confessional or national title. From inside restitution discourse their premise — that outstanding universal value outranks any single community's claim — is not admitted to the conversation; their objections register only in heritage fora.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, universal_heritage_bodies, excluded,
    institutional, generational, analytical, global).

% Historians, archaeologists, and art historians document the building's successive lives — cathedral, mosque, museum, mosque — and publish assessments of what restitution or reverent neutralization would mean for the fabric, the mosaics, and the worship. They hold no position in the dispute's outcome and serve as its main source of disinterested record.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_studies_scholarship, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__orthodox_restitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a coherent transnational Eastern Orthodox identity anchored to the Great Church: a shared narrative, a calendar of commemoration, and a pilgrimage destination binding a diaspora scattered across dozens of countries after the 1923 population exchange.
% TRANSFER_FUNCTION: Moves symbolic goods — identity coherence to the diaspora, episodic diplomatic leverage to Greece, grievance-narrative material to nationalist movements — while imposing a standing symbolic charge on Turkish sovereignty discourse and a dormant displacement threat on the building's Muslim worshippers. No material wealth moves in either direction.
% ABSENT_VOICES: The site's Muslim worshippers and Turkish heritage authorities are absent from every forum where the restitution claim is formulated; universal-heritage institutions object only from outside the claim's frame; and the Istanbul Greek community the claim nominally represents was expelled in 1923 and no longer exists as a constituency — the claim speaks for a community that is no longer there.
% DISAPPEARANCE_RATIONALE: If the claim vanished overnight, the diaspora's commemorative calendar and identity infrastructure would hollow — the community would lose its principal shared reference point — Greek-Turkish cultural rhetoric would lose a recurring motif, and nationalist movements would lose a founding grievance. Material arrangements at the site itself would not change by a stone: no enforcement existed to remove, and no budget line, statute, or treaty provision depends on the claim.
% FOUNDING_PROBLEM: The 1453 conquest stripped the Great Church of Constantinople from Orthodox worship; this reading was formed to recover the consecrated cathedral for the church that founded it — or, failing recovery, to secure reverent neutrality honoring its consecration.
% FOUNDING_PROBLEM_CORROBORATION: No consequential actor outside the claim's beneficiary set treats restitution as live. The Greek foreign ministry formally disavows territorial or restitution claims; the Lausanne settlement and a century of uninterrupted state practice corroborate closure; Turkish authorities treat the question as settled; the Ecumenical Patriarchate itself confines its petitions to religious-liberty matters short of restitution. Eschatological currents inside Orthodox tradition keep a prophetic version alive ('the City will be handed back'), but no institutional actor programs toward it — attestation of liveness is absent everywhere outside the beneficiary set.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).
:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at interval end) because the claim moves no money, labor, or territory: its entire yield is symbolic, and its principal costs — a standing rhetorical charge on Turkish sovereignty and a dormant displacement threat on the building's worshippers — are imposed without any mechanism that could realize them. Suppression is authored near-zero (0.06): the claim coerces no one, excludes no rival reading, and is held voluntarily by everyone who holds it. Theater_ratio is authored above the Goodhart line (0.58): the majority of the claim's observable activity — anniversary liturgies staged partly for cameras, parliamentary gestures, documentary and polemical production — aims at audiences and identity maintenance rather than at any step toward restitution, while a genuine residual function (liturgical memory, pilgrimage, heritage documentation) continues to perform real work for its holders. Accessibility collapse is very low (0.12): understanding this claim forecloses nothing, since every alternative account of the site remains fully available — precisely what distinguishes it from enforced arrangements. Resistance is moderate-low (0.35): Turkey rebuts the claim routinely and segments of Greek opinion reject irredentist framing, but inside the holder communities the claim meets essentially no resistance. The measurement series run on ONE shared grid (T=0..97 mapped to 1923..2020) with every tracked metric authored at every point; suppression_requirement series are deliberately omitted because no enforcement machinery ever existed to build up or decay — the scalar 0.06 already states the whole enforcement picture. The slow rise in base_extractiveness across the grid reflects creeping symbolic intensification (rising salience in bilateral friction, peaking after the 2020 reconversion handed the claim fresh provocation), not material accumulation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergently by construction. From the diaspora seat the claim is inheritance: a duty of memory owed to the founding community, costless to hold and constitutive of identity — the arrangement presents as near-pure coordination. From Ankara's seat the same sentences are an external lien on national territory: unwelcome, unactionable, and irritating chiefly in proportion to Turkey's own investment in conquest-legitimacy narrative. From the Istanbul worshippers' pew the claim is a dormant eviction notice they did not write and may not answer. From the Patriarchate's throne it is an inheritance it must embody but dares not spend. Same sentences, four different arrangements — the engine derives each seat's classification from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. eastern_orthodox_diaspora (declared beneficiary, identity_locked exit) derives toward the deep-beneficiary end: the claim subsidizes its identity and exit is structurally unavailable without self-dissolution. greek_state (beneficiary, mobile exit) also derives beneficiary-side but shallowly — it engages episodically and walks away at zero cost, so its subsidy is opportunistic rather than load-bearing. republic_of_turkey (declared victim/payer, institutional power, constrained exit) derives toward the target end: the claim is aimed squarely at its sovereignty narrative and it cannot retire the claim from the receiving side. istanbul_muslim_worship_community (payer, moderate, constrained) also derives target-side on structure — the demand, taken seriously, displaces them — although their current material burden is effectively zero because the demand is never executed. No directionality override is used: the derivation correctly encodes their structural position as targets of the claim, and the claim's overall impotence (suppression 0.06, enforcement nil) is what keeps effective extraction low for every seat. Scope effects run modestly: the claim reaches globally through the diaspora while its target is national, and verification of any hypothetical compliance is trivially moot — the engine owns those modifiers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — restore the Great Church, or secure reverent neutrality for a desecrated consecration — died as an actionable program at Lausanne in 1923, when the sovereignty settlement hardened and the population exchange removed the community a restoration would have served. The claim did not die with it; its function migrated from recovery to remembrance. Classifying the residue prevents two symmetrical errors. Read as pure extraction, the claim's total absence of enforcement, coercion, and material transfer is missed — no one is being robbed of anything realizable. Read as healthy coordination with no history, the dead mandate disappears: the R5 interview records founding_problem_status dead against disappearance_verdict world_rearranges, which routes the mismatch to the zombie/capture cross-check — the correct disposition for a constraint that persists past its purpose on identity fuel. The terminal trajectories differ: if diaspora identity-lock holds, the claim remains a live if mostly theatrical coordination device; if the lock decays across generations, what remains is performance without audience-function — the degraded, inertial endpoint. Nothing in the current profile licenses calling that endpoint yet: theater at 0.58 is high, but a functioning identity core persists beneath the performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the orthodox_restitution_reading of the hagia_sophia_substrate kernel. Would the beneficiary/victim structure and the classification invert if the islamic_sovereignty_reading or the universal_heritage_reading were instantiated instead?',
    'Compile the two sibling readings as separate constraint stories and compare per-seat classifications across the family; the substrate is shared but each reading is a distinct constraint with its own epsilon.',
    'Inverting the reading reverses the directionalities: the Turkish sovereignty seat moves from target to beneficiary and the Orthodox seats move from beneficiary to excluded claimants, and the claimed type changes with the reversed structure. The three files must never be merged into one constraint or one epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Kernel-membership and reading-indexing uncertainty: this constraint is one reading, not the substrate.').

omega_variable(
    restitution_pathway_existence,
    'Does any pathway — legal revision, great-power sponsorship, treaty renegotiation, or some unforeseeable political-eschatological conjuncture — exist by which the restitution demand could become actionable?',
    'Survey international-law analyses, Greek official policy documents (successive governments formally disavow restitution), and Turkish constitutional provisions; monitor whether any state actor ever programs toward title transfer.',
    'A live pathway would convert the prospective victim burdens into material ones: effective extraction would rise sharply for the Turkish seats and the arrangement would drift toward enforced-extraction classifications. Confirmed absence locks the low-extractiveness profile in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restitution_pathway_existence, empirical, 'Whether the claim''s dormancy is permanent or merely contingent.').

omega_variable(
    symbolic_extraction_reality,
    'Does the standing claim impose real costs on Turkish sovereignty discourse and bilateral heritage diplomacy, or is it cost-free expressive speech that extracts nothing?',
    'Attempt causal attribution of measurable effects — the 2020 reconversion''s rhetorical justification as rebuttal, recurring bilateral summit friction, EU cultural-policy positioning — to the claim''s presence versus other drivers of Greek-Turkish tension.',
    'A pure-expression finding pushes extractiveness toward 0.05 and strengthens the pure-coordination reading; demonstrated causal weight raises the metric and strengthens hybrid coordination/extraction readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_extraction_reality, empirical, 'Whether symbolic imposition counts as extraction for this constraint.').

omega_variable(
    diaspora_identity_lock_decay,
    'How durable is the diaspora''s identity-lock on the claim across coming generations?',
    'Longitudinal cohort studies of commemoration participation and heritage salience among third-generation-and-beyond diaspora populations.',
    'Erosion converts the claim into inertial performance maintained by a shrinking remnant — a decay trajectory toward the degraded, theatrical endpoint; a durable lock sustains the live identity-coordination function indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_identity_lock_decay, empirical, 'Durability of the identity fusion that maintains the claim past its dead mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 0, 97).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(hagi_tr_t0, observed).
narrative_ontology:measurement(hagi_tr_t20, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(hagi_tr_t20, observed).
narrative_ontology:measurement(hagi_tr_t40, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(hagi_tr_t40, observed).
narrative_ontology:measurement(hagi_tr_t60, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement_basis(hagi_tr_t60, observed).
narrative_ontology:measurement(hagi_tr_t80, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 80, 0.54).
narrative_ontology:measurement_basis(hagi_tr_t80, observed).
narrative_ontology:measurement(hagi_tr_t97, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 97, 0.58).
narrative_ontology:measurement_basis(hagi_tr_t97, observed).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(hagi_be_t0, observed).
narrative_ontology:measurement(hagi_be_t20, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement_basis(hagi_be_t20, observed).
narrative_ontology:measurement(hagi_be_t40, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement_basis(hagi_be_t40, observed).
narrative_ontology:measurement(hagi_be_t60, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 60, 0.19).
narrative_ontology:measurement_basis(hagi_be_t60, observed).
narrative_ontology:measurement(hagi_be_t80, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 80, 0.2).
narrative_ontology:measurement_basis(hagi_be_t80, observed).
narrative_ontology:measurement(hagi_be_t97, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 97, 0.22).
narrative_ontology:measurement_basis(hagi_be_t97, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hagia_sophia_substrate__orthodox_restitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% Colloquial usage treats 'whose Hagia Sophia?' as one question; it is three structurally distinct constraints sharing one substrate. The islamic_sovereignty_reading is the upstream, operative member (it controls actual practice and holds enforceable title); the orthodox_restitution_reading (this file) is downstream and exerts symbolic counter-pressure on it without touching practice; the universal_heritage_reading mediates both in heritage fora. The epsilon values differ by construction: the operative reading assesses its own arrangement as legitimate, the restitution reading's claim is materially impotent but symbolically generative, and the universal reading prices exclusivity of any kind. Linking the family lets contamination analysis track, for instance, how the 2020 reconversion (an islamic-sovereignty assertion) intensified the restitution claim's rhetorical activity without changing its enforcement capacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
