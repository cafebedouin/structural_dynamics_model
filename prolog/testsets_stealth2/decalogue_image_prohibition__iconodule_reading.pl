% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconodule_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Settlement: Honor Through Images, Worship Reserved to God Alone
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The Decalogue's image prohibition is a fixed text that three readings
 *   instantiate as three different constraints; this story authors the
 *   iconodule reading alone: the prohibition forbids latria — worship of
 *   images — and permits dulia, honor shown through images to their
 *   prototypes, because the Incarnation sanctifies matter as a valid conduit
 *   to the divine. The standing arrangement under contest is the lived
 *   iconodule settlement of the Byzantine world across 726-886 CE
 *   (measurement grid T0-T160, twenty-year steps): customary veneration
 *   challenged by imperial iconoclasm (726-787, 815-843), codified at Nicaea
 *   II (787), restored finally in 843, and thereafter enforced as obligatory
 *   orthodoxy through the annual Synodikon. Scoping note: the manifest's
 *   expected delta lists a victim set 'under iconoclast enforcement'
 *   (destroyed artworks, suppressed practices, persecuted venerators); those
 *   cost-bearers belong to the sibling iconoclast story, and per the
 *   one-reading rule they are not folded into this constraint's structure.
 *   This arrangement's own payer class is those who decline veneration under
 *   its enforcement — strict commandment observers anathematized after 843 —
 *   together with the popular devotees repeatedly corrected for crossing the
 *   honor/worship line. Claim and metrics are independent authored facts: the
 *   claim is tangled_rope (coordination core plus enforced asymmetry in the
 *   settled regime); the metrics describe the operation as the record shows
 *   it.
 *
 * KEY AGENTS:
 *   - ecclesial_hierarchy: agenda-setting beneficiary (institutional/identity_locked) — defines lawful veneration, collects deference and authority
 *   - byzantine_laity: primary beneficiary (moderate/constrained) — receives devotional access, carries offerings and post-843 performance obligations
 *   - monastic_communities: beneficiary (organized/identity_locked) — produces, guards, and theologically anchors the icon economy
 *   - icon_painter_guilds: beneficiary (moderate/mobile) — commissioned labor under canonical depiction rules
 *   - strict_commandment_observers: primary payer (powerless/constrained) — declines veneration, bears anathema and civil disability
 *   - latria_excess_practitioners: recurrent payer (powerless/constrained) — popular fervor corrected by canon and sermon
 *   - imperial_authority: agenda-setter with arbitrage (powerful/arbitrage) — inverted from persecutor to enforcer across the interval
 *   - western_patriarchate: observer (institutional/analytical) — endorses the settlement while practicing differently
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.3).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.45).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, tangled_rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Settlement: Honor Through Images, Worship Reserved to God Alone").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'b338d728-5130-4fc4-bc73-f4c1757d423c').
narrative_ontology:cs_kernel_codification('b338d728-5130-4fc4-bc73-f4c1757d423c', fixed_text).
narrative_ontology:cs_authority_grounding('b338d728-5130-4fc4-bc73-f4c1757d423c', lineage).
narrative_ontology:cs_interpretation_layer_present('b338d728-5130-4fc4-bc73-f4c1757d423c').
narrative_ontology:cs_reading_relation('b338d728-5130-4fc4-bc73-f4c1757d423c', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('b338d728-5130-4fc4-bc73-f4c1757d423c', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('b338d728-5130-4fc4-bc73-f4c1757d423c', foundational, incarnation_sanctifies_material_mediation).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_material_mediation, holdable).
narrative_ontology:cs_axiom_grounding('b338d728-5130-4fc4-bc73-f4c1757d423c', incarnation_sanctifies_material_mediation, theological).
narrative_ontology:cs_axiom('b338d728-5130-4fc4-bc73-f4c1757d423c', foundational, honor_through_images_reaches_prototype).
narrative_ontology:cs_axiom_status(honor_through_images_reaches_prototype, holdable).
narrative_ontology:cs_axiom_grounding('b338d728-5130-4fc4-bc73-f4c1757d423c', honor_through_images_reaches_prototype, theological).
narrative_ontology:cs_axiom('b338d728-5130-4fc4-bc73-f4c1757d423c', secondary, images_are_books_of_the_illiterate).
narrative_ontology:cs_axiom_status(images_are_books_of_the_illiterate, holdable).
narrative_ontology:cs_axiom_grounding('b338d728-5130-4fc4-bc73-f4c1757d423c', images_are_books_of_the_illiterate, instrumental).
narrative_ontology:cs_reference_frame('b338d728-5130-4fc4-bc73-f4c1757d423c', incarnational_dulia_permission).
narrative_ontology:cs_drift_state('b338d728-5130-4fc4-bc73-f4c1757d423c', post_triumph_of_orthodoxy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b338d728-5130-4fc4-bc73-f4c1757d423c', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, byzantine_laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, monastic_communities).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, ecclesial_hierarchy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_painter_guilds).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, strict_commandment_observers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, latria_excess_practitioners).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnation_matter_sanctification_doctrine).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, dulia_latria_distinction).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, conciliar_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines what counts as lawful veneration: councils fix the distinction between worship owed to God alone and honor shown through images to their prototypes, canons regulate depiction, and bishops correct excess. After the restoration of 843 it prescribes the annual Synodikon, which anathematizes those who reject icon veneration. It collects the deference, offerings, and institutional authority that flow through the devotional economy it administers; its own standing is constituted by guardianship of the settlement, so stepping back from it would dissolve the office's claim to teach.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, ecclesial_hierarchy, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, ecclesial_hierarchy, beneficiary).

% Attends churches filled with mosaic and panel icons, teaches its children the faith through images, makes offerings before favored icons, and joins processions on the great feasts. It receives accessible devotion and communal identity; it carries the cost indirectly through the offerings and, after 843, through the obligation to perform veneration publicly on pain of suspicion.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, byzantine_laity, beneficiary,
    moderate, biographical, constrained, national).

% Paint, guard, and venerate icons; house the shrines and pilgrimage sites; produced the confessors who suffered mutilation and exile under the iconoclast emperors and supplied the theological argument for the restoration. After the settlement they collect patronage and commissions and staff the feasts; their rule and self-understanding are fused with the guardianship of holy images.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, monastic_communities, beneficiary,
    organized, generational, identity_locked, national).

% Work under canonical rules governing how Christ, the Theotokos, and the saints may be depicted. Commissions flow to them from churches and monasteries; the canons constrain composition but guarantee demand. A painter who cannot accept the rules can paint secular work or move to a jurisdiction with different practice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_painter_guilds, beneficiary,
    moderate, biographical, mobile, regional).

% Read the prohibition as covering the veneration act itself and decline to bow before images, however honorific the intent. Within the empire after 843 this refusal draws anathema, loss of office for clergy, and civil disability; some emigrate to jurisdictions beyond imperial reach at the cost of home, community, and career. Their position is transmitted in scattered communities and neighboring traditions.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, strict_commandment_observers, payer,
    powerless, generational, constrained, national).

% Popular devotees whose fervor crosses the line the councils draw — kneeling to icons as though to gods, attributing power to the object, treating the panel as the saint himself. They are corrected by sermon and canon, sometimes excommunicated until they amend; the discipline is absorbable and the practice adjustable, but the correction recurs generation after generation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, latria_excess_practitioners, payer,
    powerless, biographical, constrained, local).

% Sets civil penalties and convenes councils; its policy inverted twice in the interval, first stripping the practice of legal footing, then, after 843, enforcing it. In the settled regime it gains legitimacy from association with orthodoxy and lends the police power that backs the anathemas; it retains the demonstrated capacity to reverse course.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, imperial_authority, agenda_setter,
    powerful, generational, arbitrage, national).

% Endorsed the conciliar settlement and received its refugees, yet never adopted the annual anathema machinery or the same density of icon practice, and later developed statuary and a different theology of images. It watches the eastern settlement from adjacent practice — close enough to validate it, distant enough to show the settlement is not the only way the text can be kept.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, western_patriarchate, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, ecclesial_hierarchy).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem the commandment creates for an incarnational faith: how a community affirming a visibly incarnate God can use material images without collapsing into the idolatry the text forbids. The latria/dulia distinction supplies a shared criterion separating worship owed to God alone from honor passing through the image to its prototype; canonical depiction rules supply quality control; the result is a common devotional technology that teaches the illiterate, unifies practice across a vast territory, and anchors collective memory.
% TRANSFER_FUNCTION: Moves offerings, patronage, and commissions from laity and donors into the church-monastery icon economy; moves public allegiance — performed veneration — from all subjects to the ecclesial-imperial axis; after 843 it additionally moves conformity from dissenters under penalty of anathema and civil disability.
% ABSENT_VOICES: Strict commandment observers had no seat at Nicaea II or in the Synodikon: the councils fixing the reading were convened and staffed by parties already committed to images, and the recorded objections enter only as the condemned position. The Jewish and Muslim communities whose aniconism the iconodule polemic cast as the archetypal error were likewise outside the conversation. Their standing objection — that the intent-based distinction rationalizes what the text forbids — survives in their own traditions, not in the conciliar record.
% DISAPPEARANCE_RATIONALE: Devotional life, church decoration, monastic economies, and the liturgical calendar are organized around sanctioned images; overnight disappearance would empty the churches' visual program, collapse the commission economy, sever the pedagogical channel for the illiterate, and force an immediate doctrinal crisis over whether the remaining practice is obedience or idolatry.
% FOUNDING_PROBLEM: A faith centered on an incarnate, visible God carries a founding text that forbids images; communities needed to know whether matter can mediate divine honor without repeating the idolatry Israel was warned against.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the durable existence of communities that reject the iconodule resolution — aniconic Protestant denominations, Jewish and Muslim practice — whose persistence attests that the founding problem admits no self-evidently settled answer; and by the acts of Nicaea II itself, which preserve the iconoclast party's arguments as a genuinely disputed position rather than a strawman. The sibling readings' continued vitality is the external attestation that the problem is real.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconodule_reading_tests).
:- end_tests(decalogue_image_prohibition__iconodule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores describe the settled arrangement at interval end unless noted. Extractiveness 0.30 is reading-indexed: the referent is the standing iconodule arrangement assessed by the reading's own lights, which regards the structure as predominantly beneficent — the latria/dulia line protects worshippers from the commandment's danger while opening matter as conduit — and the 0.30 encodes the residue the reading itself acknowledges: authority rents consolidated in the hierarchy's definitional power, the patronage economy around shrines and panels, and the post-843 burdens on those who decline veneration, which the tradition classifies as necessary discipline rather than taking. Suppression 0.45 is a raw structural property, unscaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine): mandatory veneration, the annual Synodikon anathemas, and civil penalties for refusal are real coercion, well short of the destruction-scale enforcement the sibling reading applied to venerators. Theater 0.20: the devotional function is performed daily and sincerely; the rise from 0.08 to 0.20 tracks post-settlement ceremonial elaboration (the Feast of Orthodoxy, court ritual around the restored Chalke icon). Accessibility_collapse 0.45: open abstention collapsed inside imperial jurisdiction after 843, but quiet minimalist practice, extra-imperial aniconic communities, and the western church's divergent development kept alternatives partly alive. Resistance 0.60: a century of armed imperial opposition, twice over, plus a persistent dissenter class after the settlement. Temporal series run on one shared nine-point grid (T0-T160, twenty-year steps mapping 726-886 CE) with all three tracked metrics authored at every point. The base_extractiveness and suppression_requirement series are cyclical rather than monotonic: the two iconoclast interludes crash the arrangement's enforcement capacity and force adherents' resource mobilization underground (extraction peaks 0.42-0.44 as the network mobilizes everything its members have), while restorations release the burden. The oscillation is driven by imperial policy flips — the constraint is the persecuted party mid-interval, not the intermittent reinforcer; its own enforcement trajectory (suppression_requirement) shows collapse (0.08-0.12) and rebuild (0.50) instead. The claim (tangled_rope) and the metrics were authored independently; where the engine's per-seat computation diverges from either, that divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   From the strict-commandment seat the arrangement is a machine for compelling what conscience forbids — anathema, office-stripping, exile; from the laity and monastic seats it is the delivery system for devotion, memory, and beauty; from the hierarchy's seat it is guardianship, and the penalties are protection of the flock. The engine computes these divergent classifications from the same structural data. The reading-indexed epsilon (0.30) prices the arrangement as the iconodule reading itself assesses it — beneficent in intent, with the residue it acknowledges (institutional authority rents, the patronage economy, the reframed burdens on refusers) rather than the burden as the refuser experiences it; a sibling story authored from the iconoclast seat over the same practice would price it far higher, and that cross-family divergence is the measurement, not an inconsistency.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Beneficiaries with working exits sit nearest the subsidy end: icon painters (mobile) lowest; laity (constrained but massively net-benefited) low; monasteries (identity-locked but net-benefited) low. The hierarchy (agenda_setter, beneficiary, identity_locked) sits near the beneficiary end — it administers the flows it collects. Payers sit near the target end: strict commandment observers (constrained exit, severe penalties) highest; latria-excess practitioners high but with lighter, absorbable costs. One override: the imperial seat derives ambiguously because its relationship inverted across the interval (persecutor 726-787 and 815-843, enforcer-beneficiary after 843); the settled-arrangement d is authored at 0.2 via override on the powerful atom, since a static derivation from its agenda_setter role cannot see the inversion. The western patriarchate holds the analytical seat and enters no chi arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The manifest's rope expectation is half-right, and the half it misses is the one that matters for classification. The reading's doctrinal core — permission under intent-conditions — is genuine coordination solving the commandment's own tension, and most participants are net beneficiaries; a snare label would erase that. But the settled regime added a payer class (those who decline veneration) and an enforcement machinery (annual anathemas, civil penalties) that pure coordination does not need; a rope label would erase them. Claiming tangled_rope keeps both visible. The founding problem remains live — the tension between incarnate visibility and aniconic commandment regenerates every generation, witness the recurring Reformation-era and modern debates — so no mandatrophy declaration; the arrangement is not a scaffold (no sunset clause; nobody expects it to end) and not a piton (its function is performed daily in millions of devotional acts; theater_ratio 0.20). The live risk is drift along the dulia/latria boundary (see omegas): chronic erosion would push the arrangement toward snare flavor; relaxation of enforcement toward permission-only would pull it back toward rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the iconodule_reading instantiation of the decalogue_image_prohibition kernel; which structural elements would change under the sibling readings, and where exactly do the readings disagree?',
    'Author the sibling stories (iconoclast_reading, moderate_iconoclast_reading) and compare victim sets, permission lines, and enforcement objects; the disagreement locates in whether the prohibition''s object is the act of representation (iconoclast) or the intent of the worshipper (iconodule), with the moderate reading splitting the line by form rather than intent.',
    'Under the iconoclast reading the victim set inverts (destroyed artworks, persecuted venerators, and ruined painters replace anathematized refusers) and epsilon rises sharply; under the moderate reading the permission line moves from intent to dimensionality, changing the payer class to statuary-makers and latria-prone devotees. Cross-family comparison of the three stories is the resolution instrument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, three readings, disagreement located in the prohibition''s object (representation-act vs worshipper-intent vs representational form).').

omega_variable(
    dulia_latria_boundary_erosion,
    'Does the honor/worship boundary hold in popular practice, or does veneration chronically erode into functional latria requiring repeated correction?',
    'Conciliar and canonical record of corrections (repeated condemnations of image-directed worship practices), homiletic evidence, and study of popular devotion across generations.',
    'Chronic erosion weakens the coordination claim — the permission becomes cover for the very worship it forbids — raising effective extraction and pushing classification toward snare; a stable boundary supports the coordination reading and the tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dulia_latria_boundary_erosion, empirical, 'Whether the dulia/latria line is stable in practice or chronically eroding under popular fervor.').

omega_variable(
    obligation_creep_contingency,
    'Is post-settlement mandatory veneration (penalties for refusal) intrinsic to the iconodule reading, or a contingent capture by imperial-ecclesial power?',
    'Comparative analysis of iconodule communities outside imperial enforcement — the western patriarchate accepted Nicaea II without Synodikon-style penalties; later non-coercive iconophile traditions exist. If the reading thrives without coercion, the obligation layer is accretion.',
    'If contingent, the reading''s core is rope-like and the tangled elements are removable historical accretion; if intrinsic (orthodoxy requires uniform performance), the extraction is structural and permanent, and the tangled_rope claim hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_creep_contingency, conceptual, 'Whether the enforcement asymmetry is constitutive of the reading or an imperial appendage.').

omega_variable(
    incarnation_premise_indexicality,
    'The entire permission structure rests on the Incarnation premise (matter can mediate grace); how much of the classification depends on adopting that premise from inside the tradition?',
    'Re-author epsilon from an external aniconic seat: the same arrangement assessed without the premise shows the permission as rationalization — the sibling iconoclast story performs exactly this assessment over the same historical material.',
    'Reading-indexed epsilon already prices this: the iconodule seat yields 0.30; an iconoclast-seat story of the same practice would yield substantially higher epsilon. Classification divergence across the constraint family is the designed measurement, not an error to reconcile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incarnation_premise_indexicality, conceptual, 'Indexical dependence of the permission structure on the Incarnation premise; handled by the family''s reading-indexed epsilon values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 160).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(deca_tr_t0, observed).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconodule_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(deca_tr_t20, observed).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconodule_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement_basis(deca_tr_t40, observed).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__iconodule_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement_basis(deca_tr_t60, observed).
narrative_ontology:measurement(deca_tr_t80, decalogue_image_prohibition__iconodule_reading, theater_ratio, 80, 0.13).
narrative_ontology:measurement_basis(deca_tr_t80, observed).
narrative_ontology:measurement(deca_tr_t100, decalogue_image_prohibition__iconodule_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement_basis(deca_tr_t100, observed).
narrative_ontology:measurement(deca_tr_t120, decalogue_image_prohibition__iconodule_reading, theater_ratio, 120, 0.24).
narrative_ontology:measurement_basis(deca_tr_t120, observed).
narrative_ontology:measurement(deca_tr_t140, decalogue_image_prohibition__iconodule_reading, theater_ratio, 140, 0.22).
narrative_ontology:measurement_basis(deca_tr_t140, observed).
narrative_ontology:measurement(deca_tr_t160, decalogue_image_prohibition__iconodule_reading, theater_ratio, 160, 0.2).
narrative_ontology:measurement_basis(deca_tr_t160, observed).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(deca_be_t0, observed).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(deca_be_t20, observed).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(deca_be_t40, observed).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 60, 0.26).
narrative_ontology:measurement_basis(deca_be_t60, observed).
narrative_ontology:measurement(deca_be_t80, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 80, 0.23).
narrative_ontology:measurement_basis(deca_be_t80, observed).
narrative_ontology:measurement(deca_be_t100, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(deca_be_t100, observed).
narrative_ontology:measurement(deca_be_t120, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 120, 0.3).
narrative_ontology:measurement_basis(deca_be_t120, observed).
narrative_ontology:measurement(deca_be_t140, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 140, 0.28).
narrative_ontology:measurement_basis(deca_be_t140, observed).
narrative_ontology:measurement(deca_be_t160, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 160, 0.3).
narrative_ontology:measurement_basis(deca_be_t160, observed).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(deca_su_t0, observed).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement_basis(deca_su_t20, observed).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(deca_su_t40, observed).
narrative_ontology:measurement(deca_su_t60, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 60, 0.32).
narrative_ontology:measurement_basis(deca_su_t60, observed).
narrative_ontology:measurement(deca_su_t80, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement_basis(deca_su_t80, observed).
narrative_ontology:measurement(deca_su_t100, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement_basis(deca_su_t100, observed).
narrative_ontology:measurement(deca_su_t120, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement_basis(deca_su_t120, observed).
narrative_ontology:measurement(deca_su_t140, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 140, 0.48).
narrative_ontology:measurement_basis(deca_su_t140, observed).
narrative_ontology:measurement(deca_su_t160, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 160, 0.45).
narrative_ontology:measurement_basis(deca_su_t160, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% One fixed text (the Decalogue's image prohibition), three constraint stories: this iconodule reading (permission under intent-conditions; epsilon 0.30 reading-indexed), the iconoclast reading (total prohibition; its victims are images, their makers, and venerators), and the moderate reading (dimension-split permission). The readings form a family because each cites the same text as warrant and each defines itself against the others. The iconoclast enforcement episodes (726-787, 815-843) are the sibling story's extraction record and simultaneously this reading's martyrdom economy — the family edges run both directions, and the upstream text's ambiguity is what permits all three downstream instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconodule_reading, powerful, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
