% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Iconodule Settlement: Dulia Permission under the Image Prohibition
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   After the seventh ecumenical council restored the veneration of images,
 *   the Byzantine commonwealth settled into a standing arrangement: the
 *   Decalogue's image prohibition is read as governing the object of worship,
 *   not the existence of sacred images. Honor (dulia) shown through panels,
 *   mosaics, and frescoes to their prototypes is licit; worship (latria) is
 *   reserved to God alone, and the Incarnation supplies the warrant that
 *   matter can carry sanctity. The arrangement coordinates devotion across a
 *   vast, largely non-literate population through a shared visual grammar,
 *   channels substantial material patronage toward monastic houses, and
 *   concentrates the authority to define orthodox depiction in synods and
 *   patriarchal sees. This story authors that arrangement as it operates from
 *   the final restoration (843) to the sack of Constantinople (1204). It is
 *   one reading of the shared Decalogue kernel; the sibling readings are
 *   separate constraint stories (see commentary.kernel_context and the omega
 *   variables). KEY AGENTS (by structural relationship): - byzantine_laity:
 *   Mass constituency (moderate/constrained) — receives the permission's
 *   devotional infrastructure, pays diffuse votive costs -
 *   monastic_communities: Principal material recipient
 *   (organized/constrained) — produces and houses the image economy -
 *   ecclesial_hierarchy: Agenda setter and authority collector
 *   (institutional/arbitrage) — adjudicates orthodox depiction, administers
 *   the honor/worship border - imperial_court: Enforcement arm and
 *   legitimation recipient (powerful/arbitrage) - parish_clergy and
 *   icon_painters_guilds: Mid-level beneficiaries (moderate/identity_locked
 *   and moderate/constrained) - latria_abusers: Disciplinary edge
 *   (powerless/trapped) — bears corrective costs where honor slides into
 *   worship - heterodox_depictors: Excluded voice (powerless/trapped) —
 *   outside the canon-drawing conversation - church_historians: Analytical
 *   observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.25).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.15).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Settlement: Dulia Permission under the Image Prohibition").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74').
narrative_ontology:cs_kernel_codification('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', fixed_text).
narrative_ontology:cs_authority_grounding('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', lineage).
narrative_ontology:cs_interpretation_layer_present('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74').
narrative_ontology:cs_reading_relation('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', incarnation_sanctifies_matter, theological).
narrative_ontology:cs_axiom('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', foundational, latria_dulia_distinction).
narrative_ontology:cs_axiom_status(latria_dulia_distinction, holdable).
narrative_ontology:cs_axiom_grounding('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', latria_dulia_distinction, conventional).
narrative_ontology:cs_axiom('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', secondary, honor_passes_to_prototype).
narrative_ontology:cs_axiom_status(honor_passes_to_prototype, holdable).
narrative_ontology:cs_axiom_grounding('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', honor_passes_to_prototype, theological).
narrative_ontology:cs_reference_frame('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', latria_only_commandment_scope).
narrative_ontology:cs_drift_state('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', twelfth_century_byzantium, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1fb7c0fa-53d8-4db0-8feb-4b77f21b4c74', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, byzantine_laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, monastic_communities).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, parish_clergy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_painters_guilds).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, ecclesial_hierarchy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, imperial_court).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, latria_abusers).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnation_theology).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, communion_of_saints_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attend liturgy in churches whose walls and screens carry sanctioned images; kiss icons, keep household icon corners, name children after depicted saints. The images give their devotion a shared visual grammar and tie household practice to parish feast days. Practicing without images remains licit but is socially marked; leaving the devotional ecosystem altogether would mean leaving the community's sacramental life.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, byzantine_laity, beneficiary,
    moderate, biographical, constrained, regional).

% Paint, house, and maintain the empire's icons; receive commissions, votive gifts, lamp endowments, and pilgrimage traffic directed at wonder-working images. Monastic economies expanded with the devotional traffic the settlement channels toward their houses. Members are bound by vow and rule to the communities that receive these flows.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, monastic_communities, beneficiary,
    organized, generational, constrained, regional).

% Teach through the image program, bless new icons, and lead feast-day processions. Ordination fuses personal identity with a liturgical office conducted amid the images; departure would mean abandoning vocation, community standing, and the devotional world that constitutes daily work.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, parish_clergy, beneficiary,
    moderate, biographical, identity_locked, local).

% Work under iconographic canons that fix composition, color, and gesture; enjoy protected demand and honored craft status. Work deviating from the canons is refused consecration and ruins reputation; the specialized skills transfer poorly to markets outside the devotional economy.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_painters_guilds, beneficiary,
    moderate, biographical, constrained, regional).

% Convoke synods that define what counts as orthodox depiction, consecrate images, condemn abuses of veneration, and administer penitential discipline when honor slides into worship. The settlement's adjudication authority sits with the patriarchal sees; patriarchs can redirect practice by synodal decree and bear little of the arrangement's costs themselves.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, ecclesial_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, ecclesial_hierarchy, beneficiary).

% Endows churches, funds mosaic programs, and enforces synodal rulings; receives legitimation as the orthodox polity whose piety the image program puts on display. The court can shift patronage and enforcement at will and answers to no internal tribunal.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, imperial_court, beneficiary,
    powerful, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, imperial_court, agenda_setter).

% Devotees whose veneration crosses into treating the image itself as divine: attributing efficacy to the panel, swearing oaths on it, offering it sacrifice. They face penitential discipline, correction of the image's use, and public censure; their standing in the parish depends on accepting the correction.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, latria_abusers, payer,
    powerless, biographical, trapped, local).

% Painters and patrons whose compositions or practices fall outside the canon: novel subjects, unapproved miracle cycles, images made without blessing. Their work is refused consecration and they hold no seat in the synods that draw the lines they are judged by.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, heterodox_depictors, excluded,
    powerless, biographical, trapped, local).

% Study the settlement's records (conciliar acts, synodica, monastery typika, donor inscriptions) from outside the devotional economy. They take no share in its flows and answer to academic rather than ecclesial authority.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, monastic_communities).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives a dispersed, largely non-literate population a shared material grammar for devotion: standard iconography transmits doctrine, synchronizes feast-day observance across parishes, links household practice to liturgical life, and gives communal grief and petition a common focal object.
% TRANSFER_FUNCTION: Moves commissioned work, votive offerings, lamp and feast endowments, and pilgrimage spending from lay donors and the court toward monastic houses and churches; moves devotional attention and bodily practice (kissing, prostration, procession) into hierarchically sanctioned forms; moves doctrinal-legitimation credit upward to the synods that certify images and the dynasty that funds them.
% ABSENT_VOICES: Uncanonical painters and patrons would object to the canon's boundaries but sit outside the synods that draw them; devotees disciplined for crossing into image-worship accept correction or leave quietly; residual aniconists inside the commonwealth keep silence after the post-restoration purges; and the aniconic critique pressed by neighboring communities never enters the conciliar conversation at all.
% DISAPPEARANCE_RATIONALE: Without the permission, devotional life rearranges within a generation: parish interiors lose their visual program, feast-day processions and household icon corners lapse, monastic houses lose commission and votive income, and catechesis loses its principal channel to the non-literate. The hierarchy loses the adjudication authority the canon system exercises; the court loses a publicly displayed proof of orthodox piety.
% FOUNDING_PROBLEM: Reconcile the Decalogue's image prohibition with a community's incarnational conviction that God became visible matter: how can the faithful honor God and the saints through material images without repeating the idolatry the commandment forbids?
% FOUNDING_PROBLEM_CORROBORATION: Adversarial parties outside the beneficiary set attest the problem's genuineness: iconoclast polemicists pressed the idolatry charge and forced the latria/dulia distinction into precise form; the Frankish court's Opus Caroli Regis rejected both the council's formulation and outright iconoclasm while conceding the underlying question was real; Jewish and Muslim neighbors renewed the charge at every frontier. Secular historians of doctrine treat the eighth-century problem as a genuine hermeneutical crux, not a manufactured pretext.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.25 at interval end): the arrangement grants a permission participants overwhelmingly want, and the flows riding on it (commissions, votive gifts, adjudication authority) are modest relative to the devotional value delivered, though they creep upward across the interval with monastic accumulation and the maturing patronage economy. Suppression is authored low (0.15) as a raw structural property — the engine scales only extractiveness, never suppression: the arrangement disciplines a narrow edge (latria abuse, uncanonical work) rather than suppressing alternatives, since non-visual devotion remains licit throughout. Theater is low (0.18): feast-day processions and the annual Synodicon are functional (teaching, synchronizing, commemorating a settled verdict), with only a small ceremonial surplus. Accessibility_collapse is low (0.20) because understanding the rule collapses almost no alternatives — aniconic private prayer and image-less liturgical participation stay open. Resistance is moderate (0.40): internal dissent faded after the post-restoration purges, but borderland aniconist critique and recurring western objection keep friction alive at the margins. The measurement series run on one shared time grid (eight points, three metrics, every metric authored at every point). The dominant temporal signal is enforcement decay: suppression_requirement falls from 0.48 at the restoration (purges of iconoclast clergy, mandatory anathemas) to a routine 0.15 by 1204 as the settlement's victory becomes self-perpetuating through formation and custom. Extractiveness shows no dramatic accumulation — a slow 0.22 to 0.25 drift, not a rent ratchet.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the laity seat the arrangement is near-pure gift: a permission granted, costs diffuse and voluntary. From the hierarchy seat it is administration: the burden of drawing and policing the honor/worship border, offset by concentrated adjudication authority. From the disciplinary-edge seats (latria_abusers, heterodox_depictors) the same border is experienced as coercive force applied by distant synods to people with no seat in them. From the monastic seat it is prosperity structurally tied to the flows. One arrangement, four lived shapes — computed from power and exit differences, not asserted.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (laity, monasteries, clergy, painters, hierarchy) sit at the low-directionality end: the permission subsidizes their devotional and economic life. The hierarchy's dual position (agenda_setter collecting adjudication authority while bearing little cost) keeps it near the beneficiary end despite its administrative role. The court is a low-directionality beneficiary that pays enforcement costs voluntarily in exchange for legitimation. The high-directionality seats are narrow: latria_abusers (trapped, disciplined at the border) and heterodox_depictors (excluded from the conversation that defines their work's legality). Because the target set is a thin disciplinary edge rather than a class the arrangement feeds on, aggregate effective extraction stays low — the structural signature of a permission-coordination arrangement rather than an extraction arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: every wave of conversion into the tradition re-poses the commandment-versus-incarnation question, so there is no dead mandate being theatrically maintained. The arrangement is not inertial — its function (mediating devotion through matter) is performed daily in every parish, and the theater_ratio (0.18) reflects commemorative ceremony, not performative substitution for a lost function. Classifying this as a rope guards against two mislabels: reading the patronage flows as pure extraction (which ignores the genuine coordination the flows fund — catechesis, synchronization, shared focal objects) and reading the permission as costless natural piety (which ignores the concentrated authority and wealth effects the settlement channels toward sees and monastic houses). The identified drift risk runs the other way: if the incarnational warrant ever detached from practice — icons kept as heirlooms and investments rather than conduits — theater_ratio would climb and the arrangement would decay toward an inertially maintained remnant. The current series shows no such drift through 1204.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which reading of the Decalogue image prohibition is authoritative: does the commandment govern the object of worship (this reading), any material representation used in religion (iconoclast_reading), or particular media (moderate_iconoclast_reading)?',
    'Conciliar and jurisdictional adjudication rather than further data about the text: Nicaea II (787) settled the question for the Chalcedonian churches; the Reformation reopened it along communion lines. Compare the doctrinal verdicts and enforcement histories recorded in the sibling stories.',
    'If the iconoclast reading is authoritative, this entire arrangement is the violation itself and its effective extraction re-reads as maximal; if the moderate reading is authoritative, portions of this arrangement''s practice (notably freestanding statuary) fall outside permission and migrate to the sibling''s prohibition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'This constraint is one reading of the decalogue_image_prohibition kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    victim_set_attribution,
    'The scenario brief''s victim set (destroyed artworks, suppressed practices, persecuted icon-venerators) arises under iconoclast enforcement — which reading''s account owns those losses?',
    'Attribute persecution losses to whichever reading wields enforcement in a given regime-period; audit the sibling stories'' victim declarations rather than importing them here.',
    'Prevents double-counting iconoclast-era losses into this reading''s extraction and keeps this story''s epsilon referent fixed on the permission arrangement as it operates under its own lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_attribution, empirical, 'Location of the kernel''s victim set across the competing readings'' enforcement histories.').

omega_variable(
    dulia_latria_border_stability,
    'Is the border between honor (permitted) and worship (forbidden) stable enough in practice that the permission does not systematically generate the abuse it condemns?',
    'Frequency and distribution of synodal condemnations, penitential canons, and corrected abuses across the interval; comparison of documented popular practice against conciliar norms.',
    'If the border is practically undetectable for ordinary devotees, the permission operates as a gateway to the forbidden act and effective extraction rises sharply — the arrangement would drift toward a hybrid coordination/extraction profile with the hierarchy collecting adjudication rents from a boundary it cannot enforce clearly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dulia_latria_border_stability, empirical, 'Practical stability of the honor/worship boundary the permission presupposes.').

omega_variable(
    medium_scope_underdetermination,
    'Does the permission extend to all media alike? The conciliar cases concerned panel and wall images; whether honor-through-matter covers freestanding statuary is under-determined in this reading''s own sources.',
    'Trace synodal rulings and surviving practice across media; the moderate_iconoclast sibling resolves the question oppositely by restricting three-dimensional work outright.',
    'A narrow-medium resolution shrinks this arrangement''s coverage and moves part of its practice under the sibling''s prohibition; a broad resolution absorbs the sibling''s concern entirely and widens this arrangement''s coordination surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medium_scope_underdetermination, conceptual, 'Where the readings'' disagreement bites: the medium axis of the permission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 843, 1204).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iconodule_reading_tr_t843, decalogue_image_prohibition__iconodule_reading, theater_ratio, 843, 0.14).
narrative_ontology:measurement_basis(iconodule_reading_tr_t843, observed).
narrative_ontology:measurement(iconodule_reading_tr_t900, decalogue_image_prohibition__iconodule_reading, theater_ratio, 900, 0.12).
narrative_ontology:measurement_basis(iconodule_reading_tr_t900, observed).
narrative_ontology:measurement(iconodule_reading_tr_t950, decalogue_image_prohibition__iconodule_reading, theater_ratio, 950, 0.11).
narrative_ontology:measurement_basis(iconodule_reading_tr_t950, observed).
narrative_ontology:measurement(iconodule_reading_tr_t1000, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1000, 0.12).
narrative_ontology:measurement_basis(iconodule_reading_tr_t1000, observed).
narrative_ontology:measurement(iconodule_reading_tr_t1050, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1050, 0.13).
narrative_ontology:measurement_basis(iconodule_reading_tr_t1050, observed).
narrative_ontology:measurement(iconodule_reading_tr_t1100, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1100, 0.15).
narrative_ontology:measurement_basis(iconodule_reading_tr_t1100, observed).
narrative_ontology:measurement(iconodule_reading_tr_t1150, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1150, 0.17).
narrative_ontology:measurement_basis(iconodule_reading_tr_t1150, observed).
narrative_ontology:measurement(iconodule_reading_tr_t1204, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1204, 0.18).
narrative_ontology:measurement_basis(iconodule_reading_tr_t1204, observed).

% Extraction over time
narrative_ontology:measurement(iconodule_reading_be_t843, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 843, 0.22).
narrative_ontology:measurement_basis(iconodule_reading_be_t843, observed).
narrative_ontology:measurement(iconodule_reading_be_t900, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 900, 0.2).
narrative_ontology:measurement_basis(iconodule_reading_be_t900, observed).
narrative_ontology:measurement(iconodule_reading_be_t950, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 950, 0.19).
narrative_ontology:measurement_basis(iconodule_reading_be_t950, observed).
narrative_ontology:measurement(iconodule_reading_be_t1000, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1000, 0.19).
narrative_ontology:measurement_basis(iconodule_reading_be_t1000, observed).
narrative_ontology:measurement(iconodule_reading_be_t1050, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1050, 0.21).
narrative_ontology:measurement_basis(iconodule_reading_be_t1050, observed).
narrative_ontology:measurement(iconodule_reading_be_t1100, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1100, 0.23).
narrative_ontology:measurement_basis(iconodule_reading_be_t1100, observed).
narrative_ontology:measurement(iconodule_reading_be_t1150, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1150, 0.24).
narrative_ontology:measurement_basis(iconodule_reading_be_t1150, observed).
narrative_ontology:measurement(iconodule_reading_be_t1204, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1204, 0.25).
narrative_ontology:measurement_basis(iconodule_reading_be_t1204, observed).

% Suppression requirement over time
narrative_ontology:measurement(iconodule_reading_su_t843, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 843, 0.48).
narrative_ontology:measurement_basis(iconodule_reading_su_t843, observed).
narrative_ontology:measurement(iconodule_reading_su_t900, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 900, 0.32).
narrative_ontology:measurement_basis(iconodule_reading_su_t900, observed).
narrative_ontology:measurement(iconodule_reading_su_t950, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 950, 0.24).
narrative_ontology:measurement_basis(iconodule_reading_su_t950, observed).
narrative_ontology:measurement(iconodule_reading_su_t1000, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1000, 0.2).
narrative_ontology:measurement_basis(iconodule_reading_su_t1000, observed).
narrative_ontology:measurement(iconodule_reading_su_t1050, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1050, 0.18).
narrative_ontology:measurement_basis(iconodule_reading_su_t1050, observed).
narrative_ontology:measurement(iconodule_reading_su_t1100, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1100, 0.17).
narrative_ontology:measurement_basis(iconodule_reading_su_t1100, observed).
narrative_ontology:measurement(iconodule_reading_su_t1150, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1150, 0.16).
narrative_ontology:measurement_basis(iconodule_reading_su_t1150, observed).
narrative_ontology:measurement(iconodule_reading_su_t1204, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1204, 0.15).
narrative_ontology:measurement_basis(iconodule_reading_su_t1204, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, attachment_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Decalogue image prohibition' decomposes into three structurally distinct constraints — iconoclast_reading, iconodule_reading (this file), and moderate_iconoclast_reading — each with its own epsilon, beneficiary/victim structure, and enforcement profile, per the epsilon-invariance principle. The shared kernel text anchors all three; this reading's epsilon is assessed on the permission arrangement as it operates, by its own lights. Family links run through affects_constraints; persecution-era losses are owned by whichever sibling's enforcement produced them, not by this story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
