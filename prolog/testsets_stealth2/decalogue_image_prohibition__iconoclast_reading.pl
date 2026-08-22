% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconoclast_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Iconoclast Reading — Universal Prohibition of Religious Imagery in Worship
 *   domain: theology/religious authority/visual culture
 *
 * SUMMARY:
 *   Under the iconoclast reading of the Decalogue's second commandment, every
 *   material representation used in worship is idolatry, and the imperial
 *   state enforces that reading as law. Between 726 and 843 the Byzantine
 *   court twice made the prohibition the empire's official doctrine (the
 *   councils of 754 and 815), destroyed or whitewashed the churches' images,
 *   persecuted the monks and laity who venerated them, and redistributed
 *   confiscated monastic wealth to the treasury, the army, and a settlement
 *   clergy. The arrangement collapsed twice — at Nicaea II (787) and finally
 *   at Theodora's restoration and the Synodikon (843) — and each collapse
 *   showed what depended on it: within months icons returned to every church
 *   and production resumed. This story instantiates the iconoclast reading
 *   only, as one clean epsilon-invariant constraint; the iconodule and
 *   moderate readings are separate stories linked through the network and the
 *   reading relations. Because this reading endorses the arrangement it
 *   describes, its epsilon is authored from the reading's own lights — the
 *   imposed costs it concedes as discipline — while the structural record of
 *   victims and beneficiaries is authored as observer-grade fact. The gap
 *   between the reading's self-assessment and the victim seats' computed
 *   position is the measurement this story exists to take, not an error to
 *   reconcile. KEY AGENTS (by structural relationship): -
 *   centralizing_imperial_authority: agenda-setter and primary beneficiary
 *   (institutional/arbitrage) — defines doctrine, directs destruction,
 *   collects and redistributes confiscated wealth; can reverse the policy by
 *   decree - monastic_communities: primary target (organized/constrained) —
 *   bears confiscation, persecution, and exile; the settlement's chief
 *   institutional opponent - iconophile_laity: diffuse target
 *   (powerless/trapped) — devotional life criminalized; no seat, no exit -
 *   icon_producers: secondary target (moderate/constrained) — occupation
 *   criminalized; production survives underground and abroad -
 *   iconoclast_installed_clergy: secondary beneficiary
 *   (institutional/constrained) — holds office under the settlement; recants
 *   or falls with it - imperial_army: secondary beneficiary and enforcement
 *   backbone (institutional/constrained) — receives donatives and estates -
 *   western_church_rome: excluded objector (institutional/arbitrage) —
 *   outside enforcement reach; standing objection the settlement never
 *   answered
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.3).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.85).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Iconoclast Reading — Universal Prohibition of Religious Imagery in Worship").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious authority/visual culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, 'a376b528-441d-4977-a210-f500f09352cb').
narrative_ontology:cs_kernel_codification('a376b528-441d-4977-a210-f500f09352cb', fixed_text).
narrative_ontology:cs_authority_grounding('a376b528-441d-4977-a210-f500f09352cb', extraction).
narrative_ontology:cs_interpretation_layer_present('a376b528-441d-4977-a210-f500f09352cb').
narrative_ontology:cs_reading_relation('a376b528-441d-4977-a210-f500f09352cb', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('a376b528-441d-4977-a210-f500f09352cb', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('a376b528-441d-4977-a210-f500f09352cb', foundational, all_worship_imagery_is_idolatry).
narrative_ontology:cs_axiom_status(all_worship_imagery_is_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('a376b528-441d-4977-a210-f500f09352cb', all_worship_imagery_is_idolatry, theological).
narrative_ontology:cs_axiom('a376b528-441d-4977-a210-f500f09352cb', foundational, divine_nature_uncircumscribable).
narrative_ontology:cs_axiom_status(divine_nature_uncircumscribable, holdable).
narrative_ontology:cs_axiom_grounding('a376b528-441d-4977-a210-f500f09352cb', divine_nature_uncircumscribable, theological).
narrative_ontology:cs_axiom('a376b528-441d-4977-a210-f500f09352cb', secondary, imperial_doctrinal_guardianship).
narrative_ontology:cs_axiom_status(imperial_doctrinal_guardianship, holdable).
narrative_ontology:cs_axiom_grounding('a376b528-441d-4977-a210-f500f09352cb', imperial_doctrinal_guardianship, conventional).
narrative_ontology:cs_reference_frame('a376b528-441d-4977-a210-f500f09352cb', apostolic_imageless_worship_standard).
narrative_ontology:cs_drift_state('a376b528-441d-4977-a210-f500f09352cb', post_synodikon_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a376b528-441d-4977-a210-f500f09352cb', '2026-08-05T15:47:12Z').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_installed_clergy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, imperial_army).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, iconophile_laity).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, literalist_second_commandment_exegesis).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, caesaropapist_doctrinal_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the imperial office and with it the power to define doctrine for the church it governs. Issues the edicts ordering images removed from churches, directs their destruction, appoints and dismisses patriarchs, and tries dissenters in its own courts. Confiscated monastic estates and liturgical wealth flow into the treasury and out again as donatives to the army and grants to loyal clergy. The policy can be reversed by the same office that issued it, and was twice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Occupy bishoprics and the patriarchate under the settlement that removed images. Preach and administer the imageless liturgy; their tenure depends on continuing imperial favor and on the settlement holding. When the settlement falls they must subscribe to the restored doctrine to keep their sees, as many did in 787, or be deposed, as many were in 843.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_installed_clergy, beneficiary,
    institutional, biographical, constrained, continental).

% Keep icons at the center of their devotion, and their houses hold the empire's great collections of panels and frescoes. They refuse to surrender them, shelter venerators, and produce the movement's martyrs and polemicists. They suffer seizure of estates, torture and mutilation of monks, exile to the periphery, and dispersal of their communities; their wealth and their hold on popular devotion make them the settlement's chief institutional opponents.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    organized, generational, constrained, continental).

% Paint panels, lay mosaics, and fresco churches for devotional use. Under the edicts their occupation becomes contraband: workshops are seized, finished works destroyed or whitewashed over, and practitioners punished — the monk-painter Lazarus had his hands burned, by the tradition's account. Some continue underground or relocate to Italy and the western provinces where imperial writ does not run.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    moderate, biographical, constrained, continental).

% Order their devotional life around images in churches and household icons kept behind doors. The edicts criminalize the practice they were formed in; they hide icons, venerate in private, and take the risks of possession. They have no seat in any council and no exit from the jurisdiction that forbids them; individually they are exposed to informers and penalties.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconophile_laity, payer,
    powerless, biographical, trapped, continental).

% Provides the settlement's enforcement backbone and receives much of what the confiscations yield, in estates and donatives. Its eastern-front experience of defeat and plague is read at court as divine displeasure at image veneration, which makes the rank and file receptive to the policy. Its loyalty is court-specific: when a new dynasty needs the church reconciled, the army's stake is bought out or overridden.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, imperial_army, beneficiary,
    institutional, biographical, constrained, continental).

% Rejects the iconoclast councils, refuses communion with bishops who hold the settlement, and receives exiled monks. It sits outside the emperor's coercive reach, so the edicts cannot touch it; its opposition is a standing objection the settlement never answered, and the estrangement it records deepens the divide that later crises widen.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, western_church_rome, excluded,
    institutional, generational, arbitrage, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconoclast_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes worship across the empire under a single court-defined doctrine of the second commandment, replacing dispersed devotional variation with a uniform imageless liturgy; concentrates the definition of orthodoxy in the palace and the councils it convenes rather than in monastic centers; and gives the court a read on military catastrophe — defeat and plague as divine displeasure at images — that the policy claims to answer.
% TRANSFER_FUNCTION: Moves confiscated monastic estates, liturgical objects, and workshop inventories from monastic communities and image-producing households to the imperial treasury, the army (as estates and donatives), and the settlement clergy (as sees and patronage); moves devotional authority from dispersed shrines, monasteries, and household practice to the palace and the installed episcopal hierarchy.
% ABSENT_VOICES: The iconophile confessors — the monks imprisoned, mutilated, and killed before and after Hieria — had no seat in the council that defined the settlement's doctrine; the laity whose devotion the edicts targeted were addressed by proclamation, not consulted; Rome and the eastern patriarchates, which rejected the settlement, were outside the emperor's convocation power and absent by force. Their objections survive in hagiography, in the acts of Nicaea II, and in Rome's refusal of communion with iconoclast bishops.
% DISAPPEARANCE_RATIONALE: Demonstrated twice: within months of each enforcement collapse icons returned to the churches, production resumed, and the confiscated devotional economy reconstituted itself. What depended on the settlement was the installed episcopal hierarchy, the property distributions, and the court's doctrinal monopoly — all unwound quickly once enforcement stopped. The world the settlement governed rearranged itself around the restored images, not around the prohibition.
% FOUNDING_PROBLEM: A court facing military catastrophe and plague read both as divine displeasure at image veneration, and a genuine exegetical dispute over the second commandment's scope ran alongside. The settlement was built to secure divine favor by purging worship of images and to settle the commandment's meaning by imperial definition.
% FOUNDING_PROBLEM_CORROBORATION: No surviving source outside the iconoclast party attests the founding problem as the settlement framed it. The iconophile acts, the hagiographies of the persecuted, and the received acts of Nicaea II attest the problem as the court's military-exegetical anxiety and reject its premise — that image veneration forfeits divine favor — and Rome's refusal of communion with the iconoclast councils attests from outside the imperial settlement entirely. The corroboration that exists corroborates against the founding problem, which is itself the signal.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).
:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.3) is authored from the reading's own lights over the fixed referent of the iconoclast settlement: the reading counts the arrangement's imposed costs — destroyed property, seized estates, punished persons — as the price of enforcing divine prohibition, and denies their rent character; the scalar characterizes the settlement's operation across the interval, not its corpse at 843. The measurement series keeps the same reading-indexed lens and shows the enforcement cycle the historical record fixes: costs rise with the Hieria persecution, drop at the 787 restoration, rise again under Theophilos, and end near zero when the settlement is abolished. Suppression (0.85) is authored as a raw structural property — unscaled by power or scope, per the engine's rule — because the settlement's persistence depended on systematic coercion: destruction details, torture and mutilation of monks, exile, forced subscription of the clergy. Theater (0.2) is low while enforcement is functional and rises only as function collapses (0.6 at 843, when what remains of the settlement's activity is commemoration). Accessibility collapse (0.65): the edicts suppressed the alternatives hard within imperial jurisdiction but never eliminated them — veneration persisted in homes, production survived in the lauras and in Italy, and the practice returned intact within months of each enforcement lapse. Resistance (0.8): the settlement met a century of organized opposition, and the victims' coalition — monastic networks, iconophile laity, Rome, and court women — materialized twice and abolished the arrangement from inside the palace; individually powerless agents were never the operative unit. The claimed type is snare: the coordination story (uniform imageless worship, doctrinal purity) was contested inside the empire, produced division rather than coordination, and functioned as the settlement's legitimating cover while the operative flows ran from monastic treasuries to the palace, the army, and the installed hierarchy; persistence depended wholly on coercion and collapsed twice when coercion stopped. The measurement grid is one shared set of eight points with all three tracked metrics authored at every point. The oscillation is documented as possibly mechanistic — intermittent reinforcement of imperial leverage over religious life — and carries its own omega rather than being assumed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the imperial seat the settlement is pious governance and fiscal-military consolidation: doctrine defined, worship standardized, resources moved to the frontier army. From the monastic seat it is dispossession and martyrdom: estates seized, bodies mutilated, communities scattered. From the laity's seat it is the criminalization of the devotional life they were formed in. From Rome's seat it is heresy imposed by force and a standing cause of estrangement. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate between them. The reading-indexed epsilon (0.3) is the iconoclast seat's own assessment; the victim seats' computed extraction will sit far higher, and that divergence — a reading that endorses an arrangement whose victims the structural record names — is the datum.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (imperial authority, installed clergy, army) derive low directionality — the settlement subsidizes them; the victim declarations (monasteries, producers, laity) derive high directionality, amplified for the trapped laity and the constrained monastics whose exit is costly flight to the periphery or abroad. Rome sits near the beneficiary end despite appearing in neither array: its arbitrage-grade exit (outside imperial writ) places it beyond the extraction's reach, and its exclusion role records that it was never in the settlement's conversation. No directionality overrides are authored: the derivation from roles, power, and exit produces the right structure without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem (securing divine favor by purging worship of images, and settling the commandment's meaning by imperial definition) died with the arrangement — the settlement was abolished in 843, its doctrine formally anathematized annually thereafter, rather than persisting as performance after its mandate expired. The R5 mismatch consumer will see founding_problem_status=dead against disappearance_verdict=world_rearranges and raise the capture/zombie flag; the cross-check against the theater path clears it — the theater series rises to 0.6 only at the interval's end, where the arrangement's residue is commemoration of its abolition, and the arrangement itself was dismantled, not maintained. The fixer-relative cost class matters here: reversal was cheap for a determined emperor (Irene in 787, Theodora in 843) and prohibitive for a coalition emperor (Constantine V, whose army and hierarchy were built on the settlement) — the arrangement persisted exactly as long as its fixers willed it, which is the snare signature, not the piton's.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates the iconoclast reading of kernel decalogue_image_prohibition; what would the iconodule sibling change structurally, and where exactly is the disagreement located?',
    'Not resolvable inside this story: resolved only by which reading a commitment framework adopts. The sibling story (iconodule_reading) carries its own epsilon, victim set, and classification over the shared historical referent.',
    'Under the iconodule reading the victim and beneficiary sets invert — the enforced prohibition itself becomes the contested arrangement and the venerated image becomes lawful matter — and the same historical material classifies differently. The disagreement is located in the semantic scope of ''image'' and in whether the Incarnation sanctifies matter as a conduit to the divine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    sincere_theology_vs_extraction_cover,
    'How much of the iconoclast program was sincerely held exegetical conviction, and how much was cover for wealth transfer and the consolidation of court control over the church?',
    'Compare the argumentation of the Hieria acts (genuine patristic engagement) against fiscal records of confiscation flows, army donatives, and the timing of seizures relative to doctrinal milestones; prosopography of who drafted doctrine versus who collected.',
    'Extraction-dominant confirms the snare claim; theology-dominant with incidental extraction shifts the classification toward tangled_rope, with the coordination function genuine but asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_theology_vs_extraction_cover, empirical, 'Sincerity of the doctrinal rationale versus its fiscal-political function.').

omega_variable(
    army_stake_in_settlement,
    'How dependent was the settlement''s persistence on the army''s material stake in the confiscations and on its eastern-front piety?',
    'Prosopography of promotions, purges, and donatives under Constantine V and Theophilos; analysis of the military context of Leo V''s 815 restoration of iconoclasm.',
    'High dependence explains the 815 revival, makes fixing_cost fixer-relative (cheap for Irene and Theodora, prohibitive for a coalition emperor), and identifies the enforcement backbone whose acquiescence preceded each abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(army_stake_in_settlement, empirical, 'The army''s material and pietistic stake in the settlement''s persistence.').

omega_variable(
    intermittent_reinforcement_cycle,
    'Did the persecution-restoration-persecution cycle function as intermittent reinforcement — each swing demonstrating that the religious settlement was contingent on imperial will — or was it contingent dynastic accident?',
    'Compare imperial leverage over episcopal appointments, monastic wealth, and doctrinal definition before 726, between the cycles (787-815), and after 843; if leverage peaked between cycles, the oscillation was mechanistic.',
    'If mechanistic, the oscillation is part of the settlement''s operating apparatus rather than noise, and the classification is stable across the cycle; if accidental, the cycle is exogenous and the settlement''s structure is read from its enforcement phases alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intermittent_reinforcement_cycle, empirical, 'Whether the enforcement cycle was mechanism or accident.').

omega_variable(
    underground_persistence_scale,
    'How complete was the suppression of icon veneration in practice — did household and monastic veneration persist at scale under the edicts?',
    'Archaeology of household finds, icon stamps, and lead seals across the iconoclast periods, read against hagiographic evidence of hidden icons and private veneration.',
    'Substantial persistence lowers the honest accessibility_collapse below the authored 0.65 and raises measured resistance; near-total suppression would support the higher collapse value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(underground_persistence_scale, empirical, 'Scale of underground persistence of veneration under the edicts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 726, 843).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t726, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 726, 0.15).
narrative_ontology:measurement(deca_tr_t740, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 740, 0.15).
narrative_ontology:measurement(deca_tr_t754, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 754, 0.25).
narrative_ontology:measurement(deca_tr_t768, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 768, 0.25).
narrative_ontology:measurement(deca_tr_t787, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 787, 0.35).
narrative_ontology:measurement(deca_tr_t815, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 815, 0.2).
narrative_ontology:measurement(deca_tr_t830, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 830, 0.25).
narrative_ontology:measurement(deca_tr_t843, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 843, 0.6).

% Extraction over time
narrative_ontology:measurement(deca_be_t726, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 726, 0.28).
narrative_ontology:measurement(deca_be_t740, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 740, 0.32).
narrative_ontology:measurement(deca_be_t754, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 754, 0.52).
narrative_ontology:measurement(deca_be_t768, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 768, 0.58).
narrative_ontology:measurement(deca_be_t787, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 787, 0.22).
narrative_ontology:measurement(deca_be_t815, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 815, 0.38).
narrative_ontology:measurement(deca_be_t830, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 830, 0.46).
narrative_ontology:measurement(deca_be_t843, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 843, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t726, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 726, 0.45).
narrative_ontology:measurement(deca_su_t740, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 740, 0.55).
narrative_ontology:measurement(deca_su_t754, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 754, 0.9).
narrative_ontology:measurement(deca_su_t768, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 768, 0.85).
narrative_ontology:measurement(deca_su_t787, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 787, 0.2).
narrative_ontology:measurement(deca_su_t815, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 815, 0.6).
narrative_ontology:measurement(deca_su_t830, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 830, 0.75).
narrative_ontology:measurement(deca_su_t843, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 843, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% The kernel decalogue_image_prohibition decomposes into three structurally distinct constraints — the iconoclast reading (this story), the moderate iconoclast reading (statuary forbidden, regulated two-dimensional images permitted), and the iconodule reading (latria forbidden, dulia through images permitted; matter sanctified by the Incarnation). Each has its own epsilon, victim set, and enforcement structure over the shared historical referent; this story instantiates the iconoclast reading alone. The iconoclast settlement's enforcement history is the referent the sibling readings assess: the persecution generated the martyrology and arguments the iconodule settlement was built on, and the total prohibition's demonstrated unenforceability is the pressure that produced the moderated position. Linked here via affects_constraints and in cs_structure.reading_relations; the sibling stories carry the reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
