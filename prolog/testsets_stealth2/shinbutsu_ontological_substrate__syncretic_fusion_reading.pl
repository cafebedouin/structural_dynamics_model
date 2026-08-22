% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Ontological Unity - Syncretic Fusion Reading
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the syncretic_fusion_reading of the contested
 *   kernel shinbutsu_ontological_substrate: the claim, load-bearing in the
 *   Japanese medieval religious order from roughly the tenth century to 1868,
 *   that kami and buddhas are one reality in two modes of manifestation - the
 *   kami as traces (suijaku) of buddha originals (honji) - and that this is
 *   metaphysical truth rather than institutional convenience. The standing
 *   arrangement under contest is the honji suijaku complex itself: the engi
 *   literature, shrine-temple integration, court and bakufu patronage, and
 *   the enforcement machinery that kept alternative theologies marginal.
 *   Constraint family: this file is one of three linked readings of the same
 *   kernel. The domain_partition_reading authors the claim that kami and
 *   buddhas merely govern separate domains (functional coexistence, no
 *   ontological unity); the incoherent_bundle_reading authors the claim that
 *   no coherent kernel exists at all - syncretism as accumulated
 *   institutional drift under state enforcement. Each sibling carries its own
 *   epsilon over the same standing arrangement; this reading, taking the
 *   unity as real, authors comparatively low reading-indexed extraction while
 *   acknowledging the enforcement record. The siblings are linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   tendai_shingon_monastic_complexes: Agenda-setting beneficiary
 *   (institutional/identity_locked) - produces the doctrine, administers the
 *   cultic economy, collects its revenues - imperial_court_aristocracy:
 *   Patron-beneficiary (powerful/constrained) - funds the unified cult,
 *   receives legitimation - warrior_governments: Patron-beneficiary
 *   (institutional/constrained) - enforces the cultic peace, draws
 *   legitimation from it - shrine_priestly_lineages: Compensated subordinate
 *   (organized/identity_locked) - gains standing, cedes doctrinal sovereignty
 *   - autonomous_kami_cult_practitioners: Primary target (powerless/trapped)
 *   - bears subordination of cult and offerings -
 *   kami_supremacist_theologians: Intellectual target
 *   (moderate/identity_locked) - marginalized for teaching the inverse
 *   hierarchy - kokugaku_confucian_critics: Outside critic
 *   (moderate/constrained) - attacks the doctrine's fabric, eventually
 *   inherits the state - women_excluded_from_sacred_sites: Excluded voice
 *   (powerless/trapped) - barred from the rites that enact the unity -
 *   modern_historians_of_japanese_religion: Analytical observer
 *   (analytical/analytical) - reconstructs the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.28).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.55).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, mountain).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Honji Suijaku Ontological Unity - Syncretic Fusion Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).
domain_priors:emerges_naturally(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, '5d0d0037-0c10-4867-8e65-1eb259defb01').
narrative_ontology:cs_kernel_codification('5d0d0037-0c10-4867-8e65-1eb259defb01', formalized).
narrative_ontology:cs_authority_grounding('5d0d0037-0c10-4867-8e65-1eb259defb01', lineage).
narrative_ontology:cs_interpretation_layer_present('5d0d0037-0c10-4867-8e65-1eb259defb01').
narrative_ontology:cs_reading_relation('5d0d0037-0c10-4867-8e65-1eb259defb01', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('5d0d0037-0c10-4867-8e65-1eb259defb01', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('5d0d0037-0c10-4867-8e65-1eb259defb01', foundational, kami_buddha_same_essence_two_manifestations).
narrative_ontology:cs_axiom_status(kami_buddha_same_essence_two_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('5d0d0037-0c10-4867-8e65-1eb259defb01', kami_buddha_same_essence_two_manifestations, theological).
narrative_ontology:cs_axiom('5d0d0037-0c10-4867-8e65-1eb259defb01', foundational, honji_suijaku_describes_reality_not_arrangement).
narrative_ontology:cs_axiom_status(honji_suijaku_describes_reality_not_arrangement, holdable).
narrative_ontology:cs_axiom_grounding('5d0d0037-0c10-4867-8e65-1eb259defb01', honji_suijaku_describes_reality_not_arrangement, theological).
narrative_ontology:cs_reference_frame('5d0d0037-0c10-4867-8e65-1eb259defb01', honji_suijaku_metaphysical_truth).
narrative_ontology:cs_drift_state('5d0d0037-0c10-4867-8e65-1eb259defb01', post_meiji_separation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('5d0d0037-0c10-4867-8e65-1eb259defb01', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, tendai_shingon_monastic_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_aristocracy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, warrior_governments).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, shrine_priestly_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, autonomous_kami_cult_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, kami_supremacist_theologians).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, kokugaku_confucian_critics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_aristocracy).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, warrior_governments).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, shrine_priestly_lineages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the great temple-shrine complexes (Enryakuji, Onjoji, Toji, and their network branches) that produced and administered the doctrine: monks compose the engi narratives assigning each kami a buddha-original, staff shrine liturgies, ordain shrine priests into Buddhist orders, and maintain armed contingents that defend institutional prerogatives. Revenue flows in through offerings, estate income, and ritual fees; doctrinal authority over the kami flows from the lecture halls. The complexes are built physically and ritually around paired shrines and temples, so their institutional self-understanding is inseparable from the unity teaching.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, tendai_shingon_monastic_complexes, agenda_setter,
    institutional, generational, identity_locked, national).

% Patronizes the unified cult as the sacral frame of imperial rule: the identification of the sun-line ancestress with the Cosmic Buddha elevates the throne above both cults at once. The court funds the complexes through land grants and festival stipends and receives legitimation, rain-making, and protective rites in return. Its room for maneuver is bounded by precedent and by the clerical networks it finances.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_aristocracy, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_aristocracy, payer).

% Governs through the cultic order rather than against it: the bakufu confirms estate holdings, mediates temple-shrine disputes, and draws on protector-deity cults such as Hachiman for military legitimation. It pays in land confirmations and enforcement services and collects governance stability in return. It retains enough independence to play the establishments against each other, but abandoning the cultic frame would cost it the legitimating vocabulary of rule.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, warrior_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, warrior_governments, payer).

% Hereditary priest houses at integrated shrines (Kasuga, Iwashimizu, Kitano, and hundreds of provincial counterparts) conduct daily rites for their kami while accepting the assigned buddha-originals, hosting monastic lecturers, and routing ordination and state recognition through the temple system. They receive festival funding, legal standing, and salvific prestige; they cede final doctrinal say over their own deities. Leaving the framework would mean forfeiting recognition and the ritual economy attached to it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shrine_priestly_lineages, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, shrine_priestly_lineages, payer).

% Village and local communities whose deities were absorbed as manifestations: their festivals continue but are re-narrated as devotion to a buddha's trace, their shrines acquire resident monks or temple halls, and their offering streams are shared with or redirected to the complex. They have no literate voice in the doctrinal record and no realistic path to practicing outside the integrated landscape, since the sacred sites themselves are physically shared.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, autonomous_kami_cult_practitioners, payer,
    powerless, biographical, trapped, local).

% Scholar-priest houses, notably the Watarai line at Ise and later the Yoshida house, that teach the kami as original and prior rather than as traces. Their writings circulate under restriction, their careers advance only through accommodation with the monastic establishment, and their frameworks survive as minority inversions tolerated at the margins of the orthodox lecture circuit.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, kami_supremacist_theologians, payer,
    moderate, generational, identity_locked, national).

% Confucian advisors and National Learning scholars who attack the unity teaching as monkish fabrication layered over the ancient texts. They sit outside the cultic economy's payroll, publish at the edge of official tolerance, and for centuries command no enforcement power; their critique accumulates in samurai reading circles until, in the 1860s, their intellectual heirs hold the levers of the restoration state.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, kokugaku_confucian_critics, payer,
    moderate, generational, constrained, national).

% Women devotees barred by perimeter prohibitions (nyonin kekkai) from the mountain ascetic routes and certain shrine precincts where the unity of kami and buddha is ritually enacted. They fund pilgrimages, sustain household observance, and transmit festival practice, but cannot enter the sites where the doctrine's authority is performed, and they have no institutional channel through which to contest the prohibition.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, women_excluded_from_sacred_sites, excluded,
    powerless, biographical, trapped, regional).

% Academic specialists who reconstruct the doctrine's formation, enforcement, and collapse from engi corpora, estate records, edict archives, and missionary reports. They take testimony from every surviving textual seat, distinguish doctrinal claim from institutional effect, and publish analyses that later generations use to adjudicate what the arrangement actually was.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, modern_historians_of_japanese_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, tendai_shingon_monastic_complexes).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrated an indigenous territorial cult landscape with an imported universal salvation religion: shared ritual calendars, shrine-temple complexes (jinguji), protective-deity assignments (chinju), a unified festival, funerary, and healing economy, and a single legitimation stream for court and warrior rule - solving, without sectarian war, the integration problem that produced confessional violence elsewhere.
% TRANSFER_FUNCTION: Moved doctrinal authority and interpretive sovereignty over the kami from shrine lineages to the Buddhist monastic establishment; moved material resources (offerings, tithes, land grants, labor) from local worship communities and the court upward into the temple-shrine complexes; moved legitimation and salvific assurance back outward to the regime and the laity.
% ABSENT_VOICES: Women barred by nyonin kekkai from the mountain rites where the unity was enacted; village kami-worshipers without literate representation in the doctrinal record; kami-supremacist theologians outside the Tendai-Shingon lecture circuit. Their shared objection - that the unity functioned as a hierarchy wearing unity's clothes - survives mainly in marginal texts and post-separation testimony.
% DISAPPEARANCE_RATIONALE: Within months of the 1868 separation edicts, shrine and temple were legally divorced, buddha-figures were ejected from shrine precincts, thousands of temples were destroyed in the haibutsu kishaku wave, hereditary priesthoods were reorganized, and State Shinto was erected on the cleared ground. The entire cultic economy - parish registration, funerary affiliation, festival finance, mountain asceticism - rearranged around the doctrine's absence, demonstrating how much had been organized on top of it.
% FOUNDING_PROBLEM: How to hold the imported buddhadharma and the indigenous kami cult as jointly true, and how to integrate a universal salvation religion with territorial cults under a single court-and-warrior order without religious civil war.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: sixteenth-century Jesuit observers (Valignano, Frois) recorded the integration as clerical strategy; Neo-Confucian advisors (Hayashi Razan) and National Learning scholars (Motoori Norinaga) attacked the doctrine as monkish fabrication while conceding that the integration problem it answered was real; modern secular historiography (Kuroda Toshio's kenmitsu taisei analysis) corroborates both the problem's reality and the doctrine's service to institutional interests. No corroborating source outside the beneficiary set attests the metaphysical resolution itself - only the problem.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, ExtMetricName, E),
    domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(shinbutsu_ontological_substrate__syncretic_fusion_reading),
    narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is mountain because this reading's own lights hold the unity to be metaphysical truth - the natural-law analog within a commitment system - and emerges_naturally is set accordingly. The metrics describe the arrangement's actual operation: extractiveness 0.28 is reading-indexed (the reading denies the extraction frame and books material asymmetry as institutional accretion rather than as the doctrine's own yield); suppression 0.55 is a raw structural property, unscaled by power or scope, reflecting the enforcement machinery (armed monastic contingents, court edicts against anti-syncretic movements, the Tokugawa danka/terauke parish-registration system that compelled Buddhist funerary affiliation); theater_ratio 0.20 stays low because the arrangement remained functionally load-bearing - ritual, legitimation, and integration were real work, not performance - until external force ended it. Accessibility_collapse 0.62: once the framework is accepted, kami-autonomous theology becomes nearly inarticulable in official discourse, yet inversion movements (Watarai, Yoshida) kept alternatives alive at the margins, so collapse is substantial but incomplete. Resistance 0.58: sustained intellectual resistance from Pure Land exclusivists, Confucian advisors, and kokugaku scholars, plus periodic institutional pushback, accumulating until the Meiji coalition destroyed the framework outright. The measurement series run on one shared seven-point grid (900-1868) so every metric is authored at every examined time point; extractiveness dips slightly at 1700 as Tokugawa rationalization trimmed some fee structures even as compulsory registration deepened. Identity-coordination gaming alert: 'this is our culture's deepest commitment' is precisely the identity cover-story pattern, and the conservative floor for identity_coordination keeps the excess visible rather than excusing it. Coalition note: the powerless-atom victims lacked coalition capacity for centuries; the eventual victorious coalition formed only when literate elite critics (kokugaku, Mito) defected to the restoration side.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the monastic complexes' position the arrangement is the truth they administer: subordination of kami to buddha-originals is the order of reality, and enforcement is error-correction. From the trapped village-practitioner seat and the marginalized theologian seat the same structure operates as enforced subordination of their deities and the suppression of their theologies. The court and bakufu seats sit nearer symmetric: they paid land and enforcement and collected legitimation and governance stability. The shrine-priest seat splits internally - compensated standing against surrendered doctrinal sovereignty - which is why the organized power atom carries a directionality override. The engine computes these per-seat classifications from the structural data; the authored mountain claim does not adjudicate them, and the gap between the reading's claim and the computed seats is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The monastic complexes are the structural beneficiary and agenda-setter (collect revenues and doctrinal authority, d near the beneficiary end). Court and warrior governments are patron-beneficiaries with payer secondary roles (fund and enforce, receive legitimation; constrained exit because the cultic frame supplies their legitimating vocabulary). Autonomous kami-cult practitioners are targets: trapped by physically shared sacred sites and lack of literate advocacy, d near the target end. Kami-supremacist theologians and kokugaku-Confucian critics are targets with somewhat more mobility and voice. Women excluded from sacred sites bear the arrangement's perimeter costs without any channel of participation. Override: the organized power atom (shrine_priestly_lineages) would derive a low d from its beneficiary declaration alone, but its net position is compensated subordination - it gained ordination, recognition, and festival funding while ceding final doctrinal say over its own deities and routing authority through the temple system. This is the capture-analog case: the agent appears as beneficiary but pays through the same structure, so the override lifts d to 0.42.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy here in the atrophy sense, and the classification guards against two misreadings. First, against piton: the theater ratio stayed low across nine centuries because the arrangement kept performing real integrative work; it was not maintained performatively after its function died. Second, against pure extraction: the coordination function was genuine - Japan crossed seven centuries of dual-cult existence without confessional war, and the unified cultic economy delivered ritual, funerary, and healing services at scale. The founding problem (integration of universal salvation religion with territorial cult under a single court-and-warrior order) is dead: the Meiji state solved the integration question differently, by forcible separation and the erection of State Shinto, and the arrangement was terminated by external political force while still functionally load-bearing rather than left to atrophy. The mismatch consumer should read dead-problem plus world-rearranges as coherent termination of a working structure, not as a zombie flag: nothing persisted theatrically after 1868.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_unity,
    'Is the kami-buddha ontological unity a genuine metaphysical structure that the honji suijaku arrangement faithfully expresses, or a constructed doctrine whose form tracks the interests of the institutions that enforced it?',
    'Comparative doctrinal analysis testing whether the unity claim''s content varies with institutional advantage (reassignments of kami-buddha pairings following patronage shifts), plus assessment of whether any formulation of the unity survives stripping away enforcement-dependent elements.',
    'If constructed, the false-summit signature resolves toward the enforced hybrid the sibling readings describe and the mountain claim fails; if genuine, the reading''s mountain claim stands and the measured extraction reads as implementation imperfection rather than design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_unity, conceptual, 'Whether the unity is discovered truth or institutional construction.').

omega_variable(
    kernel_coherence_under_textual_strain,
    'Does a single coherent ontological kernel run through the engi corpora and doctrinal commentaries, or is the appearance of coherence a retrospective projection of the syncretic reading onto locally inconsistent pairings and reassignments?',
    'Systematic textual survey of kami-buddha identifications across regions and centuries, scoring consistency of the underlying principle against ad hoc variation.',
    'If no coherent kernel survives, this reading collapses into the incoherent_bundle_reading and the constraint reclassifies as an enforced institutional artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_coherence_under_textual_strain, empirical, 'Whether the transmitted doctrine constitutes one kernel or many local arrangements.').

omega_variable(
    conviction_interest_fusion,
    'How much of the establishment''s resistance to separation was ontological conviction, and how much institutional interest fused with conviction beyond separability?',
    'Trace cases where interest and doctrine diverged - sects that dropped honji suijaku identifications when patronage shifted - and test whether any retained the ontology without the revenue.',
    'If interest dominates, the enforcement record was rent defense and effective extraction runs higher than the reading-indexed measure; if conviction dominates, the arrangement''s costs were borne for the truth as its holders understood it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conviction_interest_fusion, empirical, 'Separability of belief from institutional payoff in the resistance record.').

omega_variable(
    suppression_structural_vs_internalized,
    'Was the marginality of kami-supremacist and anti-syncretic theology maintained by structural coercion (court edicts, monastic force, bakufu temple registration) or by internalized acceptance of the trace-framework?',
    'Post-1868 natural experiment: track how quickly kami-buddha identifications and combined rites lapsed once enforcement was removed, parish by parish.',
    'Rapid lapse indicates structural maintenance and supports the lower suppression reading; durable voluntary persistence indicates internalization and raises effective suppression above the structural measure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized maintenance of doctrinal exclusivity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 900, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_fusion_tr_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1100, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1100, 0.1).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1250, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1250, 0.13).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1400, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1550, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1550, 0.17).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1700, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1700, 0.19).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1868, 0.2).

% Extraction over time
narrative_ontology:measurement(shinbutsu_fusion_be_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 900, 0.12).
narrative_ontology:measurement(shinbutsu_fusion_be_t1100, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1100, 0.18).
narrative_ontology:measurement(shinbutsu_fusion_be_t1250, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1250, 0.23).
narrative_ontology:measurement(shinbutsu_fusion_be_t1400, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1400, 0.26).
narrative_ontology:measurement(shinbutsu_fusion_be_t1550, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1550, 0.29).
narrative_ontology:measurement(shinbutsu_fusion_be_t1700, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1700, 0.27).
narrative_ontology:measurement(shinbutsu_fusion_be_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1868, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_fusion_su_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 900, 0.14).
narrative_ontology:measurement(shinbutsu_fusion_su_t1100, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1100, 0.26).
narrative_ontology:measurement(shinbutsu_fusion_su_t1250, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1250, 0.38).
narrative_ontology:measurement(shinbutsu_fusion_su_t1400, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1400, 0.46).
narrative_ontology:measurement(shinbutsu_fusion_su_t1550, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1550, 0.5).
narrative_ontology:measurement(shinbutsu_fusion_su_t1700, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1700, 0.57).
narrative_ontology:measurement(shinbutsu_fusion_su_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1868, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'shinbutsu shugo / honji suijaku' conflates three structurally distinct claims, so the kernel shinbutsu_ontological_substrate decomposes into three stories. This file authors the syncretic_fusion_reading - the ontological-unity claim taken as metaphysical truth - with a low-to-moderate reading-indexed epsilon (0.28) over the standing arrangement. The domain_partition_reading authors the functional-coexistence claim (no ontological unity; separate domains), and the incoherent_bundle_reading authors the no-kernel claim (accumulated institutional drift under state enforcement), which carries a substantially higher epsilon keyed to the enforcement record. Upstream/downstream: the fusion reading was the establishment's own upstream legitimacy claim, cited as self-evident by the institutions it authorized; the bundle reading is the downstream modern decomposition that explains the fusion claim's content as institutional residue. Each member links the others through affects_constraints; no story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
