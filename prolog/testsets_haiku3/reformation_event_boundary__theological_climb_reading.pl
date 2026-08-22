% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Justification by Faith Alone: Theological Doctrinal Breakthrough
 *   domain: religious/epistemological
 *
 * SUMMARY:
 *   This constraint instantiates the theological_climb reading of the
 *   Reformation event: Luther's recovery of justification by faith alone and
 *   the direct authority of scripture over institutional interpretation
 *   constitutes a genuine doctrinal breakthrough — a climb in Christian
 *   understanding — that required institutional separation from the Catholic
 *   Church to persist. The reading posits the Reformation as fundamentally an
 *   innovation in how scripture is read and authority is distributed, not
 *   primarily as a political realignment or institutional collapse. Reformed
 *   believers and the exegetical community are beneficiaries (freed from
 *   false doctrine, gaining direct access to scripture); the Roman Catholic
 *   institutional apparatus is victim (its authority claim is exposed as
 *   unfounded). Extractiveness measures the degree to which the reading
 *   vindicates itself through institutional power (how much the constraint
 *   enforces its interpretation rather than persuading through genuine
 *   clarity). The theological_climb reading claims the core innovation was
 *   doctrinal; the authored metrics reflect extractiveness low but non-zero
 *   because institutional coercion and political exploitation do accumulate
 *   around the doctrinal opening even if the reading denies them causal
 *   primacy.
 *
 * KEY AGENTS:
 *   - reformed_believers: primary beneficiaries freed from false doctrine; exit is available (can return to Catholic or reject both); benefit is epistemological and spiritual
 *   - theological_correctness_advocates (Luther, Calvin, exegetical networks): beneficiaries and agenda-setters; benefit from the reading's authority and can migrate contexts; power derives from intellectual legitimacy
 *   - roman_catholic_institution: primary victim under this reading; institutional delegitimation; constrained exit (cannot leave the game, can only resist/reinterpret/accommodate)
 *   - scholastic_theology_establishment: secondary victim; framework presented as doctrinally deficient; embedded in institutional structures; constrained exit
 *   - secular_princes: excluded from the theological innovation itself; present as secondary actors; would contend they were drivers but this reading subordinates them to doctrine
 *   - historical_observer: analytical seat assessing whether the Reformation was primarily theological, political, or composite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.28).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.15).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Justification by Faith Alone: Theological Doctrinal Breakthrough").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "religious/epistemological").

domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '7fc8b010-afe6-410d-963e-665b5aaf4baa').
narrative_ontology:cs_kernel_codification('7fc8b010-afe6-410d-963e-665b5aaf4baa', fixed_text).
narrative_ontology:cs_authority_grounding('7fc8b010-afe6-410d-963e-665b5aaf4baa', lineage).
narrative_ontology:cs_interpretation_layer_present('7fc8b010-afe6-410d-963e-665b5aaf4baa').
narrative_ontology:cs_reading_relation('7fc8b010-afe6-410d-963e-665b5aaf4baa', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('7fc8b010-afe6-410d-963e-665b5aaf4baa', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('7fc8b010-afe6-410d-963e-665b5aaf4baa', foundational, sola_fide_authentic_recovery).
narrative_ontology:cs_axiom_status(sola_fide_authentic_recovery, holdable).
narrative_ontology:cs_axiom_grounding('7fc8b010-afe6-410d-963e-665b5aaf4baa', sola_fide_authentic_recovery, deontological).
narrative_ontology:cs_axiom('7fc8b010-afe6-410d-963e-665b5aaf4baa', foundational, scripture_direct_authority_primacy).
narrative_ontology:cs_axiom_status(scripture_direct_authority_primacy, holdable).
narrative_ontology:cs_axiom_grounding('7fc8b010-afe6-410d-963e-665b5aaf4baa', scripture_direct_authority_primacy, deontological).
narrative_ontology:cs_reference_frame('7fc8b010-afe6-410d-963e-665b5aaf4baa', apostolic_christianity_framework).
narrative_ontology:cs_drift_state('7fc8b010-afe6-410d-963e-665b5aaf4baa', medieval_institutional_overlay_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7fc8b010-afe6-410d-963e-665b5aaf4baa', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, theological_correctness_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_exegetical_community).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, roman_catholic_institution).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, scholastic_theology_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience liberation from the belief that salvation requires clerical mediation and sacramental participation. Under the old framework, their relationship to God was intermediated through institutional structures; under this reading, faith provides direct access. They gain spiritual clarity and a reframed relationship to authority. Their exit is available: they can return to Catholicism, adopt a different reformation reading, or reject institutional Christianity altogether. The constraint does not trap them; they choose it as clarifying.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformed_believers, beneficiary,
    powerless, generational, mobile, continental).

% Sets and maintains the theological reading: Luther, Calvin, and their intellectual networks produce the exegetical work, train new scholars, and defend the doctrine against Catholic and alternative reform critiques. They benefit from the reading's intellectual authority and can migrate across contexts (university, church, princedom). They produce the textual arguments and institutional frameworks that instantiate sola_fide. Their power derives from scholarly legitimacy and institutional adoption by sympathetic princes and congregations.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformed_exegetical_community, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, reformed_exegetical_community, beneficiary).

% Bears the structural cost of institutional delegitimation. Under this reading, the Church's exclusive claim to sacramental authority and interpretive monopoly is exposed as unfounded: scripture does not require clerical mediation and faith does not require sacramental works. The constraint positions the Church as the bearer of false doctrine, and its institutional survival depends on contestation of the reading or accommodation through reformed doctrine (counter-Reformation theology). The Church cannot exit the game (ceasing to be institutional would mean ceasing to exist as the Church) and its constrained status derives from this structural bind.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, roman_catholic_institution, payer,
    institutional, civilizational, constrained, continental).

% The scholastic apparatus (Aquinas, Scotus, Ockham schools, university theology faculties) is positioned as having obscured scripture's direct meaning through systematic elaboration of intermediary concepts. Under this reading, scholasticism is doctrinally deficient and its institutional bases (university chairs, monastic schools) are delegitimized. Scholastics must defend their framework, migrate to reformed theology, or be displaced. Their exit is limited by institutional embededness: theology faculty positions and monastic authority depend on institutional ecclesiastical support that the reformed reading threatens.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, scholastic_theology_establishment, payer,
    organized, generational, constrained, continental).

% Are structurally present in the political and institutional crisis but absent from the theological reading itself. Under this reading, princes are secondary actors who EXPLOITED the doctrinal opening for political and economic gain (seizing church lands, reducing papal authority, establishing territorial churches). They were not the causal drivers of the theological innovation; they adapted to it. An alternative political_swap reading would place them as primary drivers and frame the theology as rationalization. This reading excludes them from agenda-setting the theological innovation itself and thus from the beneficiary seat for the doctrinal breakthrough.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_princes, excluded,
    powerful, biographical, trapped, continental).

% Are structurally absent from the theological debates and exegetical deliberation. As European Christian settlement is reorganized around the reformed reading, indigenous peoples and Jewish populations experience institutional consequences (displacement, restricted legal status, intensified conversion pressure) that flow from the reformation but have no voice in the theological framing itself. They are excluded from both the old Catholic theology and the new Reformed theology; their absence from the conversation is a fact this reading records but does not resolve.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, indigenous_and_jewish_populations, excluded,
    powerless, generational, trapped, continental).

% Assesses the Reformation from the analytical seat external to all confessional commitments. Takes testimony from believers, Church authorities, princes, exegetes, and competing historians. Examines primary sources and competing interpretations. The analytical question is: which reading of the Reformation event is most consonant with the evidence? Is it the theological_climb (this reading), the political_swap, or the composite_overdetermination? This seat must evaluate competing causal narratives and decide whether the evidence determines one reading or underdetermines the choice.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, historical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified reinterpretation of scripture and Christian doctrine: believers, exegetes, and reformed congregations align around the principles of justification by faith alone and the direct authority of scripture. This replaces the old coordination frame (Church-mediated sacramental salvation) with a new one (faith-accessed direct relationship to divine truth). The coordination solves the problem of scriptural obscuration and divided authority by positing a single principle (faith) that all believers can apply directly without institutional gatekeeping.
% TRANSFER_FUNCTION: Transfers theological authority from the institutional Church (exclusive interpretive power, sacramental monopoly) to individual conscience informed by scripture reading. Transfers the legitimacy claim for Christian practice from sacramental mediation (Church-dependent) to faith commitment (individual-accessible). Transfers the locus of salvation from the Church's institutional apparatus to the relationship between believer and divine word. This is not a material or economic transfer (no money moves) but a structural reallocation of spiritual authority and epistemic legitimacy.
% ABSENT_VOICES: Secular princes who exploit the theological opening for political gain are absent from the theological deliberation itself — they would contend (under the political_swap reading) that they were causal drivers and the theology was their rationalization, not their discovery. Indigenous peoples and Jewish populations are absent from European theological debates and experience institutional consequences of the Christian settlement reorganization without voice in framing it. Catholic authorities would dispute that the founding problem (obscuration of scripture) is real or that sola_fide is a recovery rather than a corruption. Alternative reform readings (Zwinglian, Anabaptist, radical reformation) are suppressed or marginalized; their exponents would contend that the theological_climb reading wrongly monopolizes the exegetical field.
% DISAPPEARANCE_RATIONALE: If this reading had not emerged or gained institutional footing, the European religious world would have remained under unified Catholic institutional authority; believers would continue to experience salvation as mediated through Church structures; princes would not have had theological justification for institutional breaks; Reformed denominations would not have emerged in their historical form. The theological innovation is not marginal to the subsequent reorganization of European Christianity, political authority, and culture.
% FOUNDING_PROBLEM: Medieval Christian practice had developed a complex apparatus of sacramental mediation, clerical interpretation, and institutional gatekeeping that obscured the direct scriptural claim: that justification comes by faith alone and that scripture possesses direct authority over institutional mediation. Believers experienced tension between the lived faith they encountered in reading scripture and the institutional mediation required by the Church's sacramental system. Theological clarity was suppressed under a system that made salvation dependent on hierarchical institutional structures and clerical expertise. The founding problem is the institutional obscuration of authentic Christianity.
% FOUNDING_PROBLEM_CORROBORATION: Luther and Reformed theologians attest the founding problem was real and live — the Church's sacramental system and clerical monopoly obscured scripture's direct meaning and believers' access to grace. The Catholic institutional authority contests this diagnosis: the sacramental system was not obscuring but instantiating authentic Christianity; clerical mediation was not oppressive but salvific. Secular princes attest the founding problem was primarily institutional and political (corrupt Church power, excessive papal authority) rather than theological, and exploited the theological dispute as cover. Historians external to confessional positions dispute whether the problem was primarily theological (as this reading claims) or primarily political/institutional. The counter-Reformation's refined doctrine (claiming to restore Catholic authenticity against corruption) contends the founding problem was institutional corruption, not theological error. No voice from outside the reformed benefiting parties corroborates the sola_fide diagnosis without also having theological commitments at stake in affirming it.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under the theological_climb reading, extractiveness is LOW (0.28 at interval end, rising from zero) because the core claim is that the innovation is genuine and persuasive — a real breakthrough in understanding, not an extractive apparatus wearing doctrinal clothes. However, extractiveness is NOT zero because institutional enforcement inevitably accumulates: the reading must be defended against resistance, princes exploit the doctrinal opening for political gain, alternative readings are suppressed, and eventually Reformed Christianity becomes as institutional and extractive as the system it critiqued. The measurement series models the slow rise: before 1500 the reading barely exists; 1517 marks its public emergence; by 1530 it has institutional footing and institutional suppression of alternatives begins; 1555 (Peace of Augsburg) institutionalizes the split and extractiveness plateaus. Theater_ratio stays very low (0.08 at end) because this reading claims the theological function is real and persistent — Reformed Christianity genuinely instantiates the doctrine it claims, not merely performing it. Suppression_requirement rises but stays low (0.15 at end) because this reading denies that suppression is the primary mechanism of persistence — the reading claims persuasion and clarity are primary, suppression is secondary. The temporal slope models a doctrinal innovation gaining institutional footing and accumulating some extractive apparatus without the reading reinterpreting itself as primarily extractive.
 *
 * PERSPECTIVAL GAP:
 *   This reading claims a theological_climb: from the seat of Reformed believers and exegetical advocates, the Reformation is genuine doctrinal innovation and clarity. From the seat of the Catholic institutional apparatus, the same events are institutional rebellion and doctrinal error. From the seat of secular princes, both theological readings are cover stories for political and economic interest. The theological_climb reading denies the political_swap reading agency: it claims princes EXPLOITED the doctrinal opening rather than DROVE it. The composite_overdetermination reading claims all three readings are simultaneously true and irreducibly interdependent — theology, politics, and institutional restructuring overdetermine the event. The engine computes per-seat classifications from the structural data; where the computed seat-type diverges from this reading's claims, that divergence IS the measurement the corpus exists to take. A seat that computes as snare (pure extraction) under the structural data but claims to be mountain (genuine breakthrough) reveals whether extractiveness has displaced the theological innovation in the actual operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformed believers occupy the beneficiary seat (d near 0.1-0.2): they gain epistemological clarity, experience exit as available (they can return to Catholicism or reject both frameworks), and their baseline power is modest (they are scattered individuals and congregations, not institutional). The exegetical community occupies a dual beneficiary/agenda-setter position (d near 0.3): they set and maintain the reading, benefit from its authority, but they are also organized and mobile. The Catholic institutional apparatus occupies the target seat (d near 0.75-0.85): it bears the cost of institutional delegitimation, its exit is severely constrained (it cannot abandon its institutional form without ceasing to exist), and it is institutional power. Secular princes occupy an excluded seat (not a standard directionality seat): they are present but absent from the theological reading itself. The measurement series holds directionality constant (the structural relationships do not shift over the interval), while extractiveness rises as institutional enforcement and political exploitation accumulate around the doctrinal opening.
 *
 * MANDATROPHY ANALYSIS:
 *   The theological_climb reading denies mandatrophy: it claims the founding problem (obscuration of justification by faith) remains live as the constraint persists (reformed believers continue to experience liberation through direct scripture access; the doctrine remains generatively true). However, post-Reformation institutional Reformed churches accumulate sacramental apparatus, clerical authority, and institutional suppression of alternatives — all the features of the old system. If the founding problem (obscuration of authentic Christianity by institutional overlay) is the constraint's mandate, then Reformed Protestantism's institutionalization represents mandatrophy: the reformation principle is preserved theatrically while the institutional dynamics replicate what was critiqued. The measurement series models this as rising theater_ratio would (if measured across Reformed rather than just Catholic framework) but this reading keeps theater_ratio low by framing institutional structures as legitimate application of the doctrine, not performance masking loss of function. The commentary must record: if the founding problem is live, why has Reformed Christianity not escaped institutional apparatus? If the founding problem is dead (Reformed believers are no longer oppressed by false doctrine), then the constraint's persistence is mandatrophy by definition. This is an omega question, not a mandatrophy resolution in this file.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuineness_of_doctrinal_innovation,
    'Was justification by faith alone a genuine recovery of authentic Christianity lost under medieval overlay, or a plausible-sounding reinterpretation constructed in response to institutional crises and political opportunity?',
    'Textual analysis of early Christian sources (Paul, Augustine, medieval exegetes) to establish whether sola fide represents continuity with or radical departure from the tradition. Historical analysis of whether the exegetical breakthrough preceded or followed political opportunities for institutional break.',
    'If recovery: the theological_climb reading stands; the doctrine is the primary innovation. If reinterpretation: the reading becomes more tangled_rope or snare (construction wearing recovery costume); the political_swap reading gains force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuineness_of_doctrinal_innovation, conceptual, 'Whether the doctrinal innovation is genuine recovery or constructed reinterpretation.').

omega_variable(
    causality_of_institutional_separation,
    'Did the theological innovation require institutional separation, or did princes use doctrinal disputes as cover to break papal authority for political reasons, with the theology constructed to justify the split?',
    'Temporal sequencing: did exegetical innovation precede or follow princes'' institutional breaks? Documentary evidence of princes'' motivations and correspondence. Comparative analysis with other doctrinal controversies that did NOT lead to institutional separation.',
    'If theology-first: the theological_climb reading holds causality. If princes-first: the political_swap reading holds causality; theology is rationalization. If simultaneous and codependent: the composite_overdetermination reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_of_institutional_separation, empirical, 'Whether theological innovation drove institutional separation or political incentives drove theology.').

omega_variable(
    mandatrophy_protestant_institutionalization,
    'If the founding problem was the obscuration of authentic Christianity by institutional apparatus, why has Reformed Protestantism developed its own institutional apparatus (clergy, sacramental refinement, ecclesiastical hierarchy, doctrinal policing)?',
    'Assess whether Reformed institutional structures replicate medieval features or genuinely embody the sola_fide principle. Examine whether Reformed theology can coherently justify its own institutions or treats them as regrettable necessities. Compare Protestant practice to Protestant doctrine.',
    'If Reformed institutions replicate medieval features: the founding problem is not solved, only disguised, and mandatrophy is present. If genuinely different: the doctrine adapted validly to institutional necessity. The constraint may be mandatrophic regardless (founding problem dead but constraint persists), but this question determines whether the reading is coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_protestant_institutionalization, empirical, 'Whether the reformation principle persists generatively in Protestant institutions or suffers mandatrophy.').

omega_variable(
    excluded_narrative_of_secular_princes,
    'Are secular princes rightly excluded from the theological innovation itself (as this reading claims) or were they causal drivers whose theological interests shaped the exegetical framework?',
    'Documentary evidence of princes'' theological commitments vs. strategic deployment of theology. Analysis of which exegetical innovations align with princes'' political interests and which diverge. Testimony from secular actors about their own motivation and agency.',
    'If genuinely excluded: the theological_climb reading holds; princes are secondary exploiters. If causal: the political_swap reading holds; theology is rationalization. If mutual codependency: the composite_overdetermination reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_narrative_of_secular_princes, empirical, 'Whether secular princes were causal drivers or secondary exploiters of theological innovation.').

omega_variable(
    alternative_readings_suppressed,
    'Why did sola_fide and direct scripture authority monopolize the exegetical field among Reform movements, excluding alternative doctrinal formulations? Was this due to the genuine superiority of the reading or due to institutional suppression and political alignment?',
    'History of suppressed or marginalized exegetical alternatives (Zwingli''s semiotic reading, Anabaptist readings, radical reformation variants). Analysis of institutional mechanisms (university hiring, printing press control, ecclesiastical authority) that selected for sola_fide formulations.',
    'If genuine superiority: the theatrical ratio should remain very low; alternatives were not suppressed but abandoned. If suppression: the theatrical ratio is understated and the constraint is more extractive than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_suppressed, empirical, 'Whether sola_fide dominance reflects doctrinal superiority or institutional suppression.').

omega_variable(
    reading_genealogy_ambiguity,
    'Does this constraint represent the Reformation as it actually unfolded, or does it represent one reading among multiple equally defensible readings whose primacy is underdetermined by the historical evidence?',
    'Historiographic consensus across confessional and secular scholarship. Agreement on primary sources and their interpretation. Whether historians external to confessional commitments converge on the theological_climb narrative or remain divided.',
    'If underdetermined: this reading is one committer-axis option, not a discovered historical fact; the other readings (political_swap, composite_overdetermination) are equally valid alternative orderings of the same events. If determined: this reading captures the actual historical causality and the other readings are distortions. The constraint''s type may depend on which reading is true.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_genealogy_ambiguity, conceptual, 'Whether the theological_climb reading is historically determined or underdetermined by evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1450, reformation_event_boundary__theological_climb_reading, theater_ratio, 1450, 0.0).
narrative_ontology:measurement(refo_tr_t1500, reformation_event_boundary__theological_climb_reading, theater_ratio, 1500, 0.02).
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.05).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__theological_climb_reading, theater_ratio, 1530, 0.08).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.08).
narrative_ontology:measurement(refo_tr_t1600, reformation_event_boundary__theological_climb_reading, theater_ratio, 1600, 0.08).

% Extraction over time
narrative_ontology:measurement(refo_be_t1450, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1450, 0.0).
narrative_ontology:measurement(refo_be_t1500, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1530, 0.25).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.28).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1600, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1450, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1450, 0.0).
narrative_ontology:measurement(refo_su_t1500, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1500, 0.03).
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.08).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1530, 0.13).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.15).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1600, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__theological_climb_reading, 0.12).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The reformation_event_boundary kernel admits three structurally distinct readings instantiated as separate constraint stories: theological_climb_reading (this file, doctrinal innovation as primary driver), political_swap_reading (political realignment as primary driver), and composite_overdetermination_reading (theology, politics, and institutional restructuring as irreducibly co-causal). The ε values differ: theological_climb claims low extraction (genuine innovation) while political_swap claims high extraction (power consolidation). Each story carries omega variables documenting the reading's ambiguity. The network links enable contamination-propagation analysis and corpus-level assessment of whether historical events are underdetermined by evidence (all readings remain plausible) or overdetermined (all readings necessary to capture the phenomenon).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
