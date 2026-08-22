% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura — Scripture as Sole Self-Interpreting Authority
 *   domain: theological/religious-institutional
 *
 * SUMMARY:
 *   A doctrinal-authority arrangement originating in the sixteenth-century
 *   Reformation: the canonical Christian scriptures, held to be divinely
 *   inspired, sufficient for doctrine and practice, and clear enough that
 *   sincere readers need no binding human interpreter above the text. As
 *   instantiated across the Protestant world, the arrangement dismantled a
 *   clerical mediation monopoly — vernacular scripture and liturgy,
 *   priesthood of all believers, congregational self-governance — while
 *   generating its own characteristic costs: interpretive fragmentation into
 *   thousands of denominations, discipline of minority readers, and
 *   re-concentration of interpretive influence in pastors, confessions,
 *   seminaries, and a large publishing economy. Per the epsilon-invariance
 *   principle this file instantiates ONE reading of the kernel
 *   biblical_authority — the sola_scriptura_reading — with a single stable
 *   epsilon assessed over the standing arrangement as it actually operates
 *   (text plus its interpretive infrastructure), never over the reading's
 *   endorsed ideal of unmediated access. The sibling readings
 *   (biblical_authority__tradition_scripture_reading,
 *   biblical_authority__conciliar_reading) are separate constraint files with
 *   their own epsilon, beneficiary structures, and classifications; the
 *   contest between readings is carried in the omega variables, not averaged
 *   into this one. KEY AGENTS (by structural relationship): - lay_believers:
 *   Intended beneficiary (moderate/identity_locked) — receive direct textual
 *   access; carry tithes, unpaid interpretive labor, and fragmentation risk -
 *   ordained_pastorate: Agenda-setter and residual collector
 *   (organized/identity_locked) — administers interpretation weekly; salaried
 *   from the arrangement it serves - congregational_polities: Beneficiary
 *   (organized/mobile) — self-governance without hierarchical tribute -
 *   confessional_denominations: Enforcement agenda-setter
 *   (institutional/arbitrage) — confessional subscription, credentialing,
 *   church planting - minority_interpretation_communities: Primary payer
 *   (powerless/constrained) — bear discipline and exclusion for divergent
 *   readings of the same text - unaffiliated_religious_seekers: Payer
 *   (powerless/mobile) — bear search costs in an unadjudicated interpretive
 *   marketplace - bible_publishing_industry: Commercial beneficiary
 *   (powerful/arbitrage) — monetizes the universal-access premise -
 *   ecumenical_dialogue_bodies: Excluded voice (institutional/mobile) — seeks
 *   cross-community coherence the arrangement provides no mechanism to
 *   produce - academic_biblical_scholars: Analytical observer
 *   (analytical/analytical) — sees the full transmission and interpretation
 *   structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.46).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.42).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura — Scripture as Sole Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theological/religious-institutional").

domain_priors:requires_active_enforcement(biblical_authority__sola_scriptura_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '41d0cc39-58f8-401b-be02-588775e20c79').
narrative_ontology:cs_kernel_codification('41d0cc39-58f8-401b-be02-588775e20c79', fixed_text).
narrative_ontology:cs_authority_grounding('41d0cc39-58f8-401b-be02-588775e20c79', distributed).
narrative_ontology:cs_reading_relation('41d0cc39-58f8-401b-be02-588775e20c79', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('41d0cc39-58f8-401b-be02-588775e20c79', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('41d0cc39-58f8-401b-be02-588775e20c79', foundational, scripture_alone_sufficient_for_doctrine_and_practice).
narrative_ontology:cs_axiom_status(scripture_alone_sufficient_for_doctrine_and_practice, holdable).
narrative_ontology:cs_axiom_grounding('41d0cc39-58f8-401b-be02-588775e20c79', scripture_alone_sufficient_for_doctrine_and_practice, theological).
narrative_ontology:cs_axiom('41d0cc39-58f8-401b-be02-588775e20c79', foundational, scripture_perspicuous_to_sincere_reader).
narrative_ontology:cs_axiom_status(scripture_perspicuous_to_sincere_reader, holdable).
narrative_ontology:cs_axiom_grounding('41d0cc39-58f8-401b-be02-588775e20c79', scripture_perspicuous_to_sincere_reader, empirically_contingent).
narrative_ontology:cs_axiom('41d0cc39-58f8-401b-be02-588775e20c79', secondary, no_binding_human_interpreter_above_text).
narrative_ontology:cs_axiom_status(no_binding_human_interpreter_above_text, holdable).
narrative_ontology:cs_axiom_grounding('41d0cc39-58f8-401b-be02-588775e20c79', no_binding_human_interpreter_above_text, theological).
narrative_ontology:cs_reference_frame('41d0cc39-58f8-401b-be02-588775e20c79', self_authenticating_clear_canonical_text).
narrative_ontology:cs_drift_state('41d0cc39-58f8-401b-be02-588775e20c79', contemporary_mass_literacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('41d0cc39-58f8-401b-be02-588775e20c79', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, congregational_polities).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, bible_publishing_industry).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, minority_interpretation_communities).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, unaffiliated_religious_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, ordained_pastorate).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, scripture_perspicuity_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Read and interpret scripture directly in vernacular translations, individually and in congregational settings, without requiring a priestly intermediary for access to doctrine. Contribute tithes that fund pastors, buildings, seminaries, and media ministries; supply enormous unpaid interpretive labor; and choose among thousands of denominations, each claiming the same text. Leaving a community typically costs family ties, friendships, and an entire metaphysical framework, so most remain within the tradition that formed them even when disputing particular teachers.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, lay_believers, payer).

% Trained in seminaries, ordained or commissioned by congregations or denominations, preach and teach weekly, set the local teaching agenda, administer membership and discipline, and officiate rites. Supported financially by tithes and offerings. Their standing rests on being trusted readers of the text they serve, and they are accountable to congregations and confessions that claim the same authority. Leaving the vocation mid-career usually means forfeiting livelihood, professional identity, and community standing at once.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ordained_pastorate, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, ordained_pastorate, beneficiary).

% Self-governing local churches that select their own pastors, set doctrine statements and budgets, and hold property without remitting tribute to a hierarchical center. They may join, leave, or switch denominational affiliations comparatively freely, which is the tradition's hallmark. They bear the practical burdens the removed hierarchy used to carry: finding and vetting trustworthy teachers, policing doctrine internally, and absorbing splits.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, congregational_polities, beneficiary,
    organized, generational, mobile, regional).

% Trans-local bodies — Presbyterian, Lutheran, Baptist, Reformed, nondenominational networks — that maintain confessional standards such as the Westminster Confession or the Augsburg Confession, require ministerial subscription, credential pastors, plant churches, and publish curricula. They compete with one another for members and market share, and can merge, divide, or rebrand in response to doctrinal disputes; the fragmentation of the field is also their field of operation.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, confessional_denominations, agenda_setter,
    institutional, generational, arbitrage, continental).

% Groups whose readings diverge from majority confessional standards — non-Trinitarian readers, radical restorationists, strict separatists, and others — who appeal to the same text and the same method as their neighbors and reach condemned conclusions. They face discipline, exclusion, and heresy labels from larger bodies, and their members face social costs. They can exit to other denominations but seldom find acceptance anywhere within the tradition, since every body reserves the right to declare its own reading the biblical one.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, minority_interpretation_communities, payer,
    powerless, biographical, constrained, national).

% People approaching the text without membership in any interpretive community — converts, deconstructing believers, the simply curious. They confront a marketplace of mutually contradictory teachings, each presented as plain biblical truth, with no institution responsible for the reliability of what they encounter. They bear the full search cost of distinguishing careful teaching from charismatic improvisation, and they are the population most exposed to high-demand teachers operating under the same banner.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, unaffiliated_religious_seekers, payer,
    powerless, immediate, mobile, global).

% Commercial publishers, software makers, and media companies producing translations, study Bibles, commentaries, curricula, and apps for a market premised on every believer's need for direct access. Revenue scales with interpretive demand; new translations and study formats continually refresh the product line. They bear no responsibility for doctrinal outcomes and face no adjudicative review of their interpretive aids.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, bible_publishing_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Councils of churches and bilateral dialogue commissions seeking recoverable doctrinal agreement across the divided heirs of the Reformation. The arrangement gives them no seat: each party arrives citing the same self-interpreting text and leaves with different conclusions, and no proposal binds anyone. Decades of agreed statements have documented convergence without changing any community's doctrine.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_dialogue_bodies, excluded,
    institutional, generational, mobile, global).

% Textual critics, historians, and philologists studying the transmission, translation history, and interpretation of the canon. They document the manuscript complexity, translation choices, and reception histories that stand between the original texts and any modern reader, and they observe the distance between the 'plain reading' ideal and the interpretive infrastructure every community actually relies on. They hold no disciplinary seat in any confessing body.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, academic_biblical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__sola_scriptura_reading, ordained_pastorate).
narrative_ontology:fixing_cost_class(biblical_authority__sola_scriptura_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common written normative referent that lets geographically dispersed, hierarchically unconnected congregations claim shared doctrinal ground without a central adjudicator; gives ordinary believers direct access to the founding texts in vernacular translation; supports congregational self-governance by locating legitimacy in the text rather than in a distant office.
% TRANSFER_FUNCTION: Moves interpretive labor from a professional clerical class to individual believers and local congregations; moves money (tithes, book sales, tuition) from lay believers to pastors, seminaries, denominational bureaucracies, and publishers; moves doctrinal decision rights from councils and tradition offices to whoever can plausibly claim the text's meaning — in practice, trained teachers and popular communicators.
% ABSENT_VOICES: Ecumenical dialogue bodies and minority interpretation communities: anyone seeking a binding answer to cross-community disagreement finds no seat, because the arrangement recognizes no adjudicator above text-and-reader. Historic episcopal and conciliar churches object from outside the framework. Patristic voices are cited selectively as witnesses rather than seated as judges.
% DISAPPEARANCE_RATIONALE: Several hundred million believers' devotional and congregational life is organized around direct textual authority; congregational governance, seminary systems, denominational structures, and a large publishing economy all presuppose it. Overnight removal would force every pulpit and pew to renegotiate where doctrine comes from — the landscape of Protestant institutions would rearrange within a generation.
% FOUNDING_PROBLEM: Late-medieval Western Christendom routed salvation-knowledge through a clerical monopoly: an untranslated liturgy, doctrine governed by papal and conciliar decree layered atop accumulated tradition, and devotional practices — indulgences foremost — that reformers read as selling what the text offered freely. The arrangement was built to place an unmediated, translatable authority above pope and council alike.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Catholic, Orthodox, and secular historians of Christianity broadly corroborate the sixteenth-century crisis — indulgence finance, simony, clerical illiteracy — and the Council of Trent's own reform decrees concede much of the indictment. What no outside party corroborates is that the founding problem remains live in its original form: adherents attest its persistence, while ecumenical and Catholic interlocutors locate the live problem elsewhere, in the interpretive fragmentation the solution itself produced.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim and the metrics are authored independently. Claimed type is tangled_rope: the arrangement possesses a genuine coordination function (a common written referent letting hierarchically unconnected congregations claim shared doctrinal ground; direct lay access; congregational autonomy) AND asymmetric costs running through the same structure (fragmentation borne by coherence-seekers and minority readers; discipline machinery; re-concentrated gatekeeping), held in place by active enforcement (confessional subscription, credentialing, membership discipline) — hence requires_active_enforcement. Metrics describe actual operation. Extractiveness 0.46 is low relative to magisterial arrangements — the founding extraction economy (indulgences, salvific mediation rents) was abolished, which is the reading's real achievement — but materially above zero because tithes, tuition, and publishing revenue flow to a re-constituted interpretive class and because fragmentation imposes real search and security costs on the least protected parties. Suppression 0.42 is mostly social and internalized rather than juridical: the enforcement series peaks in the confessional age (T~200, catechisms, subscription oaths, discipline courts) and decays under disestablishment and religious liberty, while the scalar stays elevated because fear-of-error and community-loss persist independently of enforcement machinery (see omega suppression_mechanism_ambiguity). Theater 0.38 and rising: the founding generations were martyred for the principle; contemporary adherence increasingly performs an inherited identity ('we just read the Bible') while relying on an interpretive infrastructure the slogan denies. Accessibility_collapse 0.52: rival readings persist and conversion out occurs, but within committed communities the magisterial alternative is foreclosed by the axiom itself. Resistance 0.55: five centuries of Catholic and Orthodox counter-argument, ecumenical pressure, and perpetual internal splintering. All three series share one time grid (T=0..500, mapping 1517 to the present); no metric is sampled on a private grid.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. Congregational polities and the publishing industry (mobile/arbitrage beneficiaries) experience a workable, even flourishing coordination order. Lay believers (identity_locked, moderate) sit nearer symmetric: subsidized access shadowed by transfer costs they cannot itemize. Minority interpretation communities and unaffiliated seekers (powerless payers) experience enforced interpretive insecurity — the same 'everyone can read' promise that liberates the majority exposes them with no recourse, because the arrangement validates every sincere reading while disciplining the conclusions it dislikes. The pastorate experiences stewardship from inside and gatekeeping from outside; its dual position (agenda_setter/beneficiary) is the hinge on which the arrangement's costs turn. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Beneficiaries (lay_believers, congregational_polities, bible_publishing_industry) derive low d; victims (minority_interpretation_communities, unaffiliated_religious_seekers) derive high d, amplified by powerless power atoms and constrained exits. One override is authored: moderate -> 0.35. The derivation would read lay_believers — a declared beneficiary — as near-subsidized, but they simultaneously fund the pastorate through tithes, perform the unpaid interpretive labor the arrangement runs on, and carry fragmentation risk personally; their net position is beneficiary-leaning-symmetric, not subsidized. The override targets the moderate atom, which in this story only lay_believers occupy, so no other seat is disturbed. Vindicated propositions (perspicuity doctrine, priesthood of all believers) are recorded as propositions, not beneficiaries: they collect no rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a clerical monopoly over salvation-knowledge, indulgence finance, an unreadable liturgy — was substantially addressed within a century of the arrangement's adoption; even the Council of Trent's own reform decrees concede most of the indictment. The arrangement nonetheless persists and expanded, sustained by functions the founding problem never named: Protestant identity maintenance, congregational governance, and a publishing economy built on the access premise. The R5 mismatch consumer will register status=contested x verdict=world_rearranges, and correctly so — but the persistence is not mere inertia: theater_ratio 0.38 sits below piton range, enforcement still operates wherever doctrine is contested, and the coordination function remains load-bearing for hundreds of millions. The live drift hypotheses run in opposite directions: continued enforcement decay plus rising identity-performance would push theater past 0.5 and strand the arrangement as maintained habit (piton trajectory), while high-discipline sects within the field show the opposite pull, toward snare-side dynamics. The tangled_rope classification prevents both mislabels: reading the arrangement as pure extraction erases the real liberation of lay access that motivated it; reading it as pure coordination erases the discipline machinery and the identifiable parties who pay for everyone else's interpretive freedom.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the kernel biblical_authority; how would the classification change under the sibling readings?',
    'Instantiate the sibling files (biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading) and compare computed per-seat classifications across the kernel family.',
    'The tradition reading restores an adjudicative monopoly (higher clerical extraction, lower fragmentation cost); the conciliar reading distributes adjudication across councils (intermediate on both axes). This file''s tangled_rope verdict is contingent on the sola scriptura framing and should not be generalized to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer-frame contingency: classification is indexed to one reading of a contested kernel.').

omega_variable(
    perspicuity_empirical_status,
    'Is the perspicuity axiom — that sincere readers can reach essential doctrine from the text alone — empirically supported, or does cross-community fragmentation constitute accumulating counter-evidence?',
    'Comparative study of interpretive convergence and divergence among communities sharing the axiom versus communities with adjudicative layers (conciliar, magisterial); hermeneutical research on reader-variance under controlled conditions.',
    'Sustained divergence among equally sincere readers undermines the empirically_contingent foundational axiom, driving axiom_overriding drift toward tradition- or council-mediated interpretation; demonstrated convergence would stabilize the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perspicuity_empirical_status, empirical, 'Empirical status of the self-interpreting-text claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (membership discipline, confessional gatekeeping, social sanction) or internalized (fear of error and divine displeasure that persists independent of enforcement)?',
    'Post-exit trajectory studies of leavers from high-enforcement and low-enforcement sola-scriptura communities: if fear-of-error and community-loss responses persist after the enforcing community is gone, the internalized share is substantial.',
    'If largely internalized, effective suppression stays high even as enforcement machinery decays — reconciling the falling suppression_requirement series with the stable scalar — and exit-based remedies underperform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized composition of suppression.').

omega_variable(
    gatekeeper_reconcentration,
    'Does the arrangement actually eliminate interpretive mediation, or re-brand it in pastors, celebrity teachers, seminaries, and publishers who collect quasi-clerical returns while denying clerical status?',
    'Income and authority-flow analysis comparing concentration of interpretive influence and material returns in sola-scriptura systems versus magisterial and conciliar systems.',
    'High reconcentration would raise effective extraction toward the sibling readings'' levels and erode this reading''s distinctive low-extraction claim; low reconcentration would confirm the structural delta.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_reconcentration, empirical, 'Whether anti-clerical form conceals re-constituted clerical function.').

omega_variable(
    fragmentation_cost_or_feature,
    'Is doctrinal fragmentation a cost borne by real parties (coherence-seekers, minority readers) or a competitive process that improves interpretive accuracy over time?',
    'Track whether five centuries of fragmentation show convergence on better-supported readings (marketplace model) or stable multiplication of incompatible communities with no adjudication mechanism (fragmentation model).',
    'If feature-not-bug, the victim declarations weaken and the classification shifts toward rope; if cost, tangled_rope holds and high-discipline sects within the field become candidates for snare-side dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fragmentation_cost_or_feature, conceptual, 'Whether the fragmentation externality is a defect or a discovery procedure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__sola_scriptura_reading, theater_ratio, 100, 0.16).
narrative_ontology:measurement_basis(bibl_tr_t100, observed).
narrative_ontology:measurement(bibl_tr_t200, biblical_authority__sola_scriptura_reading, theater_ratio, 200, 0.23).
narrative_ontology:measurement_basis(bibl_tr_t200, observed).
narrative_ontology:measurement(bibl_tr_t300, biblical_authority__sola_scriptura_reading, theater_ratio, 300, 0.29).
narrative_ontology:measurement_basis(bibl_tr_t300, observed).
narrative_ontology:measurement(bibl_tr_t400, biblical_authority__sola_scriptura_reading, theater_ratio, 400, 0.34).
narrative_ontology:measurement_basis(bibl_tr_t400, observed).
narrative_ontology:measurement(bibl_tr_t500, biblical_authority__sola_scriptura_reading, theater_ratio, 500, 0.38).
narrative_ontology:measurement_basis(bibl_tr_t500, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__sola_scriptura_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement_basis(bibl_be_t100, observed).
narrative_ontology:measurement(bibl_be_t200, biblical_authority__sola_scriptura_reading, base_extractiveness, 200, 0.37).
narrative_ontology:measurement_basis(bibl_be_t200, observed).
narrative_ontology:measurement(bibl_be_t300, biblical_authority__sola_scriptura_reading, base_extractiveness, 300, 0.41).
narrative_ontology:measurement_basis(bibl_be_t300, observed).
narrative_ontology:measurement(bibl_be_t400, biblical_authority__sola_scriptura_reading, base_extractiveness, 400, 0.44).
narrative_ontology:measurement_basis(bibl_be_t400, observed).
narrative_ontology:measurement(bibl_be_t500, biblical_authority__sola_scriptura_reading, base_extractiveness, 500, 0.46).
narrative_ontology:measurement_basis(bibl_be_t500, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__sola_scriptura_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__sola_scriptura_reading, suppression_requirement, 100, 0.34).
narrative_ontology:measurement_basis(bibl_su_t100, observed).
narrative_ontology:measurement(bibl_su_t200, biblical_authority__sola_scriptura_reading, suppression_requirement, 200, 0.44).
narrative_ontology:measurement_basis(bibl_su_t200, observed).
narrative_ontology:measurement(bibl_su_t300, biblical_authority__sola_scriptura_reading, suppression_requirement, 300, 0.37).
narrative_ontology:measurement_basis(bibl_su_t300, observed).
narrative_ontology:measurement(bibl_su_t400, biblical_authority__sola_scriptura_reading, suppression_requirement, 400, 0.3).
narrative_ontology:measurement_basis(bibl_su_t400, observed).
narrative_ontology:measurement(bibl_su_t500, biblical_authority__sola_scriptura_reading, suppression_requirement, 500, 0.24).
narrative_ontology:measurement_basis(bibl_su_t500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, information_standard).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the kernel biblical_authority per the epsilon-invariance principle. The colloquial label 'biblical authority' conflates three structurally distinct arrangements: this file instantiates the sola scriptura reading (epsilon ~0.46: low clerical extraction, fragmentation costs, re-concentrated gatekeeping); the tradition_scripture_reading file authors a higher epsilon over magisterial mediation with coherent doctrine; the conciliar_reading file authors an intermediate profile. Coupling runs both ways: the tradition reading cites patristic continuity against this reading's perspicuity claim, while this reading's vernacular-literacy and translation legacy reshapes the operating environment of both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__sola_scriptura_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
