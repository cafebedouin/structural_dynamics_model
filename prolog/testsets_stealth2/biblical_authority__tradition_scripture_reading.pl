% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial-Tradition Interpretive Settlement (Tradition-and-Scripture Reading)
 *   domain: theology/religious studies/history of Christianity
 *
 * SUMMARY:
 *   The arrangement under contest is the magisterial-tradition settlement:
 *   Scripture is authoritative for doctrine only as interpreted within
 *   Tradition, and a continuing teaching office — pope and bishops in
 *   communion, deploying the curia and parish clergy — guards and adjudicates
 *   the deposit of faith. Access to authoritative meaning runs through
 *   ordination: only magisterial readings bind, sacramental grace is
 *   administered through clerical mediation, and doctrinal dissent meets
 *   canonical sanction. The settlement solves a real coordination problem — a
 *   fixed ancient text read across millennia, languages, and cultures
 *   fragments without a continuing adjudicator — while concentrating
 *   interpretive authority and material support in a self-perpetuating
 *   hierarchy. KEY AGENTS (by structural relationship):
 *   magisterium_and_ordained_clergy — agenda-setter
 *   (institutional/identity_locked), defines binding interpretation and
 *   collects authority plus material support; curial_administrative_apparatus
 *   — beneficiary-administrator (institutional/constrained); parish_clergy —
 *   dual-positioned local administrators (organized/identity_locked);
 *   unordained_laity — primary bearing party (powerless/constrained);
 *   independent_catholic_theologians — bearing party with locked professional
 *   identity (moderate/identity_locked); vernacular_scripture_translators —
 *   bearing party gated by approval machinery, historically martyred
 *   (moderate/constrained); academic_biblical_scholars_outside_magisterium —
 *   excluded from the adjudicating conversation (organized/mobile);
 *   historians_of_christianity — analytical observer documenting the full
 *   structure including the founding problem's attestation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.63).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.42).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial-Tradition Interpretive Settlement (Tradition-and-Scripture Reading)").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious studies/history of Christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '09b956be-1784-43d7-8633-e29fe33af94c').
narrative_ontology:cs_kernel_codification('09b956be-1784-43d7-8633-e29fe33af94c', fixed_text).
narrative_ontology:cs_authority_grounding('09b956be-1784-43d7-8633-e29fe33af94c', lineage).
narrative_ontology:cs_interpretation_layer_present('09b956be-1784-43d7-8633-e29fe33af94c').
narrative_ontology:cs_reading_relation('09b956be-1784-43d7-8633-e29fe33af94c', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('09b956be-1784-43d7-8633-e29fe33af94c', biblical_authority__conciliar_reading, influences).
narrative_ontology:cs_axiom('09b956be-1784-43d7-8633-e29fe33af94c', foundational, revelation_transmitted_through_teaching_church).
narrative_ontology:cs_axiom_status(revelation_transmitted_through_teaching_church, holdable).
narrative_ontology:cs_axiom_grounding('09b956be-1784-43d7-8633-e29fe33af94c', revelation_transmitted_through_teaching_church, theological).
narrative_ontology:cs_axiom('09b956be-1784-43d7-8633-e29fe33af94c', foundational, magisterium_definitive_interpretive_authority).
narrative_ontology:cs_axiom_status(magisterium_definitive_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('09b956be-1784-43d7-8633-e29fe33af94c', magisterium_definitive_interpretive_authority, theological).
narrative_ontology:cs_reference_frame('09b956be-1784-43d7-8633-e29fe33af94c', apostolic_deposit_guardianship).
narrative_ontology:cs_drift_state('09b956be-1784-43d7-8633-e29fe33af94c', contemporary_post_conciliar_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('09b956be-1784-43d7-8633-e29fe33af94c', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, magisterium_and_ordained_clergy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, curial_administrative_apparatus).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, parish_clergy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, unordained_laity).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, independent_catholic_theologians).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, vernacular_scripture_translators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, parish_clergy).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, deposit_of_faith_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, apostolic_succession_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, sacramental_efficacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines which readings of Scripture bind all the faithful: issues encyclicals, conciliar decrees, and congregational rulings; ordains and deploys clergy; administers the sacramental system through which the faithful receive grace as the tradition understands it. Holds interpretive finality — no reading contrary to its rulings carries authority — and directs the material support that flows through dioceses. Members enter through lifelong formation and ordination; leaving the office forfeits the authority, community, and identity the office constitutes.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, magisterium_and_ordained_clergy, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Staffs the congregations, tribunals, and secretariats that draft, review, and enforce doctrinal and disciplinary rulings. Careers, rank, and residence in Rome depend on the continuation of the adjudicating apparatus; individual officials gain stable livelihood and institutional standing. Departure leads to academia, diplomacy, or retirement outside the governing circle.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, curial_administrative_apparatus, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, curial_administrative_apparatus, agenda_setter).

% Administers sacraments, preaches, and teaches at the local level under obedience to diocesan and Roman superiors. Receives housing, stipend, and community standing from the system administered. Bound by celibacy, assignment mobility, and limits on public doctrinal disagreement; leaving ministry entails laicization, loss of livelihood and community, and for many a collapse of a vocational identity formed since adolescence.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, parish_clergy, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, parish_clergy, payer).

% Receive authoritative teaching, sacraments, and communal life through the clergy. Bear financial support obligations (tithes, offerings, collections) and depend on ordained mediation for access to the means of grace as the tradition defines them. Individual members may attend other churches or none, but exit costs community, family cohesion, and — for the devout — sacramental life itself; most negotiate belief privately rather than exit.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, unordained_laity, payer,
    powerless, biographical, constrained, global).

% Teach and publish on Scripture and doctrine, typically in Catholic institutions or religious orders. A mandatum — recognition by the local bishop — is required to teach theology credentially; dissent from definitive rulings risks removal, loss of the mandatum, or silencing, as in documented cases of censured theologians. Vocation, training, and professional community are constituted inside the system studied; exit to secular academia severs the audience and questions that give the work its point.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, independent_catholic_theologians, payer,
    moderate, biographical, identity_locked, global).

% Translate Scripture and liturgical texts into vernacular languages. Approval machinery (nihil obstat, imprimatur, liturgical norms) gates publication and liturgical use; unauthorized translations historically met severe sanction — William Tyndale was executed for his English New Testament. Contemporary translators work within review structures or publish academically outside liturgical use.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, vernacular_scripture_translators, payer,
    moderate, biographical, constrained, global).

% Produce critical editions, philological and historical readings of the biblical texts in universities worldwide. Their readings carry no binding weight in the adjudicated system regardless of scholarly consensus; they are not seated in the adjudicating conversation, and their conclusions enter it only when the magisterium chooses to receive them.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, academic_biblical_scholars_outside_magisterium, excluded,
    organized, biographical, mobile, global).

% Study the arrangement's formation and operation across centuries from outside confessional commitment: document the second-century interpretive conflicts that produced the rule of faith and episcopal succession, trace the enforcement record, and compare fragmentation outcomes across traditions. Hold no stake in the arrangement's persistence or dissolution.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, historians_of_christianity, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, magisterium_and_ordained_clergy).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves interpretive disputes over a fixed ancient text through a single continuing adjudicating body; maintains doctrinal continuity across generations, languages, and cultures; standardizes sacramental practice; and supplies a common rule of faith that distinguishes received teaching from novel interpretation.
% TRANSFER_FUNCTION: Moves interpretive authority upward from all baptized readers to the ordained hierarchy (only magisterial readings bind); moves material support (tithes, offerings, Mass stipends) from the unordained to clergy and the administrative apparatus; moves doctrinal rulings downward from the center to parishes.
% ABSENT_VOICES: Lay readers claiming direct interpretive access, non-magisterial biblical scholars (including Catholic theologians without a mandatum), and historically the vernacular translators and reformers. They are absent because the adjudicating body defines which voices count as authorized — the excluded parties' exclusion is maintained by the arrangement itself.
% DISAPPEARANCE_RATIONALE: If the magisterial-tradition settlement vanished overnight, Christianity would reorganize around competing interpretive authorities — the pattern visible wherever its jurisdiction ended, as Reformation-era Europe produced state churches and denominational proliferation. Sacramental administration, canon law, seminary formation, and curial governance all presuppose it; lay religious practice would fragment among scripture-alone, conciliar, and restorationist forms.
% FOUNDING_PROBLEM: Early Christianity faced proliferating interpretations of apostolic writings — gnostic readings, adoptionist and docetic christologies — before a fixed canon existed (late second century). The rule of faith and episcopal succession were constructed to distinguish authentic apostolic teaching from novelty; the present arrangement descends from that solution.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of early Christianity (the Bauer school and its successors in contemporary patristics) attest from outside the benefiting parties that the interpretive-conflict problem was real and predated the arrangement's consolidation. The problem's persistence is visible today in denominational fragmentation wherever no single adjudicator operates; even historians within traditions that reject this arrangement concede the second-century crisis of authority. Dispute concerns whether this settlement is the necessary or best solution — not whether the founding problem existed.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.63: interpretive finality is monopolized in the hierarchy and material flows (tithes, offerings, stipends) run steadily to clergy and administration, but the arrangement simultaneously delivers large-scale goods — education, charity, sacramental life, doctrinal stability — so the transfer is not pure rent. Suppression 0.42 reflects the current enforcement picture: spiritual sanctions (interdict, excommunication, denial of communion), the mandatum system, and removal of dissenting teachers, with the coercive machinery far reduced from its mid-seventeenth-century peak (Index of Prohibited Books, Roman Inquisition). Suppression here mixes structural mechanisms (canonical penalties, credential gating, exclusion from teaching posts) with internalized ones (formation in deference to magisterial authority, the framing of lay response as 'reception' rather than judgment); the scalar cannot separate these, and the mixture is noted rather than resolved. Theater_ratio 0.32 requires care: in a sacramental tradition, ceremony IS function — liturgy is not performance standing in for a missing function — so baseline theater is genuinely low. The rising component is different: magisterial output increasingly consists of reiterated teaching whose uptake decays (encyclicals and synodal documents acknowledged but not operative in lay practice), which is performative maintenance of an authority relation whose practical grip is thinning. Accessibility_collapse 0.60: accepting the reading's axiom collapses private-interpretation alternatives within the framework almost completely, yet exit to rival frameworks remained historically available — the Reformation demonstrates that the collapse is partial, framework-relative, not absolute. Resistance 0.65: sustained across the whole interval — conciliarism, the Reformation, Enlightenment critique, the modernist crisis, post-conciliar dissent, and the traditionalist counter-mobilization. Identity-lock dynamics: the clergy seat is locked by ontological-character theology and celibate formation (exit collapses vocational identity); the theologian seat by mandatum-dependence and career-path dependence (exit severs the audience constituting the work). If either lock broke at scale — mass laicization or mass mandatum refusal — enforcement capacity would fall faster than the scalar series suggests. The measurement series run on one shared time grid (eight points, all three metrics authored at every point) tracing a coherent arc: enforcement built up from Nicaea's anathemas through the medieval-inquisitorial machinery to the seventeenth-century peak, then decayed after the loss of temporal power (1870) and collapsed into spiritual-only sanction after Vatican II (1965), with slight re-hardening since.
 *
 * PERSPECTIVAL GAP:
 *   The bearing seats and the agenda-setting seat should compute differently. From the magisterium's position the settlement is stewardship of a trust received from the apostles — the mediation is the gift, not the toll. From the unordained laity's position it is gatekeeping of access to meaning and grace, with the gatekeepers' livelihood funded by the gated. Parish clergy occupy both sides at once: administrators collecting a livelihood, themselves bound by obedience they did not set. Independent theologians experience the sharpest divergence — trained to prize critical inquiry inside a system that conditions their credentials on deference. The engine computes these per-seat divergences from the structural data (power, exit, role); the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium sits near the beneficiary end: it collects interpretive finality and material support, controls the rules, and its exit is locked in a way that makes defending the arrangement existential rather than optional. The curia and parish clergy derive low-to-moderate directional values as collectors, with parish clergy pulled back toward the middle by their secondary bearing position (obedience, celibacy, constrained speech). Unordained laity sit near the target end — they bear the transfer and the interpretive dependency, with constrained (not trapped) exit keeping them short of full-target amplification. Independent theologians sit near-full-target, amplified by identity_locked exit: the derivation reads their victim declaration plus locked exit correctly, so no override is needed. Vernacular translators are historical-and-current targets of the approval machinery. Excluded scholars sit outside the extraction surface proper — their exclusion is the enforcement object, not their payment — and the observer seat takes the analytical value. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two mislabelings. Reading the settlement as pure predation misses the documented coordination function: the founding problem (interpretive fragmentation of a fixed text) is real, externally corroborated by secular historiography, and visibly recurs wherever the adjudicating function is absent — denominational proliferation is the control condition. Reading it as benign coordination misses the asymmetric structure: a standing hierarchy collects continuously, the bearing seats are diffuse and individually weak, and the enforcement record (executed translators, censured theologians, indexed books) shows the asymmetry was held by force as much as by consent. Tangled_rope holds both facts. The mandate is not atrophied: adjudication and sacramental administration remain operative (theater_ratio 0.32, below inertial thresholds), and the R5 interview returns founding-problem status live with disappearance verdict world_rearranges — the mismatch consumer finds no dead-mandate-plus-world-rearranges flag. The open question is trajectory: if enforcement continues decaying while formal teaching output grows, theater_ratio rises and the arrangement drifts toward inertial maintenance; the enforcement_decay_trajectory omega tracks exactly that fork.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the biblical_authority kernel — how would adopting a sibling reading (sola_scriptura or conciliar) change the constraint''s beneficiary/victim structure?',
    'Generate the sibling stories and compare computed classifications: the sola_scriptura reading removes the standing mediating hierarchy entirely (no continuous adjudicating seat to collect); the conciliar reading replaces continuous magisterial adjudication with episodic council-and-consensus adjudication.',
    'Victim sets and epsilon are reading-indexed, not topic-indexed: under the sola_scriptura reading, lay interpretive agency stops being a bearing party and becomes the operative authority; under the conciliar reading, adjudication concentrates intermittently in councils rather than continuously in the magisterium. Cross-reading comparisons that pool these as one constraint are invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: reading-indexed victim sets across the biblical_authority kernel.').

omega_variable(
    development_vs_rupture,
    'Is post-conciliar practice change (religious liberty, ecumenism, liturgical reform, collegiality) legitimate development of the deposit of faith, or departure from it?',
    'Apply the tradition''s own internal criteria (the Vincentian canon: what has been believed everywhere, always, by all) to specific changed teachings and test whether the continuity narrative survives scrutiny from within the reading''s own framework.',
    'If rupture, the lineage-based authority claim erodes and traditionalist repudiation pressure grows (authority_erosion drift accelerates); if development, the interpretive layer successfully absorbs drift and the reference frame holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_vs_rupture, conceptual, 'Whether the development-of-doctrine framing absorbs or conceals reference-frame drift.').

omega_variable(
    mediation_necessity_vs_gatekeeping,
    'Within the reading''s own premises, is clerical mediation structurally necessary for transmitting doctrine and grace, or is the mediating gate separable from the interpretive authority it concentrates?',
    'Comparative ecclesiology: examine communities practicing unmediated interpretive access (Quaker meetings, congregational discernment models) for doctrinal-stability outcomes; internally, test whether magisterial rulings track the received deposit or introduce novel positions.',
    'If mediation is separable from the authority concentration, part of the measured burden on unordained seats is contingent gatekeeping riding on a genuine function; if inseparable, the concentration is the price of the coordination itself and the hybrid-coordination reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mediation_necessity_vs_gatekeeping, conceptual, 'Separability of the anti-fragmentation coordination function from the interpretive monopoly.').

omega_variable(
    enforcement_decay_trajectory,
    'Will spiritual-sanction enforcement continue decaying toward ceremonial maintenance, or re-harden under internal polarization?',
    'Track canonical-penalty application rates, communion-denial incidents, doctrinal-congregation interventions against theologians, and diocesan implementation of doctrinal directives across coming decades.',
    'Continued decay pushes the arrangement toward inertial maintenance (rising theater_ratio, falling suppression); re-hardening restores active enforcement and raises the burden on dissenting seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_trajectory, empirical, 'Trajectory of enforcement capacity after the post-conciliar liberalization.').

omega_variable(
    lay_coalition_potential,
    'Could coordinated lay action (withheld donations and attendance, organized reform movements) convert the unordained seat from individually weak to collectively consequential?',
    'Observe post-scandal donation and attendance elasticity, the formation and durability of organized lay movements (reform networks and traditionalist counter-mobilization alike), and hierarchical concessions following collective pressure.',
    'A viable lay coalition raises resistance beyond the authored 0.65 and forces negotiated rearrangement; absent coalition capacity, the bearing seat stays diffuse and the arrangement persists on current terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_coalition_potential, empirical, 'Coalition capacity of the diffuse unordained seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_authority__tradition_scripture_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement_basis(bibl_tr_t325, observed).
narrative_ontology:measurement(bibl_tr_t700, biblical_authority__tradition_scripture_reading, theater_ratio, 700, 0.12).
narrative_ontology:measurement_basis(bibl_tr_t700, observed).
narrative_ontology:measurement(bibl_tr_t1215, biblical_authority__tradition_scripture_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t1215, observed).
narrative_ontology:measurement(bibl_tr_t1545, biblical_authority__tradition_scripture_reading, theater_ratio, 1545, 0.2).
narrative_ontology:measurement_basis(bibl_tr_t1545, observed).
narrative_ontology:measurement(bibl_tr_t1650, biblical_authority__tradition_scripture_reading, theater_ratio, 1650, 0.26).
narrative_ontology:measurement_basis(bibl_tr_t1650, observed).
narrative_ontology:measurement(bibl_tr_t1870, biblical_authority__tradition_scripture_reading, theater_ratio, 1870, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t1870, observed).
narrative_ontology:measurement(bibl_tr_t1965, biblical_authority__tradition_scripture_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t1965, observed).
narrative_ontology:measurement(bibl_tr_t2025, biblical_authority__tradition_scripture_reading, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(bibl_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_authority__tradition_scripture_reading, base_extractiveness, 325, 0.35).
narrative_ontology:measurement_basis(bibl_be_t325, observed).
narrative_ontology:measurement(bibl_be_t700, biblical_authority__tradition_scripture_reading, base_extractiveness, 700, 0.45).
narrative_ontology:measurement_basis(bibl_be_t700, observed).
narrative_ontology:measurement(bibl_be_t1215, biblical_authority__tradition_scripture_reading, base_extractiveness, 1215, 0.6).
narrative_ontology:measurement_basis(bibl_be_t1215, observed).
narrative_ontology:measurement(bibl_be_t1545, biblical_authority__tradition_scripture_reading, base_extractiveness, 1545, 0.68).
narrative_ontology:measurement_basis(bibl_be_t1545, observed).
narrative_ontology:measurement(bibl_be_t1650, biblical_authority__tradition_scripture_reading, base_extractiveness, 1650, 0.7).
narrative_ontology:measurement_basis(bibl_be_t1650, observed).
narrative_ontology:measurement(bibl_be_t1870, biblical_authority__tradition_scripture_reading, base_extractiveness, 1870, 0.72).
narrative_ontology:measurement_basis(bibl_be_t1870, observed).
narrative_ontology:measurement(bibl_be_t1965, biblical_authority__tradition_scripture_reading, base_extractiveness, 1965, 0.57).
narrative_ontology:measurement_basis(bibl_be_t1965, observed).
narrative_ontology:measurement(bibl_be_t2025, biblical_authority__tradition_scripture_reading, base_extractiveness, 2025, 0.63).
narrative_ontology:measurement_basis(bibl_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_authority__tradition_scripture_reading, suppression_requirement, 325, 0.25).
narrative_ontology:measurement_basis(bibl_su_t325, observed).
narrative_ontology:measurement(bibl_su_t700, biblical_authority__tradition_scripture_reading, suppression_requirement, 700, 0.4).
narrative_ontology:measurement_basis(bibl_su_t700, observed).
narrative_ontology:measurement(bibl_su_t1215, biblical_authority__tradition_scripture_reading, suppression_requirement, 1215, 0.55).
narrative_ontology:measurement_basis(bibl_su_t1215, observed).
narrative_ontology:measurement(bibl_su_t1545, biblical_authority__tradition_scripture_reading, suppression_requirement, 1545, 0.7).
narrative_ontology:measurement_basis(bibl_su_t1545, observed).
narrative_ontology:measurement(bibl_su_t1650, biblical_authority__tradition_scripture_reading, suppression_requirement, 1650, 0.78).
narrative_ontology:measurement_basis(bibl_su_t1650, observed).
narrative_ontology:measurement(bibl_su_t1870, biblical_authority__tradition_scripture_reading, suppression_requirement, 1870, 0.65).
narrative_ontology:measurement_basis(bibl_su_t1870, observed).
narrative_ontology:measurement(bibl_su_t1965, biblical_authority__tradition_scripture_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement_basis(bibl_su_t1965, observed).
narrative_ontology:measurement(bibl_su_t2025, biblical_authority__tradition_scripture_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(bibl_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'biblical authority' decomposes into three structurally distinct constraints (epsilon-invariance principle): this tradition_scripture_reading (continuous magisterial adjudication; standing hierarchy as beneficiary; unordained interpretive agency as bearing party), the sola_scriptura_reading (no mediating hierarchy; entirely different beneficiary/victim structure), and the conciliar_reading (episodic council-and-patristic-consensus adjudication; extraction intermittent rather than continuous). This reading is upstream of the siblings historically: the Reformation reading arose as a reaction against it, and Vatican I's definition of papal authority changed the legitimacy conditions under which conciliar claims operate inside Catholic space. Each story carries its own epsilon, stakeholders, and classification; they are linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
