% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Authority: Strict Orthodox Reading
 *   domain: systematic_theology/ecclesiology/doctrinal_history
 *
 * SUMMARY:
 *   The Nicene Creed (325 CE) formally established a single authoritative
 *   metaphysical framework for understanding Christ's nature and divine
 *   composition. In this STRICT ORTHODOX READING, the creed functions as a
 *   binding doctrinal requirement: deviation from its metaphysical claims
 *   constitutes heresy, warranting sanction, excommunication, and
 *   suppression. The constraint's persistence across 1545 years depends on
 *   active enforcement by hierarchical clergy and secular rulers. This
 *   reading instantiates one pole of the contested kernel — a rigid,
 *   magisterial interpretation treating the creed as binding metaphysical
 *   truth. Sibling readings (liturgical habituation and symbolic
 *   confessional) offer different framings of the same kernel, with
 *   substantially different beneficiary/victim structures and extracted
 *   values.
 *
 * KEY AGENTS:
 *   - Hierarchical Clergy (bishops, councils, magistrates): agenda-setters and beneficiaries — collect interpretive authority, enforce boundaries, control sacramental access
 *   - Heterodox Communities (Arian, Nestorian, Monophysite, Oriental Orthodox): primary victims — face excommunication, exile, property seizure, suppression by orthodox authorities
 *   - Lay Interpreters and Mystical Practitioners: secondary victims — local theologies suppressed, identity-locked to orthodox framework or face community ejection
 *   - Ecumenical Councils: institutional mechanism for setting and enforcing creedal boundaries — convene to define orthodoxy and pronounce anathemas
 *   - Secular Rulers (emperors, kings): agenda-setters with strategic distance — enforce creedal uniformity through civil law and exile, maintain religious legitimacy through orthodoxy maintenance
 *   - Philosophical Naturalists (modern observers): analytical seat — examine how the constraint structures authority and suppresses alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.68).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.79).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Authority: Strict Orthodox Reading").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/doctrinal_history").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '6ef5f019-55ae-4a47-bd22-61860fc35222').
narrative_ontology:cs_kernel_codification('6ef5f019-55ae-4a47-bd22-61860fc35222', fixed_text).
narrative_ontology:cs_authority_grounding('6ef5f019-55ae-4a47-bd22-61860fc35222', lineage).
narrative_ontology:cs_interpretation_layer_present('6ef5f019-55ae-4a47-bd22-61860fc35222').
narrative_ontology:cs_reading_relation('6ef5f019-55ae-4a47-bd22-61860fc35222', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ef5f019-55ae-4a47-bd22-61860fc35222', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_axiom('6ef5f019-55ae-4a47-bd22-61860fc35222', foundational, metaphysical_assent_required_for_membership).
narrative_ontology:cs_axiom_status(metaphysical_assent_required_for_membership, holdable).
narrative_ontology:cs_axiom_grounding('6ef5f019-55ae-4a47-bd22-61860fc35222', metaphysical_assent_required_for_membership, deontological).
narrative_ontology:cs_axiom('6ef5f019-55ae-4a47-bd22-61860fc35222', secondary, hierarchical_magisterial_enforcement).
narrative_ontology:cs_axiom_status(hierarchical_magisterial_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('6ef5f019-55ae-4a47-bd22-61860fc35222', hierarchical_magisterial_enforcement, conventional).
narrative_ontology:cs_reference_frame('6ef5f019-55ae-4a47-bd22-61860fc35222', apostolic_metaphysical_authority).
narrative_ontology:cs_drift_state('6ef5f019-55ae-4a47-bd22-61860fc35222', contemporary_ecumenical_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6ef5f019-55ae-4a47-bd22-61860fc35222', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, orthodox_doctrinal_magistrates).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, mystical_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and councils set the authorized creedal interpretation and enforce doctrinal orthodoxy through ordination gating, sacramental access control, and excommunication. They benefit directly from the constraint's operation: their authority derives from their monopoly on legitimate interpretation, they collect deference and institutional standing, and the constraint protects their interpretive monopoly against lay and mystical challenge. They have exit options through reframing (councils can reaffirm, modify, or selectively enforce the creed), so their power atom remains institutional with arbitrage-grade mobility.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, beneficiary).

% Theologians, philosophers, and Church fathers recognized as authoritative interpreters of the creed. They benefit from the constraint's enforcement: their writings are cited as canonical, their interpretive authority is unchallenged within orthodox bounds, and deviation from their framing triggers heresy proceedings against others. They have reframing mobility — their writings can evolve the creed's meaning within the bounds of 'consistent development.'
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, orthodox_doctrinal_magistrates, beneficiary,
    institutional, generational, arbitrage, universal).

% Arian, Nestorian, Monophysite, and other communities holding metaphysical interpretations divergent from the Nicene orthodoxy. They bear the costs of the constraint: exclusion from sacraments, condemnation in councils, loss of institutional standing, and active suppression by orthodox authorities. Their interpretations are framed as heresy, their communities as schismatic or damned. They cannot exit while remaining in Christendom; their exit is either recantation (identity death) or migration to non-Orthodox communities.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    moderate, biographical, trapped, universal).

% Lay believers who hold idiosyncratic or locally-developed understandings of Christ's nature, the Trinity, or divine properties — interpretations that diverge from the approved creedal reading. They bear suppression: their interpretations are silenced in official contexts, they are subject to correction and penance, their children are taught the official creed, and public deviation can bring social censure or clerical sanction. Their identity as Christians is fused with their community's faith tradition; exit means leaving their community and their faith simultaneously.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    powerless, biographical, identity_locked, local).

% Mystics, contemplatives, and apophatic theologians whose spiritual experience and interpretive framework diverge from strict metaphysical orthodoxy. They are treated with suspicion: their claims to direct encounter with the divine are subordinated to creedal doctrine, their teachings are subject to examination by orthodox magistrates, and extreme cases are pursued as heresy. They have constrained exit: they can retreat into individual contemplative practice, but institutional recognition and teaching authority are conditional on creedal conformity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, mystical_practitioners, payer,
    moderate, biographical, constrained, regional).

% Formal gatherings of bishops that convene to settle doctrinal disputes and issue binding creedal statements. They are the formal mechanism through which the constraint is authored and enforced: councils vote on creedal language, issue anathemas against heterodoxy, and establish the grounds for excommunication. They possess analytical optionality — they can theoretically convene a new council to revise prior creedal statements, though institutional inertia and apostolic authority claims limit this mobility.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, ecumenical_councils, agenda_setter,
    institutional, generational, analytical, universal).

% Christian emperors and kings who enforce creedal orthodoxy through civil law, exile, and property confiscation. They benefit indirectly by maintaining ecclesiastical legitimacy and social order; they also constrain the clergy by reserving final authority to themselves. Their exit options are high — they can revise religious policy, tolerate heterodoxy for political reasons, or withdraw enforcement machinery. The constraint persists in part because secular rulers find it serviceable for maintaining religious uniformity as a tool of political control.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, secular_rulers, agenda_setter,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, secular_rulers, excluded).

% Communities outside the official ecclesiastical structure — frontier churches, isolated communities, Coptic and Oriental Orthodox churches — that never accepted or predate the Nicene creed's enforcement. They are excluded from the constraint's scope in one sense (not bound by its formal machinery) but also exclude themselves from orthodox fellowship and sacramental communion. They have no effective exit from their position; they exist in a structurally separated category.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, non_nicene_diaspora, excluded,
    powerless, generational, trapped, continental).

% Observers from outside the Church — philosophers, historians, modern scholars — who analyze the creed's operation and its relationship to power, knowledge, and identity. They take testimony from all other seats, examine historical records of councils and persecutions, and produce analysis of how the constraint structures ecclesiastical authority and suppresses alternatives.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, philosophical_naturalists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single shared metaphysical framework for Christological interpretation, enabling unified worship, sacramental coherence, and doctrinal dispute resolution across a geographically dispersed and culturally diverse Church. The coordination problem is: how to maintain Christianity as a unified faith rather than fragmenting into mutually contradictory sects with no shared foundation.
% TRANSFER_FUNCTION: Transfers authority from dispersed interpretation to hierarchical magistrates: individual lay believers and local communities must surrender their own metaphysical judgments to the authorized creedal reading, enforced by bishops and councils. This transfers institutional control, deference, and standing to the clergy and away from heterodox communities. It also transfers the cost of maintaining doctrinal boundaries — excommunication, suppression, and heresy proceedings — onto those whose interpretations deviate.
% ABSENT_VOICES: Heterodox communities (Arian, Nestorian, Monophysite, etc.) are structurally excluded from councils and from the process of setting creedal definitions. They would argue that the creed privileges one metaphysical reading as universally binding, suppresses legitimate theological pluralism, and uses ecclesiastical power to enforce metaphysical claims that should remain open to philosophical debate. Lay interpreters are also absent — their local theologies and lived interpretations are overridden by top-down creedal enforcement. Mystical practitioners and apophatic theologians would argue that rigid metaphysical language oversimplifies and distorts the apophatic dimension of divine mystery.
% DISAPPEARANCE_RATIONALE: If the strict Nicene constraint vanished overnight — if creeds no longer bound believers to a single metaphysical ontology and deviation no longer warranted sanction — Christianity would reorganize around multiple legitimate interpretive frameworks. Heterodox communities would reemerge into open communion; lay interpreters would recover local theological authority; mystical and apophatic approaches would be fully legitimate; the episcopate would lose its primary mechanism for enforcing doctrinal uniformity. The Church would fragment into genuinely coexisting theologies rather than persisting as an enforced orthodoxy with hidden heterodoxy.
% FOUNDING_PROBLEM: In the early centuries after Christ, Christian communities developed divergent understandings of Christ's nature (fully divine? fully human? how related to God the Father?), the composition of God (Trinity?), and the proper relationship between divine and human natures. These divergences threatened to fracture Christianity into incommensurable sects with no shared doctrinal foundation, making unified worship, sacramental practice, and Church governance impossible. The Nicene creed was authored at the Council of Nicaea (325 CE) to settle these disputes by defining an orthodox metaphysical position binding on all Christians.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchical clergy and orthodox magistrates attest that the founding problem of doctrinal fragmentation is still live and requires the constraint. Modern historians, philosophers, and scholars outside the benefiting parties attest that the founding problem has been substantially transformed: doctrinal divergence persists (modern Christianity comprises multiple denominations with incompatible metaphysics), but is now managed through separate ecclesial communities and ecumenical dialogue, not through excommunication and heresy proceedings. The constraint's persistence is better explained by institutional inertia, the clergy's interest in maintaining authority, and cultural embedding, than by the ongoing reality of the founding problem.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.68 at interval end) because the constraint's primary function — transferring metaphysical authority from dispersed communities to the hierarchical clergy — is purely extractive; no genuine coordination problem requires that *this specific metaphysical reading* bind all believers (alternative frameworks could serve the coordination function equally well). Suppression is higher still (0.79) because the constraint's persistence depends critically on active enforcement: excommunication, heresy proceedings, intellectual exclusion, and secular exile. Theater ratio rises over time (0.18 → 0.42) because as heterodoxy was suppressed and driven underground, the visible enforcement machinery became increasingly theatrical — the creed was recited and councils convened, but the active heretical resistance diminished and the enforcement became more about maintaining institutional performance than suppressing live alternatives. Accessibility collapse is high (0.72) because once the creed's binding force is accepted, alternatives appear genuinely closed off — they are not options within the Christian framework, they are apostasy. Resistance peaks in the medieval period (when heterodox communities were most organized) and declines as suppression became total, but never reaches zero because mystical and lay interpretive traditions persisted in hidden or peripheral forms. The measurement grid shares one time axis: all metrics are measured at the same six historical moments, enabling temporal correlation analysis.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchical clergy's seat, the constraint is genuine coordination binding a dispersed Church to a shared metaphysical foundation — they frame it as apostolic, necessary, and protecting truth. From the heterodox and lay interpreter seats, the constraint is pure extraction: the monopolization of metaphysical authority by a power-holding elite, enforced through suppression and identity-locking. The engine computes these divergent classifications from the structural data — the beneficiary/victim declarations, the exit options (institutional arbitrage vs. identity-locking), and the active enforcement requirement. The claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope (coordination + enforcement) from the orthodox seat, but the authored metrics describe highly extractive, heavily suppressive operation — the divergence is what the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchical clergy are structural beneficiaries (d near 0.0): they collect interpretive authority, institutional standing, and deference; they have exit options through reframing and doctrinal development; they face zero suppression from the constraint. Heterodox communities are structural targets (d near 1.0): they bear the constraint's full extraction through exclusion, suppression, and identity threat; their exit is recantation (identity death) or emigration; they have no reframing mobility. Lay interpreters are even more constrained (d above 0.85): their exit is identity-locked (cannot be Christian and heterodox simultaneously in most contexts); they lack institutional reframing options; suppression reaches them through sacramental gating and community enforcement. Secular rulers sit in the range 0.4-0.6: they benefit from the constraint's maintenance of religious uniformity, but they have mobile exit options and can revise policy; they are neither pure beneficiaries nor pure targets. The directionality derivation from beneficiary/victim declarations and exit options produces these asymmetric values; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (doctrinal fragmentation threatening Church unity) was substantially solved by the Nicene creed's establishment and enforcement in the 4th-5th centuries. However, the constraint persists with rising extractiveness (0.48 → 0.68 over the interval) long after the founding problem is dead — heterodox communities were suppressed into separation, internal variations submitted to orthodoxy, and the doctrinal unity was institutionally locked. The persistence is explained not by ongoing coordination need (the problem is solved) but by institutional inertia, the clergy's interest in authority, and cultural embedding (born Christians are taught the creed as cultural inheritance, not as contested metaphysics). The rising theater ratio (0.18 → 0.42) indicates proxy-goal drift: the visible creedal enforcement becomes increasingly theatrical — councils reaffirm orthodoxy, heresy is condemned, but the active struggle is over. By the modern period (1870 and later), the constraint is substantially a piton — inertial theater of heresy hunting with little live alternative to suppress — though this JSON captures only through 1870. The tangled_rope classification holds because the constraint DOES perform coordination (shared metaphysical framework enables unified worship), but the coordination is separable from THIS reading's specific metaphysics — another reading could provide equivalent coordination with lower extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_reading_distinction,
    'Is the Nicene Creed''s persistent authority best understood as grounded in the creed''s metaphysical truth-content (the strict reading), or as grounded in community liturgical practice and identity construction (the habituation reading), or as grounded in ongoing community discernment (the symbolic reading)?',
    'Examine historical evidence of how the creed is invoked and enforced: if enforced through metaphysical uniformity requirements, the strict reading explains the data; if invoked primarily in liturgical contexts without metaphysical enforcement, the habituation reading explains it; if invoked through community consensus and dialogue processes, the symbolic reading explains it.',
    'If the habituation or symbolic reading better explains the creed''s actual operation, the constraint''s extracted value drops substantially because the binding force becomes voluntary participation in shared practice rather than enforced metaphysical assent, and the beneficiary/victim structure shifts (clergy retain institutional standing but lose metaphysical authority monopoly; heterodox communities are less victimized because their metaphysics are not the enforcement target).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_distinction, conceptual, 'Which reading of the Nicene Creed kernel correctly characterizes its actual operating logic').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of heterodox theology structurally enforced (through institutional mechanisms like excommunication, exile, property seizure) or substantially internalized (heterodox communities have absorbed the orthodox framework and suppress their own alternative interpretations)?',
    'Examine the post-exit trajectories of heterodox individuals and communities: if suppression persists after institutional enforcement mechanisms are removed (e.g., in modern secular contexts where excommunication has no civil force), the suppression is substantially internalized; if it dissolves when institutional machinery is dismantled, it is primarily structural.',
    'If internalized, the effective suppression on heterodox targets is higher than the structural measure (0.79) suggests — the constraint travels with them after exit and their exit options are even more constrained. If primarily structural, the measured suppression is approximately accurate and alternative readings would substantially reduce it by removing the enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the suppression of heterodoxy in Nicene enforcement is structural or internalized').

omega_variable(
    coordination_extraction_separability,
    'Is a single binding metaphysical framework structurally necessary for Church unity and sacramental coherence, or could genuine coordination be achieved through alternative arrangements (e.g., liturgical uniformity without metaphysical mandate, or meta-agreement on doctrinal pluralism)?',
    'Natural experiment from ecumenical Christianity and modern denominations that maintain communion despite metaphysical divergence: if these communities sustain effective coordination without strict metaphysical binding, the coordination and extraction are separable.',
    'If separable, the coordination function of the creed (ε_coordination ~0.15) is minimal compared to the extraction component (ε_extraction ~0.53); a reformulated creed could solve the coordination problem with far lower extractiveness. If inseparable, part of the measured extraction is the necessary price of the coordination itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether metaphysical uniformity is structurally necessary for the creed''s coordination function').

omega_variable(
    founding_problem_death_vs_zombie,
    'Is the Nicene creed''s persistence best explained by the ongoing need to prevent doctrinal fragmentation (founding problem is live), or by institutional inertia and the clergy''s authority interest despite the problem''s resolution (founding problem is dead and the constraint is a zombie)?',
    'Compare creedal enforcement intensity in periods with active heterodoxy to periods when heterodoxy is suppressed into separation. If enforcement intensity remains high or increases after heterodoxy is separated and suppressed, the zombie hypothesis is supported. If enforcement intensity declines with heterodoxy suppression, the live-problem hypothesis is supported.',
    'If the zombie hypothesis is correct, the constraint''s claimed type should be reclassified from tangled_rope (coordination + enforcement) to piton (theater of inertial enforcement without functional necessity). This would indicate mandatrophy: the founding problem is dead, but the constraint persists as institutional theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_death_vs_zombie, empirical, 'Whether the Nicene creed''s founding problem remains live or is dead but the constraint persists as zombie theater').

omega_variable(
    lay_interpreter_identity_lock_mechanism,
    'Is the identity-locking of lay believers to orthodoxy caused by institutional suppression (external barriers to heterodoxy), or by socialization and cultural embedding (internalized orthodoxy), or by genuine theological conviction?',
    'Examine lay communities in contexts where institutional suppression is removed (modern secular societies, interfaith environments) and measure the rate of theological divergence, reinterpretation, and exit from orthodoxy. If divergence is common, identity-lock is primarily institutional. If divergence is rare despite removal of institutional barriers, identity-lock is primarily internalized.',
    'If primarily institutional, reducing suppression (removing the enforcement machinery) would reduce identity-lock and increase exit options (d drops for lay victims). If primarily internalized, the identity-lock persists even after institutional machinery is removed, and the constraint''s effective suppression remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_interpreter_identity_lock_mechanism, empirical, 'Whether lay identity-locking to Nicene orthodoxy is institutional suppression or internalized through socialization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 325, 1870).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 325, 0.18).
narrative_ontology:measurement(nice_tr_t525, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 525, 0.25).
narrative_ontology:measurement(nice_tr_t800, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 800, 0.32).
narrative_ontology:measurement(nice_tr_t1200, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1200, 0.4).
narrative_ontology:measurement(nice_tr_t1550, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1550, 0.43).
narrative_ontology:measurement(nice_tr_t1870, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1870, 0.42).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 325, 0.48).
narrative_ontology:measurement(nice_be_t525, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 525, 0.58).
narrative_ontology:measurement(nice_be_t800, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 800, 0.64).
narrative_ontology:measurement(nice_be_t1200, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1200, 0.7).
narrative_ontology:measurement(nice_be_t1550, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1550, 0.72).
narrative_ontology:measurement(nice_be_t1870, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1870, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 325, 0.52).
narrative_ontology:measurement(nice_su_t525, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 525, 0.68).
narrative_ontology:measurement(nice_su_t800, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 800, 0.75).
narrative_ontology:measurement(nice_su_t1200, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1200, 0.82).
narrative_ontology:measurement(nice_su_t1550, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1550, 0.84).
narrative_ontology:measurement(nice_su_t1870, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1870, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__strict_orthodox_reading, 0.22).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, heresy_suppression_apparatus).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, episcopal_doctrinal_authority).

% DUAL FORMULATION NOTE:
% The Nicene Creed Authority kernel decomposes into three constraint stories with structurally distinct ε values and beneficiary/victim structures: (1) strict_orthodox_reading (this story) — high extraction through metaphysical uniformity enforcement; (2) liturgical_habituation_reading — creed as identity performance, lower extraction; (3) symbolic_confessional_reading — creed as community discernment, extraction becomes negotiable. The three readings coexist as live interpretive stances within Christianity, held by different institutional factions and modern theological movements. No single reading has foreclosed the others; they persist in institutional separation (Roman Catholic/Orthodox/various Protestant denominations each emphasis different readings). This constraint (strict orthodoxy) has historically dominated institutional authority structures, but the other readings are increasingly mainstream in modern ecumenical and liberal theology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
