% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Confessional Commitment Incompatibility Structure (Theological Fragmentation Reading)
 *   domain: religious/historical/political
 *
 * SUMMARY:
 *   This story instantiates the theological_fragmentation_reading of the
 *   reformation_composite kernel (see commentary.kernel_context): the
 *   fragmentation-generating structure of the Reformation is read here as the
 *   confessional-commitment apparatus — competing soteriological and
 *   ecclesiological commitments, codified in fixed confessional texts
 *   (Augsburg Confession, Book of Concord, Canons of Trent, Westminster
 *   Confession), that made the denominations structurally incompatible and
 *   kept them so. The ε referent is the standing arrangement under contest:
 *   the confessional-commitment order of Western Christendom from the Edict
 *   of Worms to the Toleration Act era (1520-1700), assessed by this
 *   reading's own lights — doctrinal pluralism as the primary observable,
 *   confessional documents as the constraint's artifacts, denominational
 *   leadership as the party positioned to collect from the fragmentation. The
 *   apparatus solved a real problem (per-community doctrinal coordination
 *   once the single adjudicating authority was rejected) while transferring
 *   obedience, revenue, and identity-allegiance to confessional institutions
 *   and pricing dissent at censure, exclusion, and — in the radical case —
 *   death. Claimed type and metrics are authored independently: the
 *   structural reading is tangled_rope (genuine per-confession coordination
 *   plus asymmetric collection plus active enforcement); the metric values
 *   describe the apparatus as the historical record shows it. The
 *   logical-incompatibility core of the doctrinal commitments is decomposed
 *   out (see network.dual_formulation_note) so this story carries a single
 *   stable ε. The structure persists past 1700 in attenuated, disestablished
 *   form; the interval ends where enforcement machinery visibly decays.
 *
 * KEY AGENTS:
 *   - denominational_leadership: agenda-setter and primary beneficiary (institutional/identity_locked) — administers the confessional standards and collects office, authority, and livelihood from the boundaries it maintains
 *   - confessional_theologians: secondary beneficiary (organized/constrained) — careers and chairs constituted by confessional distinctives; collect without administering
 *   - confessional_laity: primary payer (moderate/identity_locked) — bears tithes, discipline, closed communion, and confessional war costs; exit means losing community and standing
 *   - intra_confessional_dissenters: payer (moderate/constrained) — Arminians, Jansenists, Puritans; disciplined inside their own confession
 *   - radical_reformation_communities: excluded payer (powerless/trapped) — Anabaptists and spiritualists; persecuted by every confession, admitted to no colloquy
 *   - ecumenical_irenic_advocates: excluded (moderate/identity_locked) — Calixtus and the irenicists; reconciliation proposals rejected as syncretism by all sides
 *   - historical_epistemologists: analytical observer (analytical/analytical) — comparative seat from which the whole confessional record is visible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.64).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.58).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Confessional Commitment Incompatibility Structure (Theological Fragmentation Reading)").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "religious/historical/political").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '563f2682-e58f-45e0-a880-009591e08c7c').
narrative_ontology:cs_kernel_codification('563f2682-e58f-45e0-a880-009591e08c7c', fixed_text).
narrative_ontology:cs_authority_grounding('563f2682-e58f-45e0-a880-009591e08c7c', lineage).
narrative_ontology:cs_interpretation_layer_present('563f2682-e58f-45e0-a880-009591e08c7c').
narrative_ontology:cs_reading_relation('563f2682-e58f-45e0-a880-009591e08c7c', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('563f2682-e58f-45e0-a880-009591e08c7c', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('563f2682-e58f-45e0-a880-009591e08c7c', foundational, confessional_separation_doctrinally_necessary).
narrative_ontology:cs_axiom_status(confessional_separation_doctrinally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('563f2682-e58f-45e0-a880-009591e08c7c', confessional_separation_doctrinally_necessary, deontological).
narrative_ontology:cs_axiom('563f2682-e58f-45e0-a880-009591e08c7c', secondary, coercive_confessional_uniformity_legitimate).
narrative_ontology:cs_axiom_status(coercive_confessional_uniformity_legitimate, overridden).
narrative_ontology:cs_axiom_grounding('563f2682-e58f-45e0-a880-009591e08c7c', coercive_confessional_uniformity_legitimate, conventional).
narrative_ontology:cs_reference_frame('563f2682-e58f-45e0-a880-009591e08c7c', confessional_doctrinal_commitment_framework).
narrative_ontology:cs_drift_state('563f2682-e58f-45e0-a880-009591e08c7c', contemporary_ecumenical_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('563f2682-e58f-45e0-a880-009591e08c7c', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_theologians).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, confessional_laity).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, intra_confessional_dissenters).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, radical_reformation_communities).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, ecumenical_irenic_advocates).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, confessional_boundary_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Consistories, synods, bishops, and presbyteries administer the confessional standards: they examine ordinands against the confession, convene discipline for deviation, decide which books may be taught, and represent the confession in disputes with rivals. The standards are also the source of their office — a pastor's or bishop's standing exists only inside a confession that certifies him, so leaving the confession means leaving office. Their livelihoods, authority, and succession arrangements are all constituted by the boundary they administer.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, denominational_leadership, beneficiary).

% University faculties and academies staffed by confessional theologians: chairs, disputations, and publishing careers are organized around defending and elaborating the confession's distinctives. A Tubingen Lutheran dogmatics professor has no path to a Louvain chair; expertise is confession-specific and largely non-transferable. They collect salaries, status, and institutional protection without administering the discipline machinery itself.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_theologians, beneficiary,
    organized, biographical, constrained, continental).

% Ordained believers baptized into a confession: they attend its liturgy, submit to its discipline, pay its tithes and assessments, and may receive sacraments only at its altars. Marriage, burial, and poor relief run through confessional institutions. Crossing to a rival confession means losing community, family standing, and in many territories legal standing; dissenting within it brings censure or excommunication. Congregational organization gives some collective voice, but individual exit is effectively closed.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_laity, payer,
    moderate, biographical, identity_locked, continental).

% Arminians before Reformed synods, Jansenists in Catholic faculties, Puritans under the Anglican settlement: members who hold the confession's core but dispute its elaborations. They bear investigation, suspension, and removal from office. Their options are recantation, silence, forming splinter bodies, or crossing to a rival confession at the cost of everything confessional identity carries.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, intra_confessional_dissenters, payer,
    moderate, biographical, constrained, continental).

% Anabaptist and spiritualist communities proposing voluntary membership and believers' baptism: admitted to no confessional colloquy, anathematized in Catholic and Protestant documents alike, and persecuted by every territorial church — Felix Manz drowned in Zurich in 1527, Munster suppressed in 1535, congregations meeting in fields and cellars for a century afterward. No jurisdiction in Western Christendom offers them legal standing; exit means the margins of the continent or the New World.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, radical_reformation_communities, excluded,
    powerless, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, radical_reformation_communities, payer).

% Irenic theologians such as Georg Calixtus and the later philadelphian networks who propose that the confessions recognize each other's baptism and ordination and narrow the anathemas. Every side receives the proposal as betrayal: Lutherans charge Calixtus with syncretism, Rome treats Protestant orders as void regardless, and Reformed synods discipline the willing. They remain inside their own confessions — their standing depends on the very boundaries they propose to soften.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_irenic_advocates, excluded,
    moderate, biographical, identity_locked, continental).

% Analysts of the confessional record — historians of doctrine, sociologists of religion, epistemologists of testimony — who read the confessional documents, discipline registers, and conversion records across all confessions. They hold no confessional standing and collect no confessional revenue; their seat is the comparative one from which the whole structure is visible.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, historical_epistemologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within each confession, the confessional standard coordinates doctrine, liturgy, ordination, and discipline at scale: ministers are mutually certified, sacraments mutually recognized, teaching verifiable against a fixed text, and communal trust extended to co-confessors. Once the evangelical parties rejected the medieval church's single adjudicating authority, some per-community doctrinal-coordination structure was a real requirement; the confessional documents solved it per-community.
% TRANSFER_FUNCTION: Moves obedience, tithes and assessments, and identity-allegiance from laity to denominational institutions; moves legitimacy, livelihood, and career advancement to clergy and confessional theologians; moves the costs of doctrinal disagreement — censure, suspension, exclusion, in the radical case death — onto dissenters within each confession and onto rival confessions across each boundary.
% ABSENT_VOICES: Radical reformers had no seat at any confessional deliberation: Augsburg, Trent, and every synod excluded them while all confessions agreed on their suppression. Irenic advocates of reunion spoke inside the system and were rejected as syncretists. Unlettered laity encountered the confessions only as catechetical objects; their assent was presumed, not solicited.
% DISAPPEARANCE_RATIONALE: If the confessional-commitment structure dissolved overnight — subscription tests dropped, mutual recognition of orders and sacraments, boundaries opened — the denominational map would rearrange within a generation: mergers and shared ministries where doctrine permitted, loss of confession-specific livelihoods, reorganization of theological faculties around shared curricula, and collapse of the mutual-anathema structure that had organized Western European religious life for 150 years.
% FOUNDING_PROBLEM: After 1517, the doctrinal disputes (indulgences, justification, authority, sacraments) could no longer be resolved inside one framework: the evangelical parties rejected the existing adjudicating authority, and each party then faced doctrinal drift, rival claimants, and the practical problem of coordinating a church without a shared magisterium. The confessional documents were built to solve that problem per-community: fix the doctrine, certify the ministers, mark the boundary.
% FOUNDING_PROBLEM_CORROBORATION: Denominational leadership attests the founding problem is live wherever communities codify doctrine without a shared authority. Corroboration from outside the beneficiary set: the Joint Declaration on the Doctrine of Justification (1999) and subsequent ecumenical agreements — signed by the very traditions whose confessions anathematized each other — attest that the founding incompatibilities are substantially reframed or resolved; confessionalization historiography (Schilling, Reinhard) attests the boundaries functioned as identity and political artifacts as much as doctrinal necessities. No party outside the benefiting set attests that the specific confessional boundaries remain doctrinally necessary.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.64 (interval end): the apparatus moves obedience, tithes, and identity-allegiance from laity to confessional institutions, and the coordination it provides is real but priced at submission — the per-confession coordination problem was genuinely unsolved after 1517, and the confessional documents solved it per-community while paying their administrators from the boundary. Suppression is 0.58 at interval end, down from a war-time peak of 0.88 (1648): the suppression series is authored because enforcement-capacity change IS the story here — machinery built up from the Edict of Worms through the Thirty Years' War, then decayed through the settlement era (Toleration Act 1689), with the French revocation of 1685 a local spike against the general decay. Theater rises from 0.10 to 0.30: confessional activity was substantially functional in the first generations (catechesis, ordination, discipline against live drift), then an increasing share became performative boundary-work — anathematizing settled disputes, subscription rituals, polemic against long-dead opponents — trimmed slightly by 1700 as pietist and devotional movements revived functional practice inside the confessions. Accessibility_collapse 0.58: within a confession, alternatives collapse hard (a member cannot hold Arminian views in a strict Reformed church without discipline), but conversion across confessional lines remained possible at high cost, so collapse is substantial but not total — well below natural-law levels. Resistance 0.55: sustained — radical movements, intra-confessional parties, recusancy, irenic proposals. All three series run on one shared time grid (1521, 1555, 1577, 1618, 1648, 1685, 1700) anchored to enforcement events, so every metric is authored at every examined point. Identity-lock is the load-bearing exit fact on the payer side: confessional identity fused at baptism, through marriage networks, through livelihood (clergy), and through conviction that rival confessions forfeit salvation — exit priced not just socially but eschatologically.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently from the same structure. From denominational leadership's position the confessional order is the necessary form of truth-in-community: they stewarded doctrine against drift and rivals, and the boundary is what makes their office, sacrament, and teaching coherent — the arrangement looks like the price of doctrinal integrity. From the laity and dissenter seats the same structure operates as closed exits and priced dissent: they carry its costs (war, censure, exclusion from rival altars) without setting its terms. The theologians' seat is intermediate: benefit without administration — they collect the returns of confessional distinctiveness while the consistories bear the enforcement work. The radical communities' seat exposes the structure's edge: the one policy all confessions coordinated on was their suppression, which shows boundary-maintenance interest outranking doctrinal disagreement among the confessions themselves — a coalition-failure for the powerless that no within-confession coalition could replicate. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: denominational_leadership collects office, authority, and livelihood directly from the boundary (d near the beneficiary end — they administer and collect); confessional_theologians collect status and salary from confessional distinctives without administering (also near the beneficiary end, slightly above leadership because their position rides on the boundary without running it). Victim declarations map to the cost-bearers: confessional_laity (identity_locked) sit near but not at the full-target end because each believer also receives their own confession's coordination — sacraments, community, trust — at the price of submission; intra_confessional_dissenters (constrained) sit higher — they pay discipline without even the untroubled benefit of belonging; radical_reformation_communities (trapped) sit at the full-target end — no jurisdiction admits them, so no offsetting benefit exists anywhere in the system. ecumenical_irenic_advocates sit high: they bear the boundary's costs while proposing its dissolution, and their own standing depends on the boundary they contest. Scope amplification: the arrangement is continental for all parties, so verification of enforcement is hard and effective extraction scales modestly upward from base ε — the engine owns that arithmetic; the authored scope atoms feed it.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. A pure-extraction reading would erase the genuine coordination achievement: after 1517 there was no shared adjudicating authority, and some doctrinal-coordination structure was a real requirement — the confessional documents solved per-community coordination of ordination, sacramental recognition, and teaching at continental scale. A pure-coordination or natural-limit reading would erase the collection: the boundaries were actively enforced, the enforcement paid their administrators, and reconciliation options were rejected when offered (Calixtus) — evidence that leadership interest held the boundary beyond what doctrine alone required. The mandatrophy question is kept open rather than resolved: the founding problem (doctrinal coordination after authority-collapse) is contested — live wherever communities codify doctrine without a shared magisterium, substantially reframed by the confessions' own later ecumenical agreements. The drift_state (axiom_overriding, substantial, acknowledged) records that the traditions themselves have formally declared several anathematized differences non-church-dividing while maintaining the institutional boundaries — the mandate has partially outlived its original form without the arrangement dissolving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_primacy_committal,
    'This story commits to the theological_fragmentation_reading of the reformation_composite kernel — doctrinal pluralism as the primary observable of the fragmentation-generating structure. Is that primacy correct, or does the political_realignment_reading (sovereignty-seeking as primary generator) or technological_mediation_reading (print mediation as primary generator) identify the true primary observable?',
    'Comparative analysis holding one factor constant: territories with identical doctrinal packages under different political regimes (does fragmentation track doctrine or sovereignty-seeking?), and doctrinal movements with and without print access at matched scale. Whichever factor best predicts where and how fast incompatible denominations formed is the primary observable.',
    'If political primacy holds, the beneficiary set shifts toward territorial princes and this story''s confessional apparatus reclassifies as an instrument of the political reading''s arrangement; if technological primacy holds, the constraint''s effective scope follows print-network reach rather than confessional jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_primacy_committal, conceptual, 'Committer structure: this story is one reading of the reformation_composite kernel; sibling readings would change the beneficiary set and the ε referent.').

omega_variable(
    logical_incompatibility_vs_enforced_boundary,
    'Is the incompatibility of the denominations a logical consequence of the doctrinal commitments themselves (contradictory soteriological and ecclesiological propositions cannot both be normative for one community), or a socially maintained boundary kept rigid by enforcement and by leadership interest in the boundary?',
    'Compare communion practice across the same doctrinal differences where institutions permitted it: the gradual pre-1054 divergence with communion intact, and modern ecumenical agreements (culminating in the 1999 Joint Declaration on the Doctrine of Justification) that formally bracket differences the sixteenth-century confessions anathematized. If shared communion persists across the same differences when institutions choose it, the social incompatibility is constructed rather than logical.',
    'If constructed, this story''s ε measures institutional boundary-maintenance and the enforcement layer drifts toward pure extraction; if logical, the propositional core is a separate no-beneficiary mountain and this story''s ε covers only the enforcement premium above it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(logical_incompatibility_vs_enforced_boundary, conceptual, 'Whether the denominational incompatibility is a logical limit of the doctrinal commitments or an enforced social boundary.').

omega_variable(
    suppression_structural_vs_internalized,
    'Was the force holding believers inside confessional boundaries structural (penal statutes, consistory courts, closed communion, establishment tests) or internalized (sincere conviction that rival confessions forfeit salvation, with identity fused to confession from baptism)?',
    'Post-toleration exit trajectories: when legal penalties lifted (Toleration Act 1689 and later emancipations), did cross-confessional movement rise toward the rate the structural barriers alone predict, or remain low? Conversion registers and marriage records across confessional lines after 1689 are the test series.',
    'If substantially internalized, effective suppression exceeds the structural measure and outlasts enforcement decay — the interval-end suppression value understates the arrangement''s hold, and identity-fused exits stay closed even with the legal doors open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether confessional boundary-holding was enforced from outside or carried inside the believers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1520, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1521, reformation_composite__theological_fragmentation_reading, theater_ratio, 1521, 0.1).
narrative_ontology:measurement_basis(refo_tr_t1521, observed).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__theological_fragmentation_reading, theater_ratio, 1555, 0.15).
narrative_ontology:measurement_basis(refo_tr_t1555, observed).
narrative_ontology:measurement(refo_tr_t1577, reformation_composite__theological_fragmentation_reading, theater_ratio, 1577, 0.22).
narrative_ontology:measurement_basis(refo_tr_t1577, observed).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__theological_fragmentation_reading, theater_ratio, 1618, 0.26).
narrative_ontology:measurement_basis(refo_tr_t1618, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.3).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).
narrative_ontology:measurement(refo_tr_t1685, reformation_composite__theological_fragmentation_reading, theater_ratio, 1685, 0.33).
narrative_ontology:measurement_basis(refo_tr_t1685, observed).
narrative_ontology:measurement(refo_tr_t1700, reformation_composite__theological_fragmentation_reading, theater_ratio, 1700, 0.3).
narrative_ontology:measurement_basis(refo_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1521, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1521, 0.42).
narrative_ontology:measurement_basis(refo_be_t1521, observed).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1555, 0.52).
narrative_ontology:measurement_basis(refo_be_t1555, observed).
narrative_ontology:measurement(refo_be_t1577, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1577, 0.6).
narrative_ontology:measurement_basis(refo_be_t1577, observed).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1618, 0.66).
narrative_ontology:measurement_basis(refo_be_t1618, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.7).
narrative_ontology:measurement_basis(refo_be_t1648, observed).
narrative_ontology:measurement(refo_be_t1685, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1685, 0.67).
narrative_ontology:measurement_basis(refo_be_t1685, observed).
narrative_ontology:measurement(refo_be_t1700, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1700, 0.64).
narrative_ontology:measurement_basis(refo_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1521, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1521, 0.55).
narrative_ontology:measurement_basis(refo_su_t1521, observed).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1555, 0.66).
narrative_ontology:measurement_basis(refo_su_t1555, observed).
narrative_ontology:measurement(refo_su_t1577, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1577, 0.74).
narrative_ontology:measurement_basis(refo_su_t1577, observed).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1618, 0.82).
narrative_ontology:measurement_basis(refo_su_t1618, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.88).
narrative_ontology:measurement_basis(refo_su_t1648, observed).
narrative_ontology:measurement(refo_su_t1685, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1685, 0.76).
narrative_ontology:measurement_basis(refo_su_t1685, observed).
narrative_ontology:measurement(refo_su_t1700, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1700, 0.58).
narrative_ontology:measurement_basis(refo_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_doctrinal_incompatibility_core).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'theological fragmentation' (ε-invariance principle): (1) the propositional incompatibility core — that the soteriological and ecclesiological claims as stated cannot all be normative in one community — is a logical structure with no beneficiary and negligible extraction; it is authored separately as reformation_doctrinal_incompatibility_core (mountain candidate). (2) The confessional-commitment apparatus that codifies the commitments, enforces the boundaries, and pays their administrators is THIS story (tangled_rope, ε 0.64). The two are linked because the apparatus cites the logical core as its justification — the upstream claim is invoked to naturalize the downstream enforcement. The kernel sibling readings (political_realignment, technological_mediation) are rival primacy theses over the same historical composite, linked for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
