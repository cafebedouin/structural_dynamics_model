% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Doctrinal-Fragmentation Reading of the Reformation
 *   domain: historical epistemology/religious history/political economy
 *
 * SUMMARY:
 *   This story authors ONE reading of the reformation_composite kernel: the
 *   claim that the Reformation is fundamentally a theological event,
 *   instantiated as a standing interpretive arrangement — the practice of
 *   teaching, administering, and commemorating the Reformation as a contest
 *   of incompatible soteriological and ecclesiological commitments that
 *   necessarily issues in structurally incompatible denominations. The
 *   arrangement has a genuine coordination function (it gives confessional
 *   communities a coherent account of separate existence and preserves the
 *   doctrinal content of the sixteenth century as serious history) and an
 *   asymmetric extraction structure (denominational leadership collects
 *   legitimacy and institutional continuity from presenting contingent
 *   division as principled necessity, while ecumenists, union churches,
 *   pluricausal historians, and laity bear the costs of division rendered
 *   permanent). The epsilon referent is the standing doctrine-first
 *   arrangement itself, priced from the authoring seat — never the integrated
 *   or ecumenical alternative this arrangement resists. Per the
 *   constraint-family rule, the sibling readings
 *   (political_realignment_reading, technological_mediation_reading) are
 *   separate files with their own epsilon values; they are referenced only
 *   through network edges and omega variables, never averaged into this
 *   story's metrics. KEY AGENTS (by structural relationship): -
 *   denominational_leadership: agenda-setting beneficiary
 *   (institutional/identity_locked) — administers confessional standards and
 *   collects legitimacy from the fragmentation account -
 *   confessional_theological_faculties: beneficiary
 *   (institutional/constrained) — curricula and certification depend on the
 *   doctrine-first frame - confessional_publishing_houses: beneficiary
 *   (organized/mobile) — sells to confessional identity markets -
 *   ecumenical_dialogue_participants: payer (organized/constrained) —
 *   vocation is crossing boundaries the account declares structural -
 *   union_churches: payer (institutional/constrained) — merged bodies living
 *   as counterexamples to the account - pluricausal_historians: payer
 *   (moderate/mobile) — integrated explanations subordinated as context -
 *   parish_laity: payer-beneficiary (moderate/identity_locked) — inherits
 *   division as principled permanence - grassroots_ecumenists: excluded
 *   (powerless/trapped) — practices reconciliation without a seat in
 *   adjudication - comparative_reformation_scholars: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.62).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.5).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Doctrinal-Fragmentation Reading of the Reformation").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical epistemology/religious history/political economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e').
narrative_ontology:cs_kernel_codification('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', distributed).
narrative_ontology:cs_authority_grounding('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', lineage).
narrative_ontology:cs_interpretation_layer_present('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e').
narrative_ontology:cs_reading_relation('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', foundational, soteriological_ecclesiological_primacy).
narrative_ontology:cs_axiom_status(soteriological_ecclesiological_primacy, holdable).
narrative_ontology:cs_axiom_grounding('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', soteriological_ecclesiological_primacy, empirically_contingent).
narrative_ontology:cs_axiom('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', secondary, confessional_documents_bind_incompatibility).
narrative_ontology:cs_axiom_status(confessional_documents_bind_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', confessional_documents_bind_incompatibility, conventional).
narrative_ontology:cs_reference_frame('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', doctrinally_generated_confessional_pluralism).
narrative_ontology:cs_drift_state('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', contemporary_pluricausal_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5f3c49d7-b6f5-42fb-8dd2-e71c7195ae4e', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_theological_faculties).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_publishing_houses).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, ecumenical_dialogue_participants).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, union_churches).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, pluricausal_historians).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, parish_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, parish_laity).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, doctrinal_exclusivism).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, confessional_boundary_permanence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Confessional bodies — Lutheran world federations, Reformed alliances, Anglican instruments, Roman dicasteries, Free-church conventions — maintain the doctrinal standards, ordination requirements, seminary curricula, and commemoration calendars through which the Reformation is taught as a contest of incompatible first principles. Their authority to speak for their communities rests on the account that separation follows from doctrine itself; revising that account would unsettle the very offices that administer it. Leaving the arrangement would mean dissolving the distinct confessional identity their office exists to guard.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, denominational_leadership, beneficiary).

% University faculties and seminaries organized around confessional theology train clergy, publish commentaries on the confessional documents, and staff the commissions that certify doctrinal fidelity. The doctrine-first account of the Reformation orders their curricula and justifies their separate existence alongside rival confessional faculties. Adopting an integrated account would shrink the distinct subject matter their institutions exist to teach.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_theological_faculties, beneficiary,
    institutional, biographical, constrained, continental).

% Publishers specializing in confessional literature — catechisms, confessional commentaries, anniversary and quincentenary volumes — sell to identity-marketed audiences whose demand depends on the Reformation being told as a contest of irreconcilable teachings. They can shift catalogues toward other religious markets, so their exposure is commercial rather than vocational.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_publishing_houses, beneficiary,
    organized, biographical, mobile, global).

% Bilateral commissions and councils — Lutheran-Catholic, Anglican-Roman, Reformed-Catholic — spend decades producing agreed statements that qualify the mutual condemnations inherited from the sixteenth century. Their vocation is precisely the boundary-crossing that the inherited account declares structurally impossible, so each round of dialogue must first argue past the presumption of irreconcilability before reaching the doctrines themselves.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_dialogue_participants, payer,
    organized, generational, constrained, global).

% United and uniting churches — the Prussian Union tradition, the Church of South India, the United Church of Canada, the Uniting Church in Australia — have merged formerly separate confessional streams into single bodies. They live as standing counterexamples to the account of necessary incompatibility and carry a persistent charge of doctrinal shallowness from confessional critics on both sides of the merges they effected.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, union_churches, payer,
    institutional, generational, constrained, national).

% Social, political, and book historians of the early modern period treat doctrinal difference as one variable among several interacting causes. In confessional venues their work is received as background or reductionism rather than explanation, and grant lines, journal space, and commemoration platforms skew toward doctrinal framing. They can redirect careers toward other periods or questions, at the cost of accumulated specialization.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, pluricausal_historians, payer,
    moderate, biographical, mobile, global).

% Members of confessional congregations inherit a division they did not choose, taught as the principled outcome of incompatible truths about salvation and the church. They receive belonging, catechesis, and communal identity from their confession, and they pay in divided families, duplicated institutions, and communion broken at shared tables. Switching confessions carries real social and familial cost, so most remain where they were born.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, parish_laity, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, parish_laity, beneficiary).

% Congregants who already share worship, covenant communities, and occasionally communion across confessional lines practice the reconciliation that the official account declares structurally unavailable. They hold no seat in synods, faculties, or dialogue commissions, and their local arrangements are treated as anomalies rather than data by the institutions that adjudicate the Reformation's meaning.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, grassroots_ecumenists, excluded,
    powerless, biographical, trapped, local).

% Historians and sociologists comparing the Reformation with other episodes of religious differentiation take testimony from all the other seats, weigh doctrinal against political and medial variables, and publish assessments that neither confessional institutions nor their critics control.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, comparative_reformation_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives each confessional community a coherent, teachable account of why it exists separately; preserves the doctrinal content of the sixteenth century as serious intellectual history; organizes curricula, catechesis, ordination standards, and commemoration around a shared explanatory frame.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy from integrative accounts and their advocates — ecumenists, union churches, pluricausal historians — to confessional leadership and academies; converts the contingency of division into the permanence of principle, shifting the burden of justification from the separated institutions to those who would reunite them.
% ABSENT_VOICES: Grassroots ecumenists who already share worship across confessional lines would object that lived practice contradicts the declared structural incompatibility; early modern populations assigned confession by princely fiat would object that affiliation often tracked jurisdiction rather than conviction. Both sit outside the synods, faculties, and commissions that adjudicate the account.
% DISAPPEARANCE_RATIONALE: If the doctrine-first account vanished overnight, confessional self-understanding would lose its principal justification for separate existence; ecumenical dialogue would lose the presumption it must argue past; seminary curricula, catechetical materials, and commemoration calendars would require rewriting; and historiographical funding and platform allocation would rebalance toward integrated accounts. Many arrangements depend on the account, so the world rearranges.
% FOUNDING_PROBLEM: After the confessional wars and into the modern era, each confessional community needed a stable account of why the Western church fragmented — one that justified separate existence as fidelity to truth rather than failure of charity or effect of politics. The theological-fragmentation reading supplied that account: division as the entailment of incompatible first principles.
% FOUNDING_PROBLEM_CORROBORATION: Secular early-modern historiography attests that explaining the fragmentation is a live problem while denying that doctrine alone answers it; the Joint Declaration on the Doctrine of Justification (1999) and subsequent bilateral consensus statements — produced by authorized interlocutors of the confessions themselves, outside the benefiting parties' unilateral control — attest that the structural-incompatibility premise is contested. No source outside the benefiting parties attests the permanence claim itself.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.62 at interval end: the doctrine-first account converts a contingent historical outcome into a principled necessity, and that conversion is what the separated institutions collect on — but the account also carries real doctrinal content, bounding epsilon below snare territory. Suppression is 0.50 and is authored as a raw structural property, unscaled by power or scope: enforcement runs through ordination standards, curricular control, and the dismissal of integrative work as reductionism, not through coercion of persons. Theater_ratio is 0.38: catechesis and doctrinal instruction remain functional, but a growing share of activity is commemorative and identity-performative (anniversary volumes, quincentenary events), tracking the rise in the temporal series. Accessibility_collapse is 0.50: once the doctrine-first frame is seen as a constructed interpretive choice, the political and technological alternatives become visible and persist in secular academia, so alternatives are marginalized rather than eliminated. Resistance is 0.60: the ecumenical movement, union churches, and pluricausal historiography actively contest the account, with the 1999 Joint Declaration as the highest-profile instance. Claimed type and metrics are independent authored facts: tangled_rope is stated from the structure (genuine coordination plus named payers plus active enforcement); the metrics describe operation as observed. The measurement series run on one shared time grid (1580, 1700, 1800, 1900, 1999, 2026) with every tracked metric authored at every point. Identity-lock dynamics: denominational_leadership and parish_laity sit at identity_locked exit — for leadership the lock is institutional identity fusion (the office has become the account it administers), for laity relational and formative identity (confession as inherited selfhood); if either frame broke, the account's enforcement cost would drop sharply. Suppression mechanism is predominantly structural (gatekeeping over credentials, curricula, and platforms) rather than internalized, though laity-side identity attachment supplies an internalized complement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the agenda-setter seat, the account is faithful stewardship: doctrine really was at stake in the sixteenth century, and teaching the incompatibilities honestly is fidelity, not rent collection. From the payer seats, the same account operates as legitimated division: ecumenists must spend decades arguing past a presumption of impossibility, union churches carry a standing charge of shallowness, and historians watch their variables demoted to context. Parish_laity is genuinely dual-positioned — real belonging received, real division paid — and should compute near symmetric rather than at either pole. Coalition potential among the payers is real but historically under-realized: elite coalitions (Lutheran-Catholic dialogue) have produced qualified convergence statements, while the grassroots coalition that already practices boundary-crossing lacks any seat in adjudication; a laity-ecumenist coalition is the credible threat to the account's persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational_leadership sits nearest the beneficiary pole: it writes the standards, enforces them, and collects the legitimacy the account distributes; identity_locked exit amplifies its subsidy. Confessional_theological_faculties and confessional_publishing_houses collect without running the arrangement — constrained and mobile exit respectively modulate their positions. The payer seats sit toward the target pole: ecumenical_dialogue_participants and union_churches are constrained (their vocations are the boundaries in question), pluricausal_historians are mobile (real arbitrage-grade exit to other fields damps their effective extraction), and parish_laity is identity_locked with mixed flows, placing it mid-range. Grassroots_ecumenists bear costs with no seat at all — high target-directionality with zero agenda influence. Comparative_reformation_scholars are analytical and direction-neutral. Beneficiary/victim declarations map onto these relationships directly; no directionality overrides were needed because exit-option variation already differentiates the same-nominal-power seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — accounting for the fragmentation — is contested rather than dead: the question persists, but whether the doctrinal answer suffices is disputed by the confessions' own authorized interlocutors and by secular historiography, so no mandatrophy_resolved declaration is authored. Classification prevents mislabeling in both directions: reading the arrangement as pure extraction (snare) would erase the genuine coordination it performs — confessional identity, preserved doctrinal seriousness, teachable continuity — and would misread honest truth-claims as rent-seeking; reading it as pure coordination (rope) would erase the concentrated capture (gain_flow lands on denominational_leadership) and the suppressed alternatives. Tangled_rope holds both halves. Watch item: theater_ratio has risen monotonically across the interval while the doctrinal function slowly atrophies relative to commemorative performance; if the doctrinal core continues thinning while identity performance carries the arrangement, the structure drifts toward piton — an account maintained theatrically by administrators who could revise it but for whom the cost of revision exceeds what they bear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates one reading of the reformation_composite kernel; what would the classification become if a sibling reading (political_realignment_reading or technological_mediation_reading) were adopted as the primary observable?',
    'Comparative analysis across the three sibling stories: derive which beneficiary/victim sets and which epsilon each reading implies, then test which reading''s predicted observables (doctrinal alignment versus jurisdictional alignment versus press penetration) best track denominational boundary formation.',
    'Under the political reading, denominational leadership becomes instrument rather than capturer and state agendasetters enter the seat set; under the technological reading, gains accrue to printers and infrastructure owners. Effective extraction and per-seat type could shift for every seat in this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    print_vector_vs_doctrinal_content,
    'Is doctrinal content the generative structure of denominational fragmentation, or did print infrastructure amplify whatever dissent existed regardless of doctrinal depth?',
    'Compare regions with similar press penetration but divergent doctrinal uptake, and regions with deep doctrinal divergence but weak press access; measure whether boundary formation tracks content or channel.',
    'If channel dominates, the doctrine-first narrative claims credit for a technological effect and its effective extraction rises; if content dominates, part of the measured extraction is the honest price of representing real incompatibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(print_vector_vs_doctrinal_content, empirical, 'Whether the fragmentation narrative''s generative claim survives separation of message from medium.').

omega_variable(
    incompatibility_modal_status,
    'Are the confessional commitments structurally incompatible (necessarily unreconcilable) or contingently unreconciled?',
    'Assess bilateral consensus statements such as the Joint Declaration on the Doctrine of Justification against confessional subscription norms: if authorized interlocutors on both sides affirm qualified convergence without violating their standards, the necessity claim fails.',
    'If compatibility is achievable, the fragmentation narrative legitimates avoidable division and its extraction rises sharply; if genuinely incompatible, much of the measured extraction is the price of the truth-claims themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incompatibility_modal_status, conceptual, 'Modal status of the incompatibility premise on which the whole reading rests.').

omega_variable(
    accuracy_vs_capture_maintenance,
    'Is the doctrine-first reading maintained because it remains the most accurate account, or because confessional institutions'' interests depend on it?',
    'Track reception of pluricausal scholarship inside confessional seminaries: whether curricula update when evidence cuts against doctrinal primacy, or absorb it as peripheral context.',
    'Interest-maintained readings support deeper extraction estimates than accuracy-maintained ones; the distinction separates coordination-serving description from legitimation service.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accuracy_vs_capture_maintenance, empirical, 'Whether maintenance of the reading tracks evidence or institutional interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1580, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1580, reformation_composite__theological_fragmentation_reading, theater_ratio, 1580, 0.15).
narrative_ontology:measurement(refo_tr_t1700, reformation_composite__theological_fragmentation_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(refo_tr_t1800, reformation_composite__theological_fragmentation_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(refo_tr_t1900, reformation_composite__theological_fragmentation_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(refo_tr_t1999, reformation_composite__theological_fragmentation_reading, theater_ratio, 1999, 0.33).
narrative_ontology:measurement(refo_tr_t2026, reformation_composite__theological_fragmentation_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(refo_be_t1580, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1580, 0.4).
narrative_ontology:measurement(refo_be_t1700, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1700, 0.48).
narrative_ontology:measurement(refo_be_t1800, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1800, 0.52).
narrative_ontology:measurement(refo_be_t1900, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1900, 0.58).
narrative_ontology:measurement(refo_be_t1999, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1999, 0.64).
narrative_ontology:measurement(refo_be_t2026, reformation_composite__theological_fragmentation_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1580, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1580, 0.75).
narrative_ontology:measurement(refo_su_t1700, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1700, 0.65).
narrative_ontology:measurement(refo_su_t1800, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1800, 0.45).
narrative_ontology:measurement(refo_su_t1900, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(refo_su_t1999, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1999, 0.42).
narrative_ontology:measurement(refo_su_t2026, reformation_composite__theological_fragmentation_reading, suppression_requirement, 2026, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, technological_mediation_reading).

% DUAL FORMULATION NOTE:
% reformation_composite decomposes into three readings because 'fundamentally theological', 'fundamentally political', and 'fundamentally technological' select different primary observables and therefore different stable epsilon values (epsilon-invariance: one label, three constraints). This story prices the doctrine-first fragmentation narrative as a standing arrangement (epsilon 0.62); the political reading prices state exploitation of religious differentiation; the technological reading prices press-mediated amplification of dissent. The theological reading is upstream in content terms — it supplies the doctrinal material the technological reading transmits and the confessional vocabulary the political reading regulated — so its legitimacy conditions shape both siblings' operating environments without resolving the primacy dispute; hence coexists_with edges rather than forecloses: integrated frameworks (confessionalization theory, print-culture history) can hold pairs of readings together, so no party's framework renders a sibling unreadable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
