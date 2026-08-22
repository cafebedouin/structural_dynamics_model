% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation Theological Fragmentation: Doctrine-Driven Denominational Incompatibility
 *   domain: religious/theological/political_economy
 *
 * SUMMARY:
 *   The Reformation as a theological fragmentation event: competing
 *   soteriological commitments (sola fide, predestination, justification
 *   mechanisms) and ecclesiological authorities (Scripture alone vs.
 *   tradition, denominational vs. papal) generate structurally incompatible
 *   denominational identities. From THIS reading, the primary observable is
 *   doctrinal pluralism as an irreducible outcome of genuine theological
 *   disagreement — not reducible to political realignment or technological
 *   mediation, but rooted in incompatible theological commitments. The
 *   theological fragmentation constraint is authored as a TANGLED ROPE: it
 *   provides genuine coordination (believers organized around coherent
 *   doctrine, denominational identity, spiritual clarity) AND asymmetric
 *   extraction (denominational leadership benefits from fragmentation,
 *   unified-Christendom advocates lose authority, believers in mixed regions
 *   lose freedom). The measurement series tracks the constraint's
 *   intensification over 200 years (roughly 1520–1720): extractiveness rises
 *   from 0.35 to a peak of 0.72 around year 150 (mid-17th century, after
 *   confessional documents harden), then slightly declines as enforcement
 *   becomes more performative than freshly coercive.
 *
 * KEY AGENTS:
 *   - reformed_denominational_leadership: institutional beneficiary of doctrinal incompatibility; enforces through confessional documents
 *   - reformed_theological_scholarship: powerful beneficiary; gains authority from doctrinal exposition
 *   - unified_christendom_defenders: institutional target; papal and episcopal authority undermined by theological alternatives
 *   - parish_clergy: moderate payers, identity-locked into denominational theology
 *   - believers_in_mixed_settlements: powerless payers, highest extraction burden
 *   - ecumenical_reformers: excluded alternative voices; structural incompatibility forecloses their reconciliation agenda
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.68).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.72).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation Theological Fragmentation: Doctrine-Driven Denominational Incompatibility").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "religious/theological/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '27f93250-1852-43e5-98c9-a4324796b455').
narrative_ontology:cs_kernel_codification('27f93250-1852-43e5-98c9-a4324796b455', fixed_text).
narrative_ontology:cs_authority_grounding('27f93250-1852-43e5-98c9-a4324796b455', lineage).
narrative_ontology:cs_interpretation_layer_present('27f93250-1852-43e5-98c9-a4324796b455').
narrative_ontology:cs_reading_relation('27f93250-1852-43e5-98c9-a4324796b455', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('27f93250-1852-43e5-98c9-a4324796b455', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('27f93250-1852-43e5-98c9-a4324796b455', foundational, soteriological_incompatibility_irreducible).
narrative_ontology:cs_axiom_status(soteriological_incompatibility_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('27f93250-1852-43e5-98c9-a4324796b455', soteriological_incompatibility_irreducible, deontological).
narrative_ontology:cs_axiom('27f93250-1852-43e5-98c9-a4324796b455', foundational, scriptural_authority_over_tradition).
narrative_ontology:cs_axiom_status(scriptural_authority_over_tradition, holdable).
narrative_ontology:cs_axiom_grounding('27f93250-1852-43e5-98c9-a4324796b455', scriptural_authority_over_tradition, theological).
narrative_ontology:cs_reference_frame('27f93250-1852-43e5-98c9-a4324796b455', unified_christendom_apostolic_authority).
narrative_ontology:cs_drift_state('27f93250-1852-43e5-98c9-a4324796b455', post_reformation_institutional_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('27f93250-1852-43e5-98c9-a4324796b455', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, reformed_theological_scholarship).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, unified_christendom_defenders).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, theological_coherence_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_document_interpreters).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, parish_clergy).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, believers_in_mixed_settlements).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, sola_scriptura_authority_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, predestinarian_soteriology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denominational hierarchies (Lutheran, Reformed, Anabaptist, etc.) establish and defend their own confessional doctrines. They enforce doctrinal boundaries through catechisms, confession documents, and clerical discipline. They benefit from fragmentation because denominational authority depends on doctrinal distinctiveness — the more incompatible the soteriological commitments, the more absolute the allegiance demanded. They administer the theological constraint by writing and enforcing confessional standards.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership, agenda_setter,
    institutional, generational, arbitrage, continental).

% Reformed theologians (Calvin, Zwingli, Luther, and their intellectual heirs) gain intellectual authority and career advancement from systematic exposition of their distinctive doctrines. Denominational patronage and theological publication networks depend on denominational coherence. They collect interpretive authority within their confessional tradition; competing interpretations consolidate around incompatible soteriological premises.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, reformed_theological_scholarship, beneficiary,
    powerful, generational, mobile, continental).

% The Roman magisterium and traditional ecclesiology advocates bear the cost of fragmentation: their claim to universal doctrinal authority is structurally undermined by each successfully defended reformed alternative. They cannot exit without renouncing their founding commitment to catholicity. They are the primary target of the constraint: the theological incompatibility is authored precisely against their integrative authority.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, unified_christendom_defenders, payer,
    institutional, civilizational, trapped, continental).

% Medieval universalist theologians and later Christian ecumenicalists who seek reconciliation across doctrinal boundaries bear a real cost: the more sharply the soteriological boundaries are drawn, the harder reconciliation becomes. Their vision of comprehensive theological integration is foreclosed by the constraint's operation. They are partly excluded from the conversation — the theological fragmentation constraint defines its space as intra-denominational coherence, not inter-denominational negotiation.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, theological_coherence_advocates, payer,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, theological_coherence_advocates, excluded).

% Local clerics must adopt and enforce their denomination's confessional doctrine or lose their benefice and social standing. They bear the cost of doctrinal identity-lock: their livelihood and professional identity depend on perfectly embodying their denomination's theology. They have no genuine exit — leaving priesthood or changing denominations means abandonment of career, community, and sometimes family alliances.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, parish_clergy, payer,
    moderate, biographical, identity_locked, local).

% Ordinary believers in religiously mixed regions (borders, trading cities, territories shifting allegiance) face sharp doctrinal mandates from both Reformed and Catholic institutions. Conformity to local authority's chosen theology is legally and socially enforced; refusal risks exile, property loss, or violence. They are the highest-extraction agents: they bear the full enforcement weight of denominational incompatibility without any benefit.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, believers_in_mixed_settlements, payer,
    powerless, biographical, trapped, local).

% The class of theologians and clerics whose entire professional identity rests on interpreting confessional documents (the Heidelberg Catechism, Formula of Concord, Westminster standards, etc.) collects interpretive authority and institutional power from doctrinal elaboration. The fragmentation is their structural base: if the denominations reunified, their specialized expertise would evaporate.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_document_interpreters, beneficiary,
    organized, generational, constrained, continental).

% Melanchthon and later Christian unity advocates are structurally excluded from the theological fragmentation constraint's operation. They would argue for doctrinal reconciliation and minimal acceptable divergence, but the constraint's logic excludes that alternative — denominations competing for authenticity cannot admit that their doctrinal differences are negotiable. Their exclusion is not accidental; it is enforced by the logic of incompatibility itself.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_reformers, excluded,
    moderate, biographical, constrained, continental).

% Emerging nation-states (Saxony, Geneva, England, France) observe that theological denominationalism serves their political independence from Rome and each other. They do not directly benefit from the theological fragmentation constraint, but they exploit it for political ends. They remain external analysts of the constraint's operation.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, secular_political_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, internally coherent soteriological and ecclesiological frameworks that allow believers to know their standing before God through a unified doctrinal lens. Provides denominational boundaries strong enough to preserve theological integrity against both Catholic and heterodox pressures. Coordinates a community of believers around shared interpretive commitments to Scripture and doctrine.
% TRANSFER_FUNCTION: Transfers spiritual authority from Rome (and medieval universal Christendom) to Reformed denominational leadership. Transfers interpretive power over salvation doctrine from papal magisterium to confessional documents and their authorized interpreters. Transfers allegiance and obedience from the universal Church to the local denominational authority.
% ABSENT_VOICES: Ecumenical reconcilers (Melanchthon, later Protestant and Catholic unity advocates) are structurally excluded — they would argue doctrinal differences are reconcilable, but the theological fragmentation constraint's logic forecloses that claim. Medieval Thomistic universalists cannot speak within this constraint's framework without conceding the incompatibility it asserts. Believers in mixed confessional territories who want to remain neutral are excluded by necessity — the constraint enforces denominational choice.
% DISAPPEARANCE_RATIONALE: If the theological fragmentation constraint vanished — if the soteriological doctrines suddenly became compatible and denominations recognized each other's salvific validity — Christian Europe would reorganize within decades. Denominational hierarchies would lose their enforcing function. The confessional documents would be reinterpreted as regulative guidelines rather than absolutizing authorities. Parish clergy identity would decouple from doctrinal purity. The unified Christendom model would become viable again, and ecclesiastical authority would consolidate around different institutional structures.
% FOUNDING_PROBLEM: Medieval Catholicism provided a unified theological and ecclesiastical framework, but that framework was understood to rest on papal and episcopal authority that Reformed theologians deemed scriptually indefensible. The founding problem was the contradiction between claimed universal spiritual authority and Reformed reading of Scripture as the sole ultimate authority. Luther, Calvin, and others saw a genuine theological problem: how to maintain coherent Christian theology when the institutional authority claiming to guard it was, in their view, doctrinally corrupted.
% FOUNDING_PROBLEM_CORROBORATION: Reformed denominational leadership attests the founding problem is live: they continue to teach that salvation doctrine differs fundamentally between Reformed and Catholic traditions, and papal authority remains illegitimate. Catholic authorities attest the founding problem is wrongly framed — papal authority rests on apostolic succession and living tradition, not solely on medieval institutional claims. Independent historical and theological analysis (from religious scholars not embedded in any confessional tradition) confirms the dispute is genuine and ongoing: the soteriological and ecclesiological differences are real and structurally incompatible within a single framework, even if accommodation is theoretically possible.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts low (0.35) because early Reformation is still articulated as restoration and reform within a universal framework — the incompatibility is not yet fully institutionalized. By year 50 it rises to 0.58 as confessional documents (Augsburg Confession 1530, later the Heidelberg Catechism, Formula of Concord) harden denominational boundaries. Peak extraction (0.72) is reached around year 100–150 when the Counter-Reformation's Trent Council (1545–1563) responds with hardened Catholic doctrine, and the various Reformed denominations have full institutional structures. The slight decline thereafter (to 0.68 at year 200) reflects the transition to early-modern pluralism — by the 18th century, philosophical and political arguments begin to compete with purely theological ones, and the constraint's extractive function is partially diluted by secularization, not resolved by the constraint itself. Theater ratio rises from 0.18 to 0.41 because later institutional denominationalism includes increasing performative elements: the constraint becomes theater when denominations defend doctrinal boundaries routinely rather than desperately. Suppression requirement parallels extractiveness because the constraint's persistence depends on active enforcement — excommunication, property loss, expulsion — not on voluntary allegiance. The measurement grid is shared: every metric is authored at every time point (0, 25, 50, 100, 150, 200) on a single interval.
 *
 * PERSPECTIVAL GAP:
 *   The denominational leadership seat and the theological scholarship seat should compute this constraint very differently from the parish clergy and believers' seats. From the agenda-setter perspective, the theological fragmentation is COORDINATION — coherent doctrine, clear boundaries, spiritual direction. From the parish clergy and believer seats, it is EXTRACTION — doctrinal identity-lock, enforced conformity, loss of freedom to interpret or reconcile. The engine computes this divergence from power differentials and exit options: agenda-setters have high power and arbitrage exits (they can change doctrine if needed); believers have low power and trapped or identity-locked exits. The claim/metric independence rule applies: this reading is CLAIMED as tangled_rope (the theological fragmentation story really does provide coordination AND extract), and the metrics are authored to describe that structure honestly — not tuned to match any predicted engine output.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership sits at d ≈ 0.1–0.2 (beneficiary end): they set doctrine, collect authority, have options to change the framework. Reformed theological scholarship sits at d ≈ 0.15–0.25 (still beneficiary-leaning): powerful position, mobile exits, patron support. Unified-Christendom defenders sit at d ≈ 0.8–0.9 (target end): their authority is structurally undermined, they cannot exit without renouncing their founding commitment. Parish clergy sit at d ≈ 0.7–0.8 (target end): they are identity-locked and trapped — their entire social position depends on denominational coherence. Believers in mixed regions sit at d ≈ 0.9 (nearly pure target): no benefit, highest extraction, trapped exit. The directionality derivation from beneficiary/victim + power + exit produces these values naturally — no overrides needed because the structural relationships are unambiguous in this historical case.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (papal/episcopal authority conflicts with Reformed reading of Scripture) is LIVE and CONTESTED, not resolved. The constraint persists not because the problem is solved but because two incompatible institutional responses to it have consolidated into separate denominations. This is NOT a scaffold (no sunset clause declared — the denominational fragmentation did not aim for a transition state). This is NOT a piton (the theological agenda is still genuinely pursued, not performatively maintained). It IS a tangled rope: real coordination within each denomination, real extraction from those defending universalism or seeking reconciliation. The mandatrophy test asks: does the constraint prevent mislabeling coordination as extraction? YES — the theological fragmentation genuinely coordinates believers around doctrine AND genuinely extracts from those who lose universalist authority. Recognizing both prevents the error of calling it pure coordination (false) and pure extraction (false). The constraint's proper classification as tangled_rope prevents mischaracterization in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_primacy_vs_epiphenomenalism,
    'Are the soteriological and ecclesiological incompatibilities genuinely generative of denominational fragmentation, or do they rationalize and institutionalize fragmentation driven by political and technological forces?',
    'Comparative historical analysis: examine regions where theology was similar but political incentives diverged (do denominations still fragment?), and regions where political pressures aligned but theology diverged (does fragmentation still occur?). A true test requires cases where one factor is held constant.',
    'If theology is epiphenomenal, this reading should be reclassified as a rationalization superstructure atop a political or technological constraint — the extraction would then flow to political elites or technological mediators, not theological leadership. If theology is primary, this reading''s beneficiary and victim structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_primacy_vs_epiphenomenalism, conceptual, 'Whether doctrinal incompatibilities are primary drivers or derivative rationalizations of Reformation fragmentation.').

omega_variable(
    confessional_document_artifactuality,
    'Do the confessional documents (Augsburg Confession, Heidelberg Catechism, Formula of Concord, etc.) GENERATE denominational incompatibility by fixing doctrine, or do they CODIFY pre-existing incompatibilities in theological communities?',
    'Textual and community analysis: track the theological distinctiveness of communities before and after confessional document adoption. Did communities become MORE incompatible after codification, or did they merely formalize existing divergences?',
    'If confessional documents generate incompatibility, the constraint is a product of deliberate institutional choice — more clearly extractive. If they codify pre-existing divergences, the constraint reflects genuine theological pluralism — more defensible as coordination. The measurement series'' rise in theater_ratio after year 50 (confessional hardening) suggests documents play a generative role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confessional_document_artifactuality, empirical, 'Whether confessional documents are constraining artifacts or expressions of prior theological divergence.').

omega_variable(
    unified_christendom_vs_reformed_plurality,
    'Is the pre-Reformation unified Christendom model genuinely achievable as a coordinating framework, or is the theological pluralism discovered by the Reformation irreversible?',
    'Philosophical and theological reconstruction: assess whether the soteriological commitments claimed by Reformed and Catholic traditions can coexist within a single theological framework without one renouncing core commitments. Later ecumenical efforts provide empirical touchstones.',
    'If plurality is irreversible, the constraint represents the permanent structure of Christian theology, and extraction flows from denominational boundaries being REAL, not artificial. If unity is theoretically possible, the constraint is more clearly extractive — denominations are artificially maintaining separation. This omega anchors the distinction between genuine theological incompatibility and institutional rent-seeking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unified_christendom_vs_reformed_plurality, conceptual, 'Whether theological pluralism is an irreversible discovery or a contingent institutional choice.').

omega_variable(
    parish_clergy_identity_lock_internalization,
    'Is the parish clergy''s doctrinal identity-lock structural (external enforced conformity that would persist only under enforcement) or internalized (clergy have genuinely adopted the theology as core identity)?',
    'Post-enforcement historical analysis: cases where enforcement disappeared (due to secular state policy, ecumenical mandates, or pluralization) — did clergy retain doctrinal identity independently, or did identity dissolve? Personal testimony and cross-confessional movement rates provide evidence.',
    'If primarily structural, the constraint''s suppression of clergy is contingent on active enforcement — the extraction would decline if enforcement costs rose. If internalized, clergy carry the constraint with them even after enforcement ends — the extraction is deeper and more durable. This distinguishes between institutional coercion and identity-fused allegiance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parish_clergy_identity_lock_internalization, empirical, 'Whether parish clergy''s denominational identity is structurally enforced or internalized commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__theological_fragmentation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(refo_tr_t0, observed).
narrative_ontology:measurement(refo_tr_t25, reformation_composite__theological_fragmentation_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement_basis(refo_tr_t25, observed).
narrative_ontology:measurement(refo_tr_t50, reformation_composite__theological_fragmentation_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(refo_tr_t50, observed).
narrative_ontology:measurement(refo_tr_t100, reformation_composite__theological_fragmentation_reading, theater_ratio, 100, 0.41).
narrative_ontology:measurement_basis(refo_tr_t100, observed).
narrative_ontology:measurement(refo_tr_t150, reformation_composite__theological_fragmentation_reading, theater_ratio, 150, 0.43).
narrative_ontology:measurement_basis(refo_tr_t150, observed).
narrative_ontology:measurement(refo_tr_t200, reformation_composite__theological_fragmentation_reading, theater_ratio, 200, 0.41).
narrative_ontology:measurement_basis(refo_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__theological_fragmentation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(refo_be_t0, observed).
narrative_ontology:measurement(refo_be_t25, reformation_composite__theological_fragmentation_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(refo_be_t25, observed).
narrative_ontology:measurement(refo_be_t50, reformation_composite__theological_fragmentation_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(refo_be_t50, observed).
narrative_ontology:measurement(refo_be_t100, reformation_composite__theological_fragmentation_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement_basis(refo_be_t100, observed).
narrative_ontology:measurement(refo_be_t150, reformation_composite__theological_fragmentation_reading, base_extractiveness, 150, 0.72).
narrative_ontology:measurement_basis(refo_be_t150, observed).
narrative_ontology:measurement(refo_be_t200, reformation_composite__theological_fragmentation_reading, base_extractiveness, 200, 0.68).
narrative_ontology:measurement_basis(refo_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__theological_fragmentation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(refo_su_t0, observed).
narrative_ontology:measurement(refo_su_t25, reformation_composite__theological_fragmentation_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(refo_su_t25, observed).
narrative_ontology:measurement(refo_su_t50, reformation_composite__theological_fragmentation_reading, suppression_requirement, 50, 0.63).
narrative_ontology:measurement_basis(refo_su_t50, observed).
narrative_ontology:measurement(refo_su_t100, reformation_composite__theological_fragmentation_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement_basis(refo_su_t100, observed).
narrative_ontology:measurement(refo_su_t150, reformation_composite__theological_fragmentation_reading, suppression_requirement, 150, 0.75).
narrative_ontology:measurement_basis(refo_su_t150, observed).
narrative_ontology:measurement(refo_su_t200, reformation_composite__theological_fragmentation_reading, suppression_requirement, 200, 0.72).
narrative_ontology:measurement_basis(refo_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_composite__theological_fragmentation_reading, 0.12).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% The reformation_composite kernel is decomposed into three structurally distinct constraint stories, each with a different primary observable and ε value. This story (theological_fragmentation_reading) assigns doctrinal incompatibility as primary and ε=0.68; the sibling political_realignment_reading assigns state sovereignty as primary with distinct ε; the sibling technological_mediation_reading assigns mass communication capacity as primary. Each reading is one valid constraint over the same historical event — the decomposition prevents false aggregation. Links carry semantic weight: theological_fragmentation INFLUENCES political_realignment (denominational identity gives nation-states an axis for sovereignty claims) and INFLUENCES technological_mediation (theology demands reproduction in printed form to reach mass audience). The siblings do not FORECLOSE this reading — all three remain live positions in historical scholarship, held by different scholars' framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
