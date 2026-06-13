% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II Doctrinal Continuity Hermeneutic
 *   domain: ecclesiastical/institutional
 *
 * SUMMARY:
 *   Vatican II (1962–1965) convoked by John XXIII and executed by Paul VI
 *   produced sixteen documents that contemporaries and historians recognize
 *   as one of Christianity's most significant institutional
 *   transformations—on liturgy, religious freedom, ecumenism, church-world
 *   relations, and ecclesiology. The continuity reading claims these changes
 *   represent organic doctrinal development, not rupture: new formulations
 *   explicate prior implicit teaching; pastoral adaptations do not alter
 *   doctrine; ambiguities in the documents are prudential not doctrinal. This
 *   reading is institutionalized in official magisterial statements, papal
 *   encyclicals (especially John Paul II's and Benedict XVI's hermeneutics),
 *   and Vatican offices' interpretation authority. The constraint CLAIMS to
 *   be tangled_rope (genuine coordination function + asymmetric extraction),
 *   which is the reading's own self-description: it coordinates the Church's
 *   adaptation while preserving authority legitimacy. The authored metrics,
 *   independent of the claim, describe a constraint whose theater ratio rises
 *   over 60 years to 0.58 (performative maintenance increasingly
 *   predominant), whose extraction rises then plateaus at 0.38 (moderate but
 *   stable), and whose suppression rises then stabilizes at 0.42 (active
 *   enforcement to maintain the hermeneutic boundary). The claim and metrics
 *   diverge: a genuine coordination function should show low theater and low
 *   resistance; this constraint shows significant both. The constraint is ONE
 *   READING of the contested kernel vatican_ii_doctrinal_authority. The
 *   continuity reading is instantiated here as a clean constraint. Two
 *   sibling readings (rupture_progressive_reading,
 *   rupture_traditionalist_reading) plus a
 *   composite_overdetermination_reading represent competing instantiations of
 *   the same kernel.
 *
 * KEY AGENTS:
 *   - magisterial_continuity_authority: the Vatican's teaching office, which sets and enforces the continuity hermeneutic via papal documents and episcopal appointments
 *   - traditionalist_reform_advocates: organized Catholic communities arguing Vatican II contradicts pre-conciliar doctrine, disciplined as schismatic, structurally excluded from interpretation-setting
 *   - progressive_reform_interpreters: theologians and bishops using Vatican II documents as authorization for substantial reform, constrained by needing to frame innovation as continuity
 *   - parish_clergy_implementation: local priests performing the continuity narrative while implementing substantial liturgical and pastoral change
 *   - lay_catholic_believers: powerless beneficiaries of pastoral flexibility, payers of cognitive dissonance
 *   - historical_scholarship_community: excluded from official interpretation-setting despite documenting Vatican II's discontinuities
 *   - vatican_ii_documents_text: non-agent kernel entity carrying deliberate ambiguities enabling both continuity and rupture readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.38).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.42).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II Doctrinal Continuity Hermeneutic").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiastical/institutional").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'aa358768-7152-42bf-8e9f-ea7644931157').
narrative_ontology:cs_kernel_codification('aa358768-7152-42bf-8e9f-ea7644931157', fixed_text).
narrative_ontology:cs_authority_grounding('aa358768-7152-42bf-8e9f-ea7644931157', extraction).
narrative_ontology:cs_interpretation_layer_present('aa358768-7152-42bf-8e9f-ea7644931157').
narrative_ontology:cs_reading_relation('aa358768-7152-42bf-8e9f-ea7644931157', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa358768-7152-42bf-8e9f-ea7644931157', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa358768-7152-42bf-8e9f-ea7644931157', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('aa358768-7152-42bf-8e9f-ea7644931157', foundational, doctrinal_continuity_through_development).
narrative_ontology:cs_axiom_status(doctrinal_continuity_through_development, holdable).
narrative_ontology:cs_axiom_grounding('aa358768-7152-42bf-8e9f-ea7644931157', doctrinal_continuity_through_development, deontological).
narrative_ontology:cs_axiom('aa358768-7152-42bf-8e9f-ea7644931157', foundational, magisterial_interpretive_authority_preservation).
narrative_ontology:cs_axiom_status(magisterial_interpretive_authority_preservation, holdable).
narrative_ontology:cs_axiom_grounding('aa358768-7152-42bf-8e9f-ea7644931157', magisterial_interpretive_authority_preservation, conventional).
narrative_ontology:cs_reference_frame('aa358768-7152-42bf-8e9f-ea7644931157', pre_conciliar_doctrinal_stability).
narrative_ontology:cs_drift_state('aa358768-7152-42bf-8e9f-ea7644931157', post_conciliar_implementation_era_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa358768-7152-42bf-8e9f-ea7644931157', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterial_continuity_authority).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, curial_institutional_stability).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_reform_advocates).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_interpreters).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, lay_catholic_believers).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, ecumenical_dialogue_partners).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, conciliar_observers_progressive_episcopate).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, parish_clergy_implementation).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, lay_catholic_believers).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, doctrinal_development_thomist).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, hermeneutic_continuity_principle).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, magisterial_authority_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Vatican's teaching office (Pope, Curia, episcopal colleges) establishes and defends the continuity hermeneutic as the authoritative reading of Vatican II. This authority rests on claiming that Vatican II documents, correctly understood, represent organic doctrinal development, not rupture. The authority administers which interpretations are orthodox and enforces doctrinal boundaries through papal encyclicals, Vatican documents, and episcopal discipline. Failure to maintain continuity framing would undermine the magisterium's claim to unbroken authority descent.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterial_continuity_authority, agenda_setter,
    institutional, civilizational, trapped, global).

% Traditionalist Catholic communities and theologians who argue that Vatican II introduced doctrinal ruptures (especially on religious freedom, ecumenism, liturgical reform) incompatible with pre-conciliar teaching. They pay the cost of being disciplined as schismatic, having their seminaries and communities denied institutional recognition, and carrying the burden of proving their fidelity to pre-conciliar doctrine while the magisterium claims continuity. They are excluded from official interpretation-setting because the continuity reading, once institutionalized, forecloses traditionalist rupture claims as heretical. Their exit is identity-locked: leaving Catholicism abandons the spiritual tradition they defend.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_reform_advocates, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_reform_advocates, excluded).

% Progressive theologians, pastoral practitioners, and Vatican II proponents who interpret the Council as authorizing substantial doctrinal and liturgical change ('the spirit of Vatican II'). They pay the cost of hermeneutic constraint: the magisterium insists their interpretations must be read as continuity, not rupture, which delegitimizes appeals to radical innovation or formal doctrinal revision. They benefit partially because the continuity frame allows them to claim Council authorization while not requiring explicit magisterial approval for each innovation—the frame creates space for implementation flexibility. But they are constrained by needing to present their reforms as 'organic developments' rather than as principled breaks.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_interpreters, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_interpreters, beneficiary).

% Local parish priests, religious educators, and pastoral workers navigate the ambiguities of Vatican II by implementing liturgical and doctrinal changes while the magisterium simultaneously claims nothing has changed doctrinally. They bear the cost of explaining to lay Catholics why the Latin Mass is replaced by vernacular while doctrine is 'unchanged,' why ecumenical dialogue proceeds while Catholic distinctiveness is affirmed, why celibacy is questioned while celibacy is reaffirmed. Their constraint is occupational—leaving the priesthood means abandoning their vocation, but staying requires performing the continuity narrative whether they believe it or not.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, parish_clergy_implementation, payer,
    moderate, biographical, constrained, regional).

% Lay Catholics experience the Council's changes (new liturgy, new ecumenical posture, new teaching on religious freedom and conscience) as either liberating adaptations or confusing ruptures, depending on their formation. They benefit from pastoral flexibility and updated teachings on contemporary issues (religious liberty, social justice). They pay by bearing the cognitive dissonance of being told the Church is unchanged while everything they experience—liturgy, catechesis, priest behavior—appears radically different. Their exit is constrained: switching denominations is identity-costly, staying requires accepting the continuity claim on authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, lay_catholic_believers, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, lay_catholic_believers, payer).

% The 16 conciliar documents (constitutions, decrees, declarations) are the kernel this constraint reads. The documents themselves carry deliberate ambiguities—some passages state traditional doctrine, others open interpretive space for change, creating the hermeneutic possibility that both continuity and rupture readings can cite the same text. The documents are not an agent but a non-agent entity kept for completeness: they are the textual substrate the constraint reads, and the constraint's persistence depends on maintaining their ambiguity as a feature rather than resolving it as a defect.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_documents_text, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_documents_text).

% Academic historians, theologians in non-Catholic universities, and conciliar historians (like John O'Malley) who have documented extensive evidence of Vatican II's discontinuities with pre-conciliar teaching—on authority structures, liturgical theology, church-world relations—are structurally excluded from magisterial interpretation-setting. Their scholarly findings (that the Council WAS a significant shift, even if justified) contradict the continuity narrative, but they lack institutional authority to make their reading official. They would argue for explicit acknowledgment of rupture-with-development rather than continuity-framing, but the magisterium does not need to engage them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, historical_scholarship_community, excluded,
    powerful, generational, analytical, global).

% Non-Catholic Christian traditions (Orthodox, Lutheran, Reformed, Anglican) benefited from Vatican II's ecumenical opening and new teaching on religious freedom and separated brethren. They are beneficiaries of the Council's apparent doctrinal shift toward them. However, they remain partly frustrated by the magisterium's insistence that this represents 'continuity' rather than doctrinal development—the framing prevents formal recognition of how much Catholic teaching changed regarding their status and ecclesial legitimacy. Their exit is mobile: they can engage ecumenically or withdraw, but the continuity frame constrains how deeply Rome will engage.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, ecumenical_dialogue_partners, beneficiary,
    powerful, generational, mobile, global).

% The Vatican Curia—the administrative bureaucracy of the Catholic Church—has institutional incentive to maintain that Vatican II represents continuity, because acknowledging rupture would raise questions about the Curia's responsibility for implementing doctrinal change and would expose Curia agencies (some reformed, some resistant) as either incompetent (failing to implement the Council) or heterodox (implementing beyond the Council). Maintaining continuity framing allows the Curia to avoid accountability for either outcome. The Curia administers enforcement through doctrinal oversight bodies (Congregation for the Doctrine of Faith) and episcopal appointments. It is trapped because abandoning the continuity frame would destabilize the institutional authority structure it maintains.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, curial_institutional_stability, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, curial_institutional_stability, beneficiary).

% Progressive bishops (Rhine Group, Belgian bishops, eventually many from Latin America, Africa) who authored and pushed Council documents in more innovative directions benefit from the continuity frame because it allows them to claim they did not violate orthodoxy while advancing substantial reforms. The frame gives them cover: they can present their episcopal leadership as faithful application of continuity rather than as doctrinal innovation. They are constrained because they cannot formally declare the change they effected; their power depends on the continuity narrative holding.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, conciliar_observers_progressive_episcopate, beneficiary,
    powerful, generational, constrained, global).

% Institutional historians, theologians, and sociologists outside the Church who study Vatican II and the constraint it operates under. They observe the structural dynamics: how continuity framing simultaneously enables pastoral change and forecloses explicit doctrinal rupture acknowledgment; how the constraint's persistence depends on maintaining ambiguity in the documents and suppressing the scholarly consensus that significant change occurred; how the constraint extracts from traditionalists and progressive reformers through hermeneutic closure.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, analytical_observer_framework, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, magisterial_continuity_authority).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global Catholic Church's adaptation to post-war modernity (religious freedom, ecumenical engagement, liturgical accessibility, social engagement) while preserving the magisterium's claim to unbroken doctrinal authority. Without the continuity frame, either the magisterium appears to have made errors (contradicting itself), or the pre-conciliar magisterium appears superseded (undermining papal authority claim). The coordination problem solved: how to change substantially while claiming nothing essential changed.
% TRANSFER_FUNCTION: Moves interpretive authority from the conciliar documents themselves (which are ambiguous and invite competing readings) to the magisterium's official reading (continuity = organic development = no rupture). This transfer constrains both traditionalists (who cannot claim the Council contradicted tradition) and progressives (who cannot claim the Council authorized formal doctrinal revision). The magisterium collects the benefit: unquestioned interpretive monopoly and immunity from accountability for implementing doctrinal change it officially denies making.
% ABSENT_VOICES: Historical scholarship community documenting Vatican II's discontinuities; traditionalist reform advocates demanding explicit acknowledgment of rupture; progressive reformers who would argue for formal doctrinal revision rather than 'development'; conciliar historians who attended and recorded the Council's own awareness of breaking with pre-conciliar positions. These voices would object that the continuity narrative contradicts documentary evidence (conciliar debates, papal speeches during the Council acknowledging novelty, explicit breaks on religious freedom, liturgical theology, collegiality). They are excluded because the magisterium controls the official hermeneutic frame.
% DISAPPEARANCE_RATIONALE: If the continuity constraint disappeared—if the magisterium explicitly acknowledged that Vatican II represented significant doctrinal development on religious freedom, ecclesiology, liturgy, and church-world relations—the world would rearrange substantially: traditionalist communities would either reintegrate around acknowledged rupture, progressive reformers would no longer need hermeneutic cover for their innovations and could pursue explicit doctrinal revision, scholarly consensus would replace institutional narrative as the authoritative account, and the magisterium would face accountability for implementing change it denies making. The constraint's removal would force institutional reckoning and jurisdictional realignment.
% FOUNDING_PROBLEM: Vatican II was convoked to update the Church for the modern world ('aggiornamento'—updating). The Council produced documents mandating liturgical reform, ecumenical engagement, religious freedom teaching, and missionary adaptation. The founding problem: how to present these substantial changes as faithful development of unchanging doctrine rather than as doctrinal rupture that would undermine the magisterium's authority claim. The problem is institutional legitimacy under conditions of radical change.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium attests the founding problem as still live: popes from John Paul II through Francis assert Vatican II represents continuity with tradition and that 'rupture' interpretations (whether progressive or traditionalist) misread the Council. Traditionalist bishops (SSPX) attest the problem persists because continuity claim is false. Progressive theologians (Küng, Schillebeeckx before censure) attest the founding problem is solved through rupture acknowledgment, not continuity maintenance. John O'Malley's historical scholarship (What Happened at Vatican II, drawing on conciliar documents and debates) documents that the Council itself was aware of significant breaks, undermining the continuity narrative from outside the benefiting parties' testimony.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1962: Council convoked as update, low extraction claim) to 0.28 (1975: post-conciliar conflict over implementation; magisterium asserts continuity interpretation against competing readings) to 0.38–0.42 (1985–2024: plateau as interpretation hardens and scholarship consensus develops against continuity narrative; extraction becomes maintaining the hermeneutic boundary against scholarly evidence). Theater ratio rises from 0.20 to 0.58, indicating the constraint increasingly operates through performative assertion of continuity rather than through genuine doctrinal stability—the gap between what the magisterium claims (no doctrinal change) and what scholars document (substantial change) widens, requiring more theatrical enforcement of the continuity reading. Suppression rises from 0.18 to 0.42, reflecting increasing active enforcement: traditionalist communities face institutional discipline; progressive reformers face hermeneutic constraints; historical scholars are excluded from official interpretation-setting; parish clergy perform continuity while implementing change. The shared time grid enables measurement alignment: every metric is authored at every time point so temporal analysis can track covariation. The measurement series documents the constraint's lifecycle: initiation (low extraction, minimal theater), normalization (rising extraction, rising theater as competing readings emerge), and plateau (extraction and theater stabilize as the constraint hardens but scholarly evidence accumulates). This is NOT a mountain: accessibility_collapse at 0.72 means alternatives (rupture readings) have NOT collapsed; resistance at 0.65 is substantial. The constraint must be actively maintained.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterial continuity authority's seat: Vatican II represents faithful organic development; the continuity hermeneutic protects doctrinal stability; opposition comes from those who misread the Council or refuse doctrinal development. From the traditionalist seat: Vatican II introduced doctrinal ruptures the magisterium denies; the continuity claim is false institutional cover; the constraint extracts from them by declaring their correct reading heretical. From the progressive seat: Vatican II authorizes substantial change the continuity frame constrains; the frame prevents explicit doctrinal revision they believe justified; the constraint extracts by forcing them to perform continuity while pursuing rupture. From the scholar's seat: Vatican II represents documented historical change; the continuity narrative contradicts evidence; the constraint extracts by excluding scholarship from official interpretation-setting. The engine computes each seat's classification from power + exit + beneficiary/victim data; these perspectives should map to different directionality values and potentially different types at different seats. The magisterial seat (institutional power, trapped exit, beneficiary) should compute low extraction; traditionalist and progressive seats (constrained exit, victim status) should compute higher extraction; the scholarly seat (analytical, excluded) should show the structure but lack power to affect the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Magisterial continuity authority: d approaches 0.0–0.15 (full beneficiary). Institutional power, trapped exit (cannot abandon magisterial authority function), beneficiary (collects interpretive monopoly and authority legitimacy). The constraint directly benefits this seat by conferring immunity from accountability for doctrinal change. Traditionalist reform advocates: d approaches 0.75–0.85 (full target). Organized but disciplined, identity-locked (cannot leave Catholicism without abandoning the tradition they defend), victims (disciplined as schismatic, excluded from interpretation-setting). The constraint extracts by forcing them to accept the continuity reading as orthodox or accept institutional isolation. Progressive reform interpreters: d approaches 0.55–0.65 (near target). Powerful institutional position (bishops, theologians), constrained exit (cannot leave the institutional Church without surrendering their role), mixed status (beneficiaries of flexible implementation space, victims of hermeneutic closure). The constraint extracts by preventing explicit doctrinal revision while allowing implementation flexibility—a constrained benefit. Historical scholarship community: d approaches 0.70–0.75 (strong target). Powerful intellectually but excluded institutionally, analytical exit (can publish scholarship but cannot affect magisterial teaching), victim of exclusion from interpretation-setting. The constraint extracts from them by rendering their documented evidence irrelevant to official hermeneutics. Lay Catholic believers: d approaches 0.50–0.55 (symmetric). Powerless but not trapped; constrained exit (switching denominations is identity-costly but possible); diffuse beneficiary-payer status (benefit from pastoral flexibility, pay cognitive dissonance). The constraint is roughly symmetric for this seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to present substantial change as continuity to maintain authority legitimacy) was live at Vatican II's conclusion (1965). By 2024 the problem is DEAD in scholarly consensus but CONTESTED in official magisterial position. Historical evidence (conciliar documents, papal speeches, Council participants' accounts, theological scholarship) establishes that Vatican II represented significant doctrinal development on religious freedom, ecclesiology, and church-world relations. The founding problem is solved: the Church DID change, and that change can be understood as development rather than rupture. However, the magisterium continues to insist on the continuity reading despite the founding problem being resolved. This is a mandatrophy case: the constraint persists after its justification has atrophied. The constraint now operates as institutional inertia—maintaining interpretive control and authority legitimacy is the reason, not the founding problem's unresolved status. The theater ratio rising to 0.58 indicates performative maintenance: the continuity claim is defended through assertion and discipline, not through persuasion of evidence. The constraint prevents explicit acknowledgment that the founding problem is solved, which would require the magisterium to either accept its own role in doctrinal change or concede that pre-conciliar magisterium was in error. Instead, the constraint maintains ambiguity: 'continuity' means both 'nothing essential changed' and 'organic development changed everything'—a linguistic solution that forecloses resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_documentary_evidence,
    'Do the Vatican II documents themselves contain explicit acknowledgment of doctrinal novelty and rupture with pre-conciliar teaching, or do they present changes as purely organic development?',
    'Textual analysis of the 16 conciliar documents and debates; papal speeches during the Council; conciliar observers'' and theologians'' contemporary accounts; comparing conciliar texts to pre-conciliar magisterial statements on religious freedom, ecclesiology, and liturgy.',
    'If the documents acknowledge rupture, the continuity reading is exegetically indefensible and the constraint operates as pure institutional denial. If the documents genuinely present changes as development with no rupture language, the continuity reading is more plausible and the extraction measure may be lower than authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_rupture_documentary_evidence, empirical, 'Whether documentary evidence supports or contradicts the continuity narrative').

omega_variable(
    authority_structure_accountability_coupling,
    'Is the continuity reading structurally dependent on the magisterium''s immunity from accountability for doctrinal change? Would acknowledging rupture necessarily delegitimize the pre-conciliar magisterium?',
    'Theological analysis of how other Christian traditions (Orthodox, Protestant) handle magisterial change without continuity framing; examination of whether ''faithful development'' and ''principled rupture with error'' can coexist in a legitimacy claim; historical comparison to other institutions managing substantial change.',
    'If immunity-from-accountability is structurally coupled to continuity framing, the constraint is fundamentally about authority preservation, not doctrinal truth. The extraction measure would be justified. If rupture can be acknowledged while preserving magisterial authority through a development-with-error narrative, the constraint''s necessity is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_structure_accountability_coupling, conceptual, 'Whether continuity framing is structurally necessary for magisterial authority or whether rupture-as-development could serve the same legitimacy function').

omega_variable(
    post_conciliar_excess_implementation_dynamics,
    'Are liturgical and pastoral changes post-Vatican II best explained as exuberant implementation beyond conciliar intent (the continuity reading''s interpretation), or as natural development of the Council''s actual teachings?',
    'Analysis of what conciliar documents explicitly authorized vs. what was implemented; examination of whether post-conciliar phenomena (collapse of vocations, rapid liturgical change, catechetical disruption) were anticipated or explicitly rejected by the documents; comparison of conciliar intent to post-conciliar reality.',
    'If implementation significantly exceeded or contradicted conciliar intent, the continuity reading is strengthened—excesses can be blamed on bad execution, not conciliar error. If the documents themselves authorized the changes, the continuity reading collapses and must be replaced by a ''purposeful rupture'' or ''development-with-unintended-consequences'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_conciliar_excess_implementation_dynamics, empirical, 'Whether post-conciliar changes exceed or fulfill conciliar authorization').

omega_variable(
    identity_lock_magnitude_traditionalist_exit,
    'Is traditionalist Catholic identity-lock to the faith so complete that their exit remains unavailable even under institutional suppression, or is suppression eroding the identity-lock and creating new exit pathways?',
    'Longitudinal data on traditionalist community trajectories: seminary enrollment, ordinations, community stability, apostasy rates, defection to SSPX or competing jurisdictions, theological publications; comparison of identity-lock strength pre-Vatican II, at implementation (1975), and contemporary.',
    'If identity-lock remains high despite suppression, the victim status is real and extraction is sustainable. If suppression is eroding identity-lock and creating exit options, the constraint''s effectiveness is declining and the extracted amount is lower than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_magnitude_traditionalist_exit, empirical, 'Magnitude of identity-lock constraining traditionalist exit under institutional suppression').

omega_variable(
    reading_contest_temporal_dynamics,
    'Is the contest between continuity, rupture_progressive, and rupture_traditionalist readings converging toward consensus, diverging toward institutionalized schism, or oscillating between periods of suppression and revival?',
    'Tracking papal teaching evolution (John Paul II, Benedict XVI, Francis) on Vatican II interpretation; monitoring traditionalist organizational strength and institutional recognition; measuring progressive reform momentum and constraints; documenting scholarly consensus drift.',
    'If converging to consensus, one reading will consolidate and the constraint will either harden (win) or collapse (lose). If diverging, the constraint may face increasing delegitimacy pressure. If oscillating, the theater ratio may rise further as enforcement alternates with conciliation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_temporal_dynamics, empirical, 'Temporal trajectory of the reading contest and constraint sustainability').

omega_variable(
    institutional_incentive_alignment_reformation,
    'Do institutional incentives that currently support the continuity reading (magisterial immunity, Curial authority, episcopal appointment power) remain stable or are they subject to reformation?',
    'Analysis of papal authority structures, Curia reform trajectories, competing episcopal jurisdictions (SSPX, Eastern Orthodox, schismatic groups), synodality movements potentially distributing interpretive authority; examination of whether decentralizing authority would make continuity framing sustainable or would expose its institutional instrumentality.',
    'If institutional incentives remain aligned with continuity reading, the constraint persists. If reform redistributes interpretive authority, the continuity reading loses its enforcement mechanism and may be replaced by explicit rupture acknowledgment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_incentive_alignment_reformation, conceptual, 'Stability of institutional incentive alignment sustaining the continuity reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement_basis(vati_tr_t1962, observed).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement_basis(vati_tr_t1975, observed).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1985, 0.48).
narrative_ontology:measurement_basis(vati_tr_t1985, observed).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2000, 0.58).
narrative_ontology:measurement_basis(vati_tr_t2000, observed).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2010, 0.6).
narrative_ontology:measurement_basis(vati_tr_t2010, observed).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2024, 0.58).
narrative_ontology:measurement_basis(vati_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement_basis(vati_be_t1962, observed).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement_basis(vati_be_t1975, observed).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement_basis(vati_be_t1985, observed).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(vati_be_t2000, observed).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2010, 0.39).
narrative_ontology:measurement_basis(vati_be_t2010, observed).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(vati_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1962, 0.18).
narrative_ontology:measurement_basis(vati_su_t1962, observed).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1975, 0.28).
narrative_ontology:measurement_basis(vati_su_t1975, observed).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1985, 0.36).
narrative_ontology:measurement_basis(vati_su_t1985, observed).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement_basis(vati_su_t2000, observed).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement_basis(vati_su_t2010, observed).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(vati_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__continuity_reading, 0.25).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, magisterial_immunity_doctrinal_change_accountability).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, papal_infallibility_post_conciliar_reinterpretation).

% DUAL FORMULATION NOTE:
% The vatican_ii_doctrinal_authority kernel generates four distinct constraint stories, each instantiating a different reading with different ε values, beneficiary/victim structures, and types. The continuity_reading (this constraint) represents the institutionalized reading and shows moderate extraction (0.38), moderate suppression (0.42), and high theater (0.58). The rupture_progressive_reading represents the 'spirit of the Council' interpretation enabling ongoing reform; it would show higher extraction from traditionalists and progressives alike. The rupture_traditionalist_reading represents pre-1960s doctrinal realism; it would show high extraction from the magisterium and progressive implementers. The composite_overdetermination_reading decomposes Vatican II into multiple distinct structural changes (liturgical, ecumenical, ecclesiological, political) rather than one unified reform, showing different extraction patterns per component. All four readings are linked via network.affects_constraints because they compete for institutional authority and interpretive legitimacy. The continuity reading (here) is the dominant reading—institutionalized in magisterial documents and papal teaching—but its dominance depends on suppressing the competing readings and the scholarly consensus supporting them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__continuity_reading, analytical, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
