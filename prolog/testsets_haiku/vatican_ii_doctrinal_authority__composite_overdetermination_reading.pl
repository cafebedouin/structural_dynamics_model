% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Doctrinal Authority as Composite Overdetermination
 *   domain: ecclesiology/institutional_history
 *
 * SUMMARY:
 *   Vatican II (1962–1965) is presented as a unified, coherent reform of the
 *   Catholic Church. This reading argues the opposite: Vatican II is a
 *   composite of at least four structurally distinct changes—liturgical
 *   (replacement of Latin with vernacular), ecclesiological (expansion of
 *   episcopal authority and lay role), ecumenical (opening to other Christian
 *   traditions), and political (alignment with religious freedom and
 *   pluralism)—each with different degrees of rupture/continuity, different
 *   beneficiary/victim structures, and different extractiveness. The
 *   Council's presentation as 'unified reform' is a rhetorical and
 *   institutional achievement that obscures these component differences. The
 *   composite structure allowed the Curia and progressive bishops to claim
 *   both fidelity to tradition (continuity reading) and necessary
 *   modernization (rupture reading) simultaneously. This reading rejects both
 *   the pure-continuity and pure-rupture framings as category errors applied
 *   to an inherently multi-component phenomenon.
 *
 * KEY AGENTS:
 *   - Hierarchical Curia: orchestrated Council convocation and interpretive authority; benefited from ambiguity
 *   - Progressive Episcopal Coalition: seized interpretive authority to authorize rapid reform in their dioceses
 *   - Traditionalist Orders and Conservative Bishops: identity-locked to pre-conciliar forms; paid the cost of composite structure's ambiguity
 *   - Laypeople (Liturgical Continuity Seekers): absorbed fragmentation in parish practice; constrained exit
 *   - Vatican Diplomatic Apparatus: wove Cold War and post-colonial geopolitics into Council framing
 *   - Doctrinal Historians: analytical observers measuring component-level divergences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Doctrinal Authority as Composite Overdetermination").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional_history").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '091e8f6c-f378-44e0-8948-568c893ef71c').
narrative_ontology:cs_kernel_codification('091e8f6c-f378-44e0-8948-568c893ef71c', formalized).
narrative_ontology:cs_authority_grounding('091e8f6c-f378-44e0-8948-568c893ef71c', lineage).
narrative_ontology:cs_interpretation_layer_present('091e8f6c-f378-44e0-8948-568c893ef71c').
narrative_ontology:cs_reading_relation('091e8f6c-f378-44e0-8948-568c893ef71c', vatican_ii_doctrinal_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('091e8f6c-f378-44e0-8948-568c893ef71c', vatican_ii_doctrinal_authority__rupture_progressive_reading, influences).
narrative_ontology:cs_reading_relation('091e8f6c-f378-44e0-8948-568c893ef71c', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, influences).
narrative_ontology:cs_axiom('091e8f6c-f378-44e0-8948-568c893ef71c', foundational, council_exhibits_component_independence).
narrative_ontology:cs_axiom_status(council_exhibits_component_independence, holdable).
narrative_ontology:cs_axiom_grounding('091e8f6c-f378-44e0-8948-568c893ef71c', council_exhibits_component_independence, empirically_contingent).
narrative_ontology:cs_axiom('091e8f6c-f378-44e0-8948-568c893ef71c', foundational, composite_framing_masks_structural_asymmetries).
narrative_ontology:cs_axiom_status(composite_framing_masks_structural_asymmetries, holdable).
narrative_ontology:cs_axiom_grounding('091e8f6c-f378-44e0-8948-568c893ef71c', composite_framing_masks_structural_asymmetries, empirically_contingent).
narrative_ontology:cs_reference_frame('091e8f6c-f378-44e0-8948-568c893ef71c', organic_development_framework).
narrative_ontology:cs_drift_state('091e8f6c-f378-44e0-8948-568c893ef71c', contemporary_implementation_record, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('091e8f6c-f378-44e0-8948-568c893ef71c', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, hierarchical_curia).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, episcopal_conferences).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_orders).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_practitioners_liturgical_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_episcopal_coalition).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_diplomatic_apparatus).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_protestant_partners).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, organic_development_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ressourcement_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the Council's interpretive agenda and enforces doctrinal authority through post-Council commissions, synods, and papal teaching. Orchestrates the composite framing as continuous organic development. Cannot exit the role: the Curia's existence depends on the Church's institutional coherence. Benefits from the ambiguity because it preserves curial power over implementation in each diocese.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, hierarchical_curia, agenda_setter,
    institutional, civilizational, trapped, global).

% Bishops in progressive dioceses (mainly Western Europe and North America) seize the Council's ambiguous language to authorize rapid modernization: vernacular Mass, laicization of parish roles, ecumenical cooperation, accommodation to secular law on contraception and divorce. They benefit from expanded pastoral authority under the composite framing while maintaining formal fidelity to the documents. Could have resisted the Council (traditionalist bishops did), but chose to embrace it as mandate for their preferred changes.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_episcopal_coalition, beneficiary,
    organized, generational, mobile, regional).

% Religious orders whose charism and daily practice were constituted through pre-conciliar Latin liturgy, traditional disciplines, and hierarchical obedience (Dominicans, Franciscans, Benedictines, Carmelites). Experience the Council as permitting rapid abandonment of the forms they took vows to protect. Cannot exit without severing their fundamental identity: leaving the Church means abandoning their vows. Must watch their communities dissolve or radically transform. Identity-locked because their self-understanding fuses with the pre-conciliar Church's material forms.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_orders, payer,
    moderate, civilizational, identity_locked, regional).

% Laypeople who knew the Mass in Latin and experienced the sacraments through pre-conciliar ritual forms. The Council permits rapid replacement of the liturgy they learned, internalized, and built their religious identity around. They cannot formally challenge the bishop's implementation because the Council documents genuinely permit it (Sacrosanctum Concilium affirms both Latin and vernacular). Exit means leaving the Church—but family, community, cultural identity are bound up with Catholicism. Constrained exit because leaving means social and familial rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_practitioners_liturgical_continuity, payer,
    powerless, biographical, constrained, local).

% The Vatican's Secretariat of State wove Cold War geopolitical calculation, Ostpolitik, decolonization, and pluralist state theology into the Council's framing. Religious freedom doctrine justified engagement with liberal democracies and Cold War neutrality. Ecumenism justified alliance-building with Protestant churches in anti-communist alignment. The composite framing allowed the Vatican to present these moves as doctrinal development rather than institutional necessity driven by geopolitics. Cannot exit: the Vatican's survival as state depends on diplomatic relevance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_diplomatic_apparatus, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_diplomatic_apparatus, beneficiary).

% Bishops who read the Council as continuity (especially in conservative dioceses and the Curia itself) are systematically marginalized from implementation authority. The narrative of 'authentic Council interpretation' is controlled by progressive coalitions and Rome. They object that their reading is being suppressed, but the composite framing means their exclusion can be justified as 'not understanding the Council's spirit.' They are identity-locked because they cannot leave the episcopacy without abandoning their vows and authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conservative_bishops_excluded, excluded,
    moderate, civilizational, identity_locked, regional).

% Protestant churches benefit from Vatican II's opening: recognition of 'elements of truth' in Protestant tradition, ecumenical dialogue commissions, lifting of mutual condemnations. The composite framing allows the Church to extend genuine doctrinal opening while protecting papal supremacy and Marian doctrine through careful language. Protestants benefit from the warming but the composite structure's ambiguity means full reconciliation remains impossible—the Council both opens and closes dialogue depending on which reading governs.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_protestant_partners, beneficiary,
    powerful, generational, mobile, global).

% Historians and theologians studying the Council from outside institutional authority (or from the margins of it) measure the degree of rupture and continuity in each component. They discover that the 'unified reform' narrative obscures the component-level divergences. They observe the constraint's structure without enforcing it—their analytical position provides perspective on how the composite framing functions institutionally.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, doctrinal_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, hierarchical_curia).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vatican II coordinated the Catholic Church's institutional posture toward post-World War II global conditions by packaging multiple distinct structural adaptations—liturgical modernization, religious freedom doctrine, ecumenical opening, expanded episcopal and lay authority—as a unified, doctrinally continuous reform. The coordination problem was: how to appear modernized and dialogical without admitting rupture with tradition, and how to distribute authority between Rome and bishops without fragmenting institutional coherence.
% TRANSFER_FUNCTION: Transfers interpretive authority from conservative-traditionalist bishops toward progressive episcopal coalitions and the Vatican Curia. Transfers lived liturgical continuity from traditionalist orders and lay practitioners to the Church's institutional flexibility and diplomatic posture. Transfers doctrinal stability (certainty about fixed teaching) from those seeking immutable doctrine to those navigating the 'spirit' of reform and ongoing development. Transfers cultural authority over 'authentic' Catholicism from pre-conciliar forms toward post-conciliar institutional narration.
% ABSENT_VOICES: Traditionalist religious orders and rank-and-file laypeople whose liturgical world was fragmented by implementation. They object that the Council authorized rupture while claiming continuity, but are structurally excluded from interpretation-setting authority in the Church's official decision-making. Underground traditionalist communities (Society of Saint Pius X, sedevacantist groups, pre-conciliar Mass movements) exist outside the dialogue, their exclusion justified by dismissing them as 'not understanding the authentic Council.' These voices would argue for either genuine return to pre-conciliar forms or formal acknowledgment that the Council represents rupture, not continuity—neither position is admitted to the official composite framing.
% DISAPPEARANCE_RATIONALE: If Vatican II and its composite overdetermination framing disappeared overnight—if the Church reverted to pre-conciliar doctrinal fixity, Latin liturgy, and hierarchical exclusivity—the global Catholic institutional world would reorganize dramatically. The modern priest shortage emerges partly from Vatican II's opening of religious life to laicization; reverting would demand reassertion of priestly celibacy and male exclusivity. Ecumenical dialogues would dissolve. The Church's role in post-colonial pluralist states would invert toward institutional defensiveness. Millions of lay Catholics whose participation depends on vernacular Mass and expanded roles would face exclusion or departure. The Vatican's geopolitical standing would collapse. The constraint's function is so thoroughly embedded that its disappearance forces reorganization at every institutional level.
% FOUNDING_PROBLEM: The Church faced an institutional coherence crisis in the 1960s: it claimed immutable doctrine yet confronted a world transformed by World War II, decolonization, nuclear weapons, secular science, ecumenical movements, and pluralist liberal states. Pre-conciliar responses had been piecemeal and defensive—the Church isolated itself rather than adapting. The founding problem was: how can the Church maintain doctrinal authority and institutional coherence while appearing to adapt to modern conditions?
% FOUNDING_PROBLEM_CORROBORATION: The Vatican apparatus and progressive episcopal coalitions attest the founding problem is solved: Vatican II opened the Church, modernized its posture, began genuine ecumenical dialogue, and allowed the Church to exist within pluralist societies. Traditionalist theologians and conservative bishops attest the founding problem was never genuinely solved—the Church's attempt to claim both continuity and rupture created present confusion, fragmentation, and loss of cohesion. Academic institutional historians and comparative theologians from outside the Church (secular historians, Protestant theologians, Orthodox observers) corroborate that Vatican II dramatically shifted the Church's global posture and institutional structure, but debate whether the 'unified reform' narrative accurately captures the transformation or obscures component-level divergences and power asymmetries.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.62 at interval end) reflects the Church's transfer of lived continuity from traditionalists and lay practitioners to institutional flexibility and diplomatic power. The theater ratio rises sharply from 0.20 (1962, immediately post-Council) to 0.47 (2012): in early years, the Council's organizational work was genuine institutional transformation; by 2012, much enforcement energy defends the claim that the Council was continuous and unified—theater protecting the composite narrative against accumulating evidence of component divergence. Suppression requirement plateaus around 0.58–0.59 from 1988 onward: the Church must actively suppress traditionalist interpretations (disband non-approved traditionalist communities, restrict the pre-conciliar Mass, discipline conservative bishops) to maintain the composite structure. The measurements use a shared time grid (t0=1962 at Council opening; t1=1975, a decade into implementation; t2=1988, post-John Paul II's election and reassertion of conservative theology; t3=2000, post-2000 Jubilee; t4=2012, fifty years later). Each metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the Curia's and progressive bishops' perspectives, Vatican II is a successful unified reform: the Church modernized, opened to dialogue, distributed authority, and maintained doctrinal coherence through development. From traditionalist and lay continuity seekers' perspectives, the same Council is an ambiguous rupture: they experience fragmentation, displacement, and suppression of their readings. The engine computes this divergence from stakeholder power, exit, and role: agenda-setters (Curia, progressive bishops) with powerful/organized power and mobile exit compute as beneficiaries; traditionalists and laypeople with constrained/identity-locked exit and payer roles compute as targets. The divergence is structural, not a matter of interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: (1) Hierarchical Curia: beneficiary role + institutional power + trapped exit → d near 0.0 (full beneficiary; they set the rules and cannot leave). (2) Progressive Episcopal Coalition: beneficiary role + organized power + mobile exit → d near 0.1–0.2 (beneficiary with exit option; they could have refused the Council's authority, but they embraced it). (3) Traditionalist Orders: payer role + moderate power + identity-locked exit → d near 0.85–0.95 (full target; they bear the cost and cannot exit without severing identity). (4) Lay Practitioners (Liturgical Continuity): payer role + powerless + constrained exit → d near 0.90 (full target; they absorb the cost with almost no exit option). The Curia and progressive bishops' directionality differs markedly from traditionalists and lay practitioners', driven by the same constraint but experienced as beneficial vs. extractive. No directionality overrides are needed; the structural data produces the divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is coherence: the Church claims immutable doctrine yet faces a pluralist world demanding institutional adaptation. Vatican II was built to solve this by packaging multiple adaptations as unified organic development. The constraint's function (solving institutional coherence via composite overdetermination) is live: without the Council's ambiguous framing, the Church would face an explicit choice between rupture and isolation. The mandatrophy question is whether the composite framing is still solving the founding problem or merely postponing acknowledgment of real component-level differences. By 2012, the theater ratio (0.47) and persistent traditionalist resistance suggest the founding problem is becoming contested: does the Church's coherence really rest on treating liturgical change, religious freedom, and ecumenism as unified, or is it masking divergences that demand different justifications? The constraint persists because acknowledging the components' independence would force the Church to abandon the 'organic development' narrative and explicitly adjudicate which components represent real doctrine vs. disciplinary adaptation. No mandatrophy resolution is declared because the constraint remains functionally necessary to the institutional apparatus, even as its cover story deteriorates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    component_independence_boundary,
    'Are the four components (liturgical, ecclesiological, ecumenical, political) genuinely independent structurally, or are they aspects of a single coherent development that the composite framing merely disaggregates?',
    'Genealogical investigation of each component''s theological and institutional origins: trace liturgical reform, religious freedom doctrine, ecumenical openness, and episcopal authority separately through pre-conciliar history. If each has independent intellectual and institutional lineages, the components are independent; if each is logically entailed by a single prior commitment, they are unified.',
    'If components are independent, the composite reading is correct and the Council genuinely unified multiple trajectories. If they are aspects of a unified development, the continuity reading is vindicated and the composite framing is a misanalysis. This resolves the entire kernel contest''s logical structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_independence_boundary, empirical, 'Whether Vatican II components are structurally independent or unified.').

omega_variable(
    composite_as_cover_or_necessity,
    'Is the composite overdetermination framing a deliberate institutional cover story deployed by the Curia and progressive bishops to mask incompatible goals, or a genuine intellectual necessity emerging from the Council''s attempt to balance pre-conciliar and modern commitments?',
    'Analysis of archival material from Council preparatory commissions, bishops'' interventions, and post-Council editorial decisions: did institutional actors deliberately construct ambiguity, or did the ambiguity emerge from genuine doctrinal tension? Interview data from progressive implementers and conservative resisters on whether they understood the composite structure as intentional or incidental.',
    'If deliberately constructed cover story: the constraint''s persistence rests on suppressing alternative readings and the theater ratio will rise toward 0.8+ as the cover story deteriorates. If genuine intellectual necessity: the constraint is a stable tangled rope solving a real coordination problem, and theater remains moderate. This determines whether the constraint''s future is institutional maintenance vs. growing theatricality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_as_cover_or_necessity, conceptual, 'Whether composite overdetermination is deliberate or emergent.').

omega_variable(
    traditionalist_suppression_asymmetry,
    'Is the measured suppression of traditionalist readings (0.58 at interval end) structural (logically entailed by the composite framing''s coherence) or behavioral (a choice by progressive institutional actors to delegitimize alternatives)?',
    'Comparative analysis of post-Council institutional treatment: where progressive bishops had power, did they actively suppress traditionalist alternatives, or did traditionalists opt out? In dioceses with conservative bishops, did the composite framing persist equally? Examination of canonical and magisterial documents: do they formally foreclose traditionalist readings, or merely declare progressive readings authentic?',
    'If structural suppression: traditionalist orders and communities are identity-locked victims with d near 0.90. If behavioral suppression: the suppression is contingent on progressive institutional dominance and traditionalist resistance is a form of contention rather than extraction. This determines whether the constraint''s victims are intrinsic or contingent on implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_suppression_asymmetry, empirical, 'Whether traditionalist suppression is structural or behavioral.').

omega_variable(
    reading_incommensurability_itself,
    'Is this composite_overdetermination_reading genuinely incommensurable with the continuity and rupture readings, or does it decompose the kernel into sub-components that each continuity/rupture reading also implicitly applies?',
    'Formal analysis of each sibling reading''s internal structure: does continuity reading claim that ALL four components are continuous, or does it apply continuity selectively (e.g., continuity in ecumenism but not liturgy)? Does rupture reading claim universal rupture or selective rupture? If siblings apply continuity/rupture component-wise, they are not incommensurable with the composite reading—they are incomplete versions of it.',
    'If truly incommensurable: this reading forecloses siblings by showing their category error. If partially commensurable: siblings are coexisting partial readings and this reading is a framework organizing them rather than displacing them. This determines the reading_relations topology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incommensurability_itself, conceptual, 'Whether composite reading is incommensurable with or encompasses sibling readings.').

omega_variable(
    liturgical_extraction_separability,
    'Is the extractiveness from lay practitioners (loss of Latin liturgy, fragmentation of lived continuity) a necessary cost of the Church''s institutional modernization, or a separable choice made by progressive implementers?',
    'Counterfactual analysis: could the Church have adopted religious freedom doctrine, ecumenical opening, and episcopal authority expansion while preserving the Latin Mass as legitimate (not merely tolerated) within the universal Church? If yes, the extractiveness is separable from the core institutional reform and represents a choice. If no, the extractiveness is a structural necessity.',
    'If separable: the lay practitioners are victims of a policy choice, not inherent to modernization; the constraint''s type could be tangled_rope (if the core institutional reform is genuine coordination) or snare (if the choice to eliminate the Latin rite was pure extraction). If necessary: the lay practitioners are paying the cost of the Church''s genuine institutional evolution and the extraction is justified as coordination cost. This resolves the beneficiary/victim structure and type assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_extraction_separability, conceptual, 'Whether liturgical extraction is inherent to Vatican II or a separate choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1962, 2012).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement_basis(vati_tr_t1962, observed).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement_basis(vati_tr_t1975, observed).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1988, 0.42).
narrative_ontology:measurement_basis(vati_tr_t1988, observed).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2000, 0.46).
narrative_ontology:measurement_basis(vati_tr_t2000, observed).
narrative_ontology:measurement(vati_tr_t2012, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2012, 0.47).
narrative_ontology:measurement_basis(vati_tr_t2012, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement_basis(vati_be_t1962, observed).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement_basis(vati_be_t1975, observed).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1988, 0.58).
narrative_ontology:measurement_basis(vati_be_t1988, observed).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(vati_be_t2000, observed).
narrative_ontology:measurement(vati_be_t2012, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2012, 0.62).
narrative_ontology:measurement_basis(vati_be_t2012, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.42).
narrative_ontology:measurement_basis(vati_su_t1962, observed).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement_basis(vati_su_t1975, observed).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1988, 0.58).
narrative_ontology:measurement_basis(vati_su_t1988, observed).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2000, 0.59).
narrative_ontology:measurement_basis(vati_su_t2000, observed).
narrative_ontology:measurement(vati_su_t2012, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2012, 0.58).
narrative_ontology:measurement_basis(vati_su_t2012, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the kernel vatican_ii_doctrinal_authority. The composite_overdetermination_reading argues that Vatican II is not a single unified shift (continuity or rupture) but a convergence of four structurally distinct components—liturgical, ecclesiological, ecumenical, political—each with independent extractiveness profiles and degrees of rupture/continuity. This reading decomposeshttps://the kernel and applies different structural classifications to different components. The sibling readings (continuity, rupture_progressive, rupture_traditionalist) each apply a single unified judgment (continuity or rupture) to the Council as a whole. The readings coexist across different institutional parties and theological communities; no single party would hold all four simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
