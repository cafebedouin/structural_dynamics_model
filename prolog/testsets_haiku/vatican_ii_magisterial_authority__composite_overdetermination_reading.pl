% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority via Composite Overdetermination
 *   domain: institutional/ecclesiastical/hermeneutical
 *
 * SUMMARY:
 *   Vatican II (1962–1965) stands as the most significant Catholic Church
 *   council in four centuries. It reformed the liturgy, reconceived the
 *   Church's relationship to the modern world, opened dialogue with other
 *   Christian traditions and non-Christian religions, and reframed episcopal
 *   collegiality. Yet from the moment the Council ended, observers have
 *   debated its meaning: did it represent organic development of Catholic
 *   doctrine (continuity reading) or fundamental break with preconciliar
 *   ecclesiology (rupture reading)? This constraint story instantiates a
 *   third reading: Vatican II is neither a unified development nor a coherent
 *   rupture, but an overdetermined composite in which incompatible
 *   ecclesiological visions were deliberately encoded in ambiguous language
 *   to achieve supermajority votes. The Curia and Rome's doctrinal bodies
 *   then control meaning by interpreting which reading the texts 'really'
 *   support. Implementation divergence (progressive liturgical reform,
 *   traditionalist liturgical rejection, divergent ecumenical stances) is a
 *   structural feature of the constraint, not a failure to apply Vatican II
 *   correctly — it is baked into the texts. The 10–12% dissenting votes on
 *   key documents are not marginal objections but structural signals that the
 *   bishops recognized embedded incompatibility and voted no to prevent it.
 *
 * KEY AGENTS:
 *   - Roman Curia (hermeneutical gatekeepers): controls interpretive authority over conciliar texts; benefits from ambiguity because it ensures perpetual control
 *   - Progressive theological faction (bishops, Jesuits, ressourcement theologians): reads Vatican II as rupture and modernization; benefits because texts genuinely support their reading
 *   - Traditionalist episcopal faction (Cardinals Ottaviani, Browne, their allies): reads Vatican II as continuity and organic development; pays because their reading loses in institutional implementation despite textual support
 *   - Conciliar minority dissenters: bishops who voted no; their dissent is structural evidence of embedded incompatibility
 *   - Lay Catholics: bear the cost of doctrinal confusion; trapped with no hermeneutical standing to resolve the contradiction
 *   - Academic theologians: benefit from the ambiguity as resource for plural theological projects; constrained by the hermeneutical competition it enforces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.61).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority via Composite Overdetermination").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "institutional/ecclesiastical/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '7d6cb08e-87d4-4681-a6b5-822e1e66ce09').
narrative_ontology:cs_kernel_codification('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', formalized).
narrative_ontology:cs_authority_grounding('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', extraction).
narrative_ontology:cs_interpretation_layer_present('7d6cb08e-87d4-4681-a6b5-822e1e66ce09').
narrative_ontology:cs_reading_relation('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', foundational, conciliar_texts_encode_incompatible_ecclesiologies).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_incompatible_ecclesiologies, holdable).
narrative_ontology:cs_axiom_grounding('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', conciliar_texts_encode_incompatible_ecclesiologies, empirically_contingent).
narrative_ontology:cs_axiom('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', foundational, hermeneutical_authority_is_real_authority).
narrative_ontology:cs_axiom_status(hermeneutical_authority_is_real_authority, holdable).
narrative_ontology:cs_axiom_grounding('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', hermeneutical_authority_is_real_authority, deontological).
narrative_ontology:cs_axiom('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', secondary, ambiguity_is_structurally_intentional).
narrative_ontology:cs_axiom_status(ambiguity_is_structurally_intentional, holdable).
narrative_ontology:cs_axiom_grounding('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', ambiguity_is_structurally_intentional, empirically_contingent).
narrative_ontology:cs_reference_frame('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', council_fathers_supermajority_coalition_intent).
narrative_ontology:cs_drift_state('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', contemporary_curial_hermeneutical_control, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7d6cb08e-87d4-4681-a6b5-822e1e66ce09', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia_hermeneutical_gatekeepers).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_theological_faction).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_episcopal_faction).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_doctrinal_consistency_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, academic_theologians_divergent_schools).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_minority_dissenters).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, academic_theologians_divergent_schools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the official interpretation and implementation of conciliar texts through Vatican offices, doctrinal commissions, and liturgical normalization bodies. Their power derives from the ambiguity itself: because the texts support multiple readings, whoever interprets them authoritatively determines which reading prevails institutionally. They benefit from the composite structure because it creates perpetual hermeneutical control — every reinterpretation of implementation can claim fidelity to 'the Council's real meaning.' Professional identities and institutional positions depend on maintaining interpretive authority over the Conciliar deposit.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia_hermeneutical_gatekeepers, agenda_setter,
    institutional, generational, identity_locked, universal).

% Theologians, bishops, and formation communities reading Vatican II as fundamental ecclesiological rupture: openness to the world, ressourcement, liturgical vernacularization, collegiality, religious freedom. They benefit because the texts authentically encode their reading — the ambiguity was intentionally designed to let them prevail in interpretation without explicitly overruling prior doctrine. Their constraint is that they must continuously defend their reading against claims it violates tradition; the composite structure lets them do so by citing the texts' genuine textual support.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_theological_faction, beneficiary,
    organized, biographical, constrained, global).

% Bishops and theologians for whom Vatican II's texts authentically encode continuity with preconciliar ecclesiology: no rupture, organic development, preservation of essential doctrines on authority, liturgy, and mission. They are victims of the composite structure because their reading is textually present but institutionally loses — the Curia's interpretive power and the progressive faction's numerical dominance in implementation bodies push toward the rupture reading. Their constraint is paying in credibility and institutional marginalization: they must defend fidelity to the Council while their reading loses in the competition for official interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_episcopal_faction, payer,
    powerful, generational, constrained, continental).

% Lay Catholics expecting the Church's teaching to be coherent and its reinterpretations to be transparent. They bear the cost of the composite structure as cognitive dissonance and institutional confusion: preconciliar catechesis contradicts postconciliar catechesis, bishops give conflicting implementations, and the Church teaches that both are faithful to the same Council. They cannot exit without ceasing to be Catholic; they cannot resolve the contradiction without hermeneutical authority they do not possess.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_doctrinal_consistency_seekers, payer,
    powerless, biographical, trapped, local).

% The bishops who voted against final conciliar documents (10–12% of votes on key texts like Sacrosanctum Concilium, Unitatis Redintegratio, Nostra Aetate). They recognized the ambiguity and voted no to prevent it; their dissent is structural evidence of embedded incompatibility. They pay in institutional marginalization and the irrelevance of their warnings — the documents passed despite their opposition, and their reading (that the texts encode rupture disguised as continuity, or that the texts are dangerously unclear) is treated as obstruction rather than prophetic clarity.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_minority_dissenters, payer,
    moderate, biographical, constrained, global).

% The conciliar bishops as a collective deliberative body are effectively excluded from interpretation post-Council. Once the documents were promulgated, interpretive authority shifted to Vatican offices and later to academic theologians; the Council as an agent ceased to exist. The original supermajority coalition that built the ambiguous compromise is no longer present to defend what it intended. They are excluded from the conversation about what they created.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_council_fathers_original_coalition, excluded,
    moderate, biographical, constrained, global).

% Different theological schools (Ressourcement, Nouvelle Théologie, Neo-Thomist, Liberation, Feminist) each cite Vatican II texts as support for incompatible programs. They benefit because the ambiguity provides authentic textual purchase for multiple projects; they pay because the constraint forces them to engage in hermeneutical competition rather than transparent doctrinal debate. The texts' ambiguity is both their resource and their prison.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, academic_theologians_divergent_schools, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, academic_theologians_divergent_schools, payer).

% The body of prior papal and conciliar teaching before 1962 functions as a silent participant in the struggle: each reading claims continuity with it, the texts were written to accommodate both claims, but the doctrine itself cannot adjudicate the interpretive dispute. Its absence from the postconciliar conversation is structural — the composite texts made it impossible to reference prior doctrine without triggering the very hermeneutical contest the texts were designed to contain.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, pre_conciliar_magisterium_textual_heritage, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__composite_overdetermination_reading, pre_conciliar_magisterium_textual_heritage).

% Church historians, hermeneutics scholars, and institutional analysts examining the Council's internal dynamics and textual composition. They observe that the final documents reflect deliberate compromises encoding both continuity and rupture language; they can trace the amendment process showing how ambiguous formulations were chosen to achieve supermajority votes. Their analytical position lets them see the structure others are defending or attacking.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_narrative_observers_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia_hermeneutical_gatekeepers).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves a supermajority conciliar vote on modernizing the Church's relation to the world, secular governance, and liturgical practice without explicitly fracturing the episcopal body or formally repudiating prior magisterium. The coordination solves the problem of how to change the Church's basic institutional posture while maintaining the appearance (and for some, the reality) of organic continuity.
% TRANSFER_FUNCTION: Transfers interpretive authority from the collegial Council (once it ends) to Vatican offices and theological magisterium, who then control what the Council 'really meant.' Also transfers credibility and institutional legitimacy from preconciliar doctrine to whatever reading the dominant interpretive faction promotes. Lay Catholics transfer cognitive security (clear doctrine) for institutional loyalty (accept whatever Rome says Vatican II said).
% ABSENT_VOICES: The bishops who voted no (10–12% on key texts) are nominally present in the records but structurally excluded from postconciliar interpretation. Preconciliar theologians and bishops opposed to aggiornamento are entirely absent from the interpretation bodies that shaped implementation. The Orthodox churches and Protestant observers (who had no vote but significant influence on some texts, especially Unitatis Redintegratio) have no hermeneutical standing in the Curia's implementation. Lay Catholics who notice the contradiction are absent from doctrinal interpretation bodies. None of these parties can effectively contest the official reading once promulgated.
% DISAPPEARANCE_RATIONALE: If Vatican II and the composite-overdetermination constraint vanished, the Church would revert to preconciliar institutional postures on liturgy, religious freedom, and ecumenical stance. The progressive faction would lose its primary textual legitimacy. The traditionalist faction would regain institutional authority. The Curia would lose the hermeneutical leverage the ambiguity provides. Lay Catholics would either recover doctrinal clarity (preconciliar standards) or face explicit rupture from the Curia (progressive implementation) — the ambiguity would be gone. The entire postconciliar institutional story depends on the Council's texts existing as overdetermined compromise.
% FOUNDING_PROBLEM: In the early 1960s, a Catholic Church structured on preconciliar institutional assumptions (liturgy in Latin, cautious posture toward modernity, papal absolutism, no doctrinal engagement with Protestantism or religious pluralism) faced pressure from within the episcopacy and from Vatican II's agenda-setters (notably the Rhenish bishops and Pope John XXIII) to modernize. The founding problem was: how to modernize fast enough to prevent schism and institutional irrelevance, but not so fast as to alienate traditionalist bishops and rupture the Church's claim to unbroken doctrinal continuity?
% FOUNDING_PROBLEM_CORROBORATION: Progressive bishops and theologians (the Rhenish bloc, Jesuits, ressourcement scholars) attest the founding problem was urgent — the Church faced modernization-or-death pressure. John XXIII's opening address names aggiornamento as the Council's goal. Traditionalist bishops at the Council (Cardinals Ottaviani, Browne, and their faction) and subsequent traditionalist scholars attest the founding problem was manufactured — the Church faced no existential crisis, and the pressure came from a minority bent on doctrinal revision disguised as development. Non-Catholic historians and scholarship (Klaus Schatz, Massimo Faggioli, Giuseppe Alberigo's collaborative history) corroborate that the Council's internal documents show deliberate compositional choice to encode ambiguity; the founding problem's urgency was real to the progressive faction and perceived as false by the traditionalist faction, so the texts reflect both readings intentionally. The corroboration comes from historians outside the benefiting factions, not from within the Church's own institutional voice.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the Curia's interpretive control extracts credibility and institutional authority from the lay faithful and traditionalist faction. Suppression is moderate-high (0.61) because hermeneutical gatekeeping prevents alternative readings from gaining institutional traction — dissenting bishops lose status, traditionalist seminaries are defunded, progressive bishops control implementation bodies. Theater is high (0.58) because much of the Curia's enforcement activity defends the fiction that there is one unified 'Vatican II teaching' rather than plural readings; the theater grows over time as implementation divergence becomes undeniable and the official response becomes more insistent that the Council's meaning is settled (it is not — only one reading is institutionally endorsed). Accessibility_collapse is high (0.72) because once lay Catholics recognize the ambiguity, they see no path to alternative institutional authority — they can join traditionalist communities (constrained exit) or accept the Curia's reading (no real exit). Resistance is moderate (0.54) because traditionalist bishops and the SSPX resistance are real but organizationally weak and institutionally isolated; lay Catholic resistance is widespread but powerless. The measurement series show base_extractiveness rising as progressive implementation hardens and traditionalist readings are gradually marginalized; theater_ratio rises as performative claims of unified Vatican II teaching become more theatrical in the face of obvious divergence; suppression_requirement rises as the Curia invests in interpretive gatekeeping to maintain control.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (traditionalists, lay Catholics, dissenters) and the agenda-setter seat (Curia) should compute as radically different types. The Curia might compute as rope-like (genuine coordination with the progressive faction, shared benefits in hermeneutical authority). The traditionalist bishops might compute as snare-like (extractive, coercive, covering extraction in coordination language). Lay Catholics might compute as pure extraction (epistemic expropriation). The engine's per-seat computation reveals this divergence from the structural data alone — no tuning needed.
 *
 * DIRECTIONALITY LOGIC:
 *   The Curia is the primary beneficiary (d near 0.0–0.2): they collect interpretive authority, their reading gains institutional traction, their position requires no exit because they control the system. Progressive theologians are secondary beneficiaries (d near 0.2–0.4): their reading is supported by the texts, but they must continuously compete for institutional dominance against the Curia's gatekeeping; their exit is to become traditionalists or leave the Church. Traditionalist bishops are targets (d near 0.7–0.9): their reading is textually sound but institutionally loses; their exit is schism (via SSPX or independent traditionalist jurisdictions), identity-locked (Catholic identity fused with preconciliar doctrine). Lay Catholics are targets (d near 0.8–1.0): trapped with no exit, identity-locked, bearing the full cost of confusion. Conciliar minority dissenters are targets (d near 0.7): their prophetic dissent was overridden, they lose institutional status. The heterogeneity of d values across seats drives the constraint's classification as tangled_rope: genuine coordination problem (need for supermajority vote, need for modernization, need to maintain unity) paired with asymmetric extraction (beneficiaries are few and institutional, victims are many and dispersed).
 *
 * MANDATROPHY ANALYSIS:
 *   Vatican II's founding problem was modernization: the Church needed to engage with modernity, ecumenism, and religious pluralism to avoid institutional irrelevance. The founding problem remains live (modernity has not ceased, religious pluralism is permanent, ecumenism is ongoing). However, the original problem (how to modernize without schism) has shifted: the constraint is no longer solving modernization-vs.-unity; it is solving progressive-dominance-while-maintaining-unity. The founding problem's status is contested: progressives say the Council solved modernization (founding problem = live and solved); traditionalists say the Council was unnecessary (founding problem = manufactured); historians say the Council created a new problem (unity-through-ambiguity) that has become unstable. The constraint's mandate has not become obsolete — it still maintains nominal institutional unity — but its original justification has shifted. The Curia uses Vatican II to justify progressive implementation; traditionalists cite Vatican II to justify resistance to progressive implementation. The founding problem's resolution is no longer transparent. This creates mandatrophy pressure: if the founding problem is truly solved (modernization achieved), why does the Church continue investing in hermeneutical gatekeeping to enforce one reading? The constraint's persistence despite the founding problem's putative resolution suggests either that the problem is not solved, or that the constraint persists for reasons other than solving the founding problem — both mandatrophy indicators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorial_intent_vs_textual_support,
    'Did the conciliar bishops deliberately encode ambiguous language to achieve supermajority votes, or did ambiguity emerge from genuine theological diversity that the texts reflect honestly without strategic design?',
    'Analysis of conciliar amendment records, voting bloc alignment with final textual provisions, and testimony from key redaction committee members (e.g., Bea, Willebrands, Congar working papers). The intent question is partly empirical (did they intend it), partly hermeneutical (does intention settle meaning).',
    'If deliberate design: the constraint is a pure extraction mechanism masquerading as coordination; the Curia''s interpretive authority becomes parasitic on a knowingly ambiguous deposit. If emergent: the texts genuinely encode plural readings, and interpretation competition is legitimate; the Curia''s control is not extractive, merely authoritative. If partly both: the texts are overdetermined but not wholly strategically composed — some ambiguity was tactical, some was honest diversity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_intent_vs_textual_support, empirical, 'Whether Vatican II''s composite structure reflects deliberate stratagem or honest theological diversity.').

omega_variable(
    identity_locked_curia_position,
    'Can a Curial theologian or official genuinely adopt the traditionalist reading and remain institutionally credible, or does institutional survival require adopting (or publicly defending) the progressive reading?',
    'Empirical study of career trajectories: promotion and visibility for officials who publicly espouse the traditionalist reading vs. the progressive reading; analysis of hermeneutical control mechanisms (doctrinal commissions, canonization proceedings, episcopal appointments) and whether they show bias toward one reading.',
    'If institutional credibility is identity-locked to the progressive reading, the Curia''s claim to neutral interpretive authority is false — it is the beneficiary faction''s enforcement arm. The constraint shifts from tangled_rope (genuine coordination with asymmetric extraction) toward snare (extraction with coordination cover). If genuine pluralism of readings is institutionally possible for Curial officials, the extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_curia_position, empirical, 'Whether the Curia''s hermeneutical authority is institutionally locked to one reading.').

omega_variable(
    continuity_vs_rupture_logical_compatibility,
    'Is it logically possible for the same texts to simultaneously be continuous with preconciliar doctrine AND rupture with it, or does accepting one reading logically foreclose the other?',
    'Formal hermeneutical and logical analysis: can both readings claim the same textual passages as support without equivocation, or does one reading require reinterpreting the texts in a way that makes the other reading textually indefensible? This is a conceptual/logical question, not empirical.',
    'If logically compatible: the texts are genuinely overdetermined; both readings are defensible. If logically incompatible: the constraint is not a composite overdetermination but a hermeneutical contest where one reading must be false or strategically misrepresenting the texts. This affects whether the constraint is a rope (genuine coordination of incompatible factions) or a snare (one faction imposing a false reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_rupture_logical_compatibility, conceptual, 'Whether continuity and rupture readings are logically compatible or mutually foreclosing.').

omega_variable(
    lay_epistemic_asymmetry,
    'Do lay Catholics experience the constraint as ambiguity in the texts themselves, or as authoritative reinterpretation by the Curia that they cannot verify against the texts?',
    'Empirical study: survey and interview lay Catholics on their hermeneutical access to conciliar documents; assess whether they can independently read Vatican II and form their own judgments, or whether they depend entirely on Curial interpretation and bishops'' implementation. Document education, language barriers, institutional gatekeeping.',
    'If lay Catholics have epistemic access, they can contest Curial interpretations and become active agents in the hermeneutical competition. If they do not, the constraint operates as pure epistemic extraction: the Curia controls the meaning while lay Catholics pay in doctrinal confusion. The suppression mechanism shifts from structural (ambiguous texts genuinely constrain what can be claimed) to internalized (lay Catholics believe they lack standing to interpret).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_epistemic_asymmetry, empirical, 'Whether suppression of lay hermeneutical agency is structural or internalized.').

omega_variable(
    reading_committer_ambiguity,
    'This reading claims Vatican II encodes incompatible visions via intentional ambiguity. But is the overdetermination reading itself a third coherent reading of the texts, or is it a meta-reading about how the texts relate to other readings?',
    'Hermeneutical clarification: treat the composite-overdetermination reading as a reading OF the texts, not a reading ABOUT the readings. Can one cite Vatican II passages that explicitly authorize the claim that the texts encode incompatible ecclesiologies? Or is this reading a structural observation about the texts'' function rather than a reading derived from the texts'' content?',
    'If the overdetermination reading is itself a substantive reading of the texts, it competes with continuity and rupture on equal epistemic footing. If it is a meta-reading, it describes the hermeneutical situation but does not adjudicate which of the sibling readings is correct — it can coexist with either. This affects how the engine classifies the reading''s relationship to continuity and rupture (forecloses, coexists, influences).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Whether the composite-overdetermination reading is a substantive ecclesiological reading or a meta-hermeneutical observation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t5, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(vati_tr_t5, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t15, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement_basis(vati_tr_t15, observed).
narrative_ontology:measurement(vati_tr_t25, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 25, 0.56).
narrative_ontology:measurement_basis(vati_tr_t25, observed).
narrative_ontology:measurement(vati_tr_t35, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement_basis(vati_tr_t35, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 50, 0.6).
narrative_ontology:measurement_basis(vati_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t5, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(vati_be_t5, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t15, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(vati_be_t15, observed).
narrative_ontology:measurement(vati_be_t25, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(vati_be_t25, observed).
narrative_ontology:measurement(vati_be_t35, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(vati_be_t35, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(vati_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t5, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(vati_su_t5, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t15, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(vati_su_t15, observed).
narrative_ontology:measurement(vati_su_t25, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(vati_su_t25, observed).
narrative_ontology:measurement(vati_su_t35, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 35, 0.61).
narrative_ontology:measurement_basis(vati_su_t35, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(vati_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.18).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, catholic_liturgical_reform_implementation_divergence).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_collegiality_post_vatican_ii).

% DUAL FORMULATION NOTE:
% Vatican II magisterial authority is a single contested kernel instantiated as three constraint stories with different claimed_types and ε values. The composite_overdetermination_reading (this file) claims tangled_rope with high ε; the continuity_reading claims mountain with low ε; the rupture_reading claims tangled_rope with different ε and victim/beneficiary structure. All three are structurally valid readings of the same conciliar texts. The three stories are linked via network.affects_constraints because each reading's adoption affects the structural conditions for the others: if overdetermination is the correct reading, continuity and rupture are both textually defensible but institutionally contested; if continuity is correct, rupture is textually indefensible; if rupture is correct, continuity is cover story. The network encodes the epistemic dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
