% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority: Rupture-Traditionalist Reading
 *   domain: ecclesiology/institutional history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) was summoned by Pope John XXIII to update the
 *   Catholic Church's engagement with the modern world while maintaining
 *   doctrinal identity. This constraint embodies ONE READING of the Council's
 *   doctrinal authority — the rupture-traditionalist reading, which claims
 *   the Council represents a genuine break with prior magisterial teaching,
 *   that its documents contain internal contradictions and ambiguities, and
 *   that these ambiguities have been exploited to implement reforms the texts
 *   do not explicitly authorize. Under this reading, the constraint operates
 *   as a tangled rope: it coordinated a real aggiornamento impulse
 *   (progressives and ecumenists collect from the ambiguities) while
 *   asymmetrically extracting from traditionalists and doctrinal
 *   conservatives (who lose liturgical standing and doctrinal certainty). The
 *   extraction persists because the conciliar magisterium must enforce the
 *   Council's authority while the texts remain unresolved, and
 *   traditionalists cannot appeal to pre-conciliar doctrine because the
 *   Council is declared normative — yet find no stable alternative doctrine
 *   within the Council itself. The claim/metric independence is intentional:
 *   this reading claims tangled_rope; the metrics describe extraction
 *   accumulating over 60 years, suppression-requirement rising as enforcement
 *   machinery hardens, and theater-ratio climbing as performative
 *   reconciliation of incompatible readings displaces actual resolution.
 *
 * KEY AGENTS:
 *   - Vatican II magisterium: institutional agenda-setter; trapped between asserting conciliar authority and defending the documents' textual stability.
 *   - Progressive reform coalition: beneficiary; collects authority and freedom to implement from conciliar warrant and textual ambiguity.
 *   - Traditional Latin Mass adherents: victim/payer; identity-locked; bear the cost of liturgical suppression and marginalization.
 *   - Doctrinal conservatives: victim/payer; constrained exit; must defend pre-conciliar tradition while the Council is declared authoritative.
 *   - Ecumenical advancement proponents: beneficiary; organized; collect prestige and resources from conciliar openness and interpretive flexibility on other churches.
 *   - Post-conciliar implementing bishops: agenda-setter with secondary payer role; constrained; must implement ambiguous texts while managing factional legitimacy loss.
 *   - Vatican theology academy: observer; sees the competing readings but has no power to resolve the ambiguities retroactively.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.71).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.68).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority: Rupture-Traditionalist Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '65ff1203-2c6a-464e-a1c1-28dbc2112b9f').
narrative_ontology:cs_kernel_codification('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', fixed_text).
narrative_ontology:cs_authority_grounding('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', extraction).
narrative_ontology:cs_interpretation_layer_present('65ff1203-2c6a-464e-a1c1-28dbc2112b9f').
narrative_ontology:cs_reading_relation('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', foundational, cardinal_rupture_with_tradition).
narrative_ontology:cs_axiom_status(cardinal_rupture_with_tradition, holdable).
narrative_ontology:cs_axiom_grounding('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', cardinal_rupture_with_tradition, empirically_contingent).
narrative_ontology:cs_axiom('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', foundational, conciliar_texts_contain_doctrinal_error).
narrative_ontology:cs_axiom_status(conciliar_texts_contain_doctrinal_error, holdable).
narrative_ontology:cs_axiom_grounding('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', conciliar_texts_contain_doctrinal_error, deontological).
narrative_ontology:cs_axiom('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', secondary, ambiguity_enables_heterodox_implementation).
narrative_ontology:cs_axiom_status(ambiguity_enables_heterodox_implementation, holdable).
narrative_ontology:cs_axiom_grounding('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', ambiguity_enables_heterodox_implementation, empirically_contingent).
narrative_ontology:cs_reference_frame('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', pre_conciliar_doctrinal_clarity_and_liturgical_stability).
narrative_ontology:cs_drift_state('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', contemporary_post_vatican_ii_disorder, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('65ff1203-2c6a-464e-a1c1-28dbc2112b9f', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reform_coalition).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_advancement_proponents).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_adherents).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_conservatives).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_orthodoxy_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_implementing_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Council itself and the post-conciliar magisterium that interprets and implements its documents. From the traditionalist seat, it authored texts with genuine internal contradictions between conciliar documents themselves and between council texts and pre-conciliar teaching. It continues to enforce orthodoxy around the conciliar texts (Unitatis Redintegratio, Sacrosanctum Concilium, Dignitatis Humanae) while those texts enable heterodox readings. The magisterium is institutionally committed to the Council as event and must defend its authority, yet the textual ambiguities persist unresolved.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_magisterium, agenda_setter,
    institutional, generational, trapped, global).

% Bishops, theologians, and ecclesial movements (many in Northern Europe and North America) who read Vatican II as authorization for substantial doctrinal and liturgical reform. The Council's ambiguities enable them to pursue ecumenical dialogue, vernacular liturgy, pastoral flexibility, and engagement with modernity while claiming conciliar warrant. They benefit from the Council's authority shield and the textual malleability that lets them advance positions the Council's letter does not explicitly endorse.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reform_coalition, beneficiary,
    organized, generational, mobile, global).

% Priests and laity committed to pre-conciliar liturgy, theology, and discipline. From the traditionalist reading, they bear the cost of the Council's rupture: their liturgical form was suppressed, their doctrinal certainty undermined, their ecclesiastical standing marginalized. The ambiguities in conciliar documents mean they cannot definitively establish that the Council forbade what was done to them; instead, progressive interpretation of ambiguous texts was mobilized to dismantle their institutional home. Their exit option is blocked by identity fusion with Catholic tradition itself — abandoning the magisterium's authority means abandoning Catholicism, yet the magisterium has turned hostile to their form of the faith.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_adherents, payer,
    moderate, biographical, identity_locked, global).

% Cardinals, theologians, and senior clergy who believe Vatican II's texts contain genuine errors of commission (statements contradicting prior doctrine) and omission (failures to defend traditional positions). They continue to work within the magisterium and canon law but bear the burden of defending the pre-conciliar tradition while the conciliar texts are treated as authoritative. Their constrained exit: they can dissent privately but institutionally they must accept the Council's authority or lose their seat at the table where doctrine is adjudicated. Some (like Archbishop Lefebvre's followers) have exit; most remain trapped.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_conservatives, payer,
    powerful, generational, constrained, global).

% Theologians, church officials, and interfaith bodies who benefit from the Council's openness to dialogue with other Christian communions and world religions. Unitatis Redintegratio's ambiguities about the Church's relationship to other churches enable both charitable re-reading of separated communions AND substantive doctrinal rapprochement. They collect prestige, institutional resources, and intellectual freedom to pursue reunion projects that the pre-conciliar magisterium would have foreclosed.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_advancement_proponents, beneficiary,
    organized, generational, mobile, global).

% Clergy and catechists tasked with handing on Catholic doctrine as formulated before Vatican II. They bear the burden of explaining why the old formulas are no longer operative, why the Council changed what was presented as unchangeable, and why the new texts must be read in light of the old (a reconciliation project that itself requires work they are not compensated to do). They face the accusation of rigidity from progressives and betrayal from traditionalists; their exit is constrained by obedience vows and employment within the institutional Church.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_orthodoxy_maintainers, payer,
    moderate, biographical, constrained, global).

% Individual bishops who must implement conciliar decrees in their dioceses. From the traditionalist reading, they inherit ambiguous texts and are pressured from Rome to move toward progressive implementation (especially on liturgy and ecumenism) while the texts do not compel this. Some bishops are themselves progressive and drive the reform; others are constrained by Rome's directive authority. All bear the cost of institutional legitimacy loss as the pre-conciliar structures are dismantled and factions within their dioceses — traditional and progressive — accuse each other of betraying the Council.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_implementing_bishops, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_implementing_bishops, payer).

% Theological commissions, the Congregation for the Doctrine of the Faith, and academic theological bodies that must interpret and defend conciliar teaching. They see both the progressive readings that exploit ambiguities and the traditionalist readings that claim the texts are flawed. Their analytical role carries no power to resolve the ambiguities retroactively; they can only issue clarifications that themselves enter the hermeneutical contest.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_theology_academy, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reform_coalition).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vatican II was summoned to aggiornamento — update Catholic teaching and practice to address modern challenges while maintaining doctrinal integrity. The coordination problem: how to modernize the Church's engagement with the world without rupturing with tradition. From the rupture-traditionalist reading, this coordination failed; the texts embody compromise rather than resolution and enable two incompatible trajectories.
% TRANSFER_FUNCTION: The arrangement transfers institutional authority from pre-conciliar precedent and practice to conciliar texts as the new source of legitimacy, while leaving the conciliar texts themselves doctrinally indeterminate on key issues (liturgical form, ecumenical status of other churches, relationship to modernity). Progressive interpreters collect the authority of the Council while advancing positions the texts do not require; traditionalists lose the standing to appeal to pre-conciliar doctrine as normative, since the Council is declared normative, yet find no stable doctrine in the Council itself. The transfer moves authority upward (to the Council event itself) while dispersing doctrinal content downward into competing interpretations.
% ABSENT_VOICES: Eastern Orthodox and Protestant theologians were observers at the Council but not voters. Lay Catholics — the ordinary faithful — were not consulted on liturgical change or ecumenical openness. Traditional religious communities (especially contemplative orders) whose charism was rooted in pre-conciliar practice were not substantially heard when their form of religious life was disrupted. From the traditionalist seat, these absent voices represent communities that would have objected to the ambiguities and their predictable consequences.
% DISAPPEARANCE_RATIONALE: Progressive readers argue that if Vatican II disappeared (its decrees repealed), the Church would be trapped in pre-conciliar rigidity and cut off from legitimate dialogue with modernity and other churches. Traditionalists argue that if the Council and its ambiguities disappeared, pre-conciliar clarity would be restored, the liturgy would stabilize, and ecumenical overreach would be checked — or, alternatively, the Church would face the full force of the modernist question it tried to compromise with rather than confront. The disappearance verdict is contested because the two readings have incommensurable predictions about what stability would look like.
% FOUNDING_PROBLEM: The Church faced a genuine challenge: how to respond to the Second Vatican Council's summons from Pope John XXIII to aggiornamento without losing doctrinal identity. Pre-conciliar ecclesiology was presented as unchangeable; post-war modernity demanded engagement. The founding problem was how to update and open the Church while maintaining that identity.
% FOUNDING_PROBLEM_CORROBORATION: Both progressive and traditionalist readers agree the founding problem is no longer live in the same form — the Council happened, aggiornamento was attempted, and sixty years have passed. The progressive reading attests the problem was solved by opening the Church to the modern world through conciliar reform. The traditionalist reading attests the problem was misdiagnosed: the Council was the cure that became the disease, introducing doctrinal confusion that has prevented a genuine response to modernity. Historians outside both camps (e.g., the Oxford movement studies, institutional sociology of Vatican) attest that the Council's texts contain unresolved tensions that enable exactly the competing implementations the traditionalist reading describes.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.45 at t=0, immediately post-Council) because the conciliar documents are fresh and appear normative; progressives and traditionalists both hope the texts will resolve ambiguities in their favor. By t=12 (early post-conciliar reform period), extractiveness rises to 0.58 as progressive readings prevail in implementation and traditionalists realize the ambiguities are being resolved against them systematically. The trajectory flattens after t=36 (around 2000) at 0.70–0.71 as the post-conciliar order calcifies: both progressives and traditionalists have entrenched positions, the major liturgical and ecumenical shifts are accomplished facts, and no further large reorganization is being driven by conciliar interpretation itself. Theater-ratio climbs from 0.32 to 0.52 because post-conciliar discourse becomes increasingly performative: the magisterium issues clarifications affirming both the Council's authority and pre-conciliar continuity; theologians produce reconciliation narratives; liturgical innovation is defended as 'in the spirit of Vatican II' while the texts do not require it. The performances are real — they dominate Catholic intellectual and pastoral life — but they do not resolve the underlying ambiguities; they displace them. Suppression-requirement rises from 0.48 to 0.68 because the constraint's persistence depends increasingly on enforcement: traditionalists must be kept from appealing to pre-conciliar doctrine as normative; progressive implementations must be defended as conciliar even when the texts do not mandate them; bishops must suppress factional conflict between parishes with traditional and modern liturgies. The constraint's persistence is enforced, not voluntary.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium's seat: the constraint is genuine coordination. Vatican II solved the aggiornamento problem by opening the Church to the world; the ambiguities are features, not bugs — they enable pastoral flexibility and dialogue with modernity. From the progressive coalition's seat: the constraint is beneficial coordination they helped design; the ambiguities are liberation theology, because they enable the Church to become what it should have been. From the traditionalist and conservative seats: the constraint is pure extraction disguised as coordination. Ambiguities are cover for doctrinal rupture; the magisterium enforces a modernized orthodoxy while claiming continuity; the old forms are suppressed not for theological reasons but to prevent traditionalists from having an institutional seat to occupy. The engine's per-seat computation should detect this divergence: beneficiary seats computing rope or strong coordination; victim seats computing snare or weak tangled_rope; moderate power/constrained exit seats showing the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Vatican II magisterium (agenda-setter, institutional power) has directionality d near 0.5 – symmetric: it benefits from the Council's authority (the event validates its reforming mission and ecumenical outreach) and bears the cost of perpetual hermeneutical crisis (defending contradictory texts, suppressing factional conflict). Progressive coalition (beneficiary, organized, mobile) has d near 0.2 – beneficiary end: they collect the authority of the Council while pursuing positions the texts do not require; their exit is mobile because they can leave the magisterium if suppressed and still maintain their ecumenical and reformist projects in academia or separated communities. Traditional Latin Mass adherents (payer, moderate power, identity-locked) have d near 0.85 – target end: they bear the costs of marginalization, liturgical suppression, and doctrinal demotion; their exit is identity-locked because leaving the magisterium means leaving Catholicism entirely, and their identity is constituted through fidelity to the pre-conciliar tradition they have no other seat to occupy. Doctrinal conservatives (payer, powerful, constrained) have d near 0.65 – toward the target end: they have greater power and exit options than traditionalists (they remain inside magisterial structures and can publish, teach, influence from within) but are constrained by obedience and institutional loyalty; their directionality is modulated upward from traditionalists by their power and partial exit but remains extractive because the constraint's texts are used against them systematically. The divergence between seats is structural and intentional: the agenda-setter benefits asymmetrically; the payers bear costs that increase over time.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: how to modernize while maintaining identity. The founding problem status is now dead (the Council happened, the order is established). But the arrangement persists and is defended, which flags mandatrophy: the Council's utility as a problem-solving mechanism has been replaced by its function as a legitimacy anchor. The post-conciliar Church invokes the Council not to solve new problems but to justify past decisions and foreclose alternative directions. The ambiguities that were originally presented as compromise-in-service-of-aggiornamento are now defended as features that enable ongoing discernment. This is the classic mandatrophy pattern: the institution no longer asks 'what was the Council for?' but instead 'what does the Council allow?' — and the answer to the latter question shifts with the magisterium's interests. A mandatrophy declaration should fire here: the founding problem is dead; the constraint persists; the theater ratio is accumulating; the suppression requirement is rising.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_intentionality,
    'Were the ambiguities in Vatican II''s documents the result of genuine theological disagreement among conciliar fathers, or the result of deliberate compromise language chosen to enable later divergent implementation?',
    'Historical analysis of conciliar debates, voting patterns, and redaction history (e.g., the _Acta Synodalia_ and conciliar commission records). If intentional compromise can be documented, it strengthens the traditionalist reading''s claim that the ambiguities are structural rather than interpretive accidents.',
    'If intentional, the constraint is a designed tangled rope — coordination problem-solving through deliberate ambiguity. If accidental (genuine disagreement left unresolved), the constraint is a snare masquerading as coordination, and the post-conciliar implementation becomes a power struggle over what the unresolved language means.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_intentionality, empirical, 'Whether conciliar ambiguities were deliberate compromises or unresolved theological disputes.').

omega_variable(
    continuity_vs_rupture_kernel_determination,
    'Is Vatican II''s relationship to prior tradition fundamentally describable as continuity (with novelty as explication) or rupture (with novelty as change)?',
    'This is a kernel-level question: the four readings coexist precisely because the conciliar texts permit both descriptions. No empirical evidence can resolve this — only a magisterial declaration reinterpreting the Council itself could foreclose one reading. The question remains open at the kernel level.',
    'If continuity is established as the binding interpretation (via new magisterial act), the rupture_traditionalist reading becomes structurally untenable — it would foreclose on axiom cardinal_rupture_with_tradition. If rupture is established, the continuity_reading is foreclosed. Currently, both coexist and the constraint persists because the magisterium officially holds both positions (affirming continuity in principle while allowing/endorsing rupture-like implementations in practice).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_vs_rupture_kernel_determination, conceptual, 'The fundamental hermeneutical question: is Vatican II continuous development or structural rupture? The kernel structure permits both readings; resolution requires new magisterial act.').

omega_variable(
    implementation_extraction_attribution,
    'To what extent is the post-conciliar implementation''s departure from traditionalism attributable to ambiguities in the conciliar texts, versus attributable to progressive bishops and theologians choosing expansive interpretation despite the texts?',
    'Comparison of conciliar text against implemented practice, plus analysis of episcopal circulars, theological writings, and pastoral decisions. If major implementations track ambiguous language in the texts, the texts are extractive. If implementations depart from what the texts actually say, the extraction is in the reading, not the constraint itself — and the constraint should be reclassified as the magisterium''s choice, not the conciliar ambiguity.',
    'This affects the classification: high extractiveness assumes the ambiguities are being exploited; if the ambiguities are NOT the mechanism but rather cover for choices the texts do not license, the constraint shifts to a snare with literary cover rather than a tangled rope with structural ambiguity. The victim set and directionality remain the same, but the structure of coercion changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_extraction_attribution, empirical, 'Whether implementation extractiveness is driven by textual ambiguity or by interpretive choices that go beyond the texts.').

omega_variable(
    traditionalist_identity_lock_mechanism,
    'Is the identity_locked exit for traditional Latin Mass adherents structural (the identity is constitutively tied to fidelity to pre-conciliar Catholicism, which the magisterium has now declared superseded), or internalized (adherents have absorbed narratives that leaving is apostasy)?',
    'Post-exit trajectory analysis: if traditionalists who leave the magisterium''s jurisdiction (e.g., joining independent traditional communities or Eastern Orthodoxy) report ongoing identity-fusion with Catholicism, the lock is structural. If they report liberation and identity reconstruction, the lock was partly internalized. Also: exposure to counter-narrative (e.g., traditionalists reading the conciliar texts freshly, without magisterial guidance) and cognitive reframing experiments.',
    'If structural, the constraint''s suppression operates through institutional architecture (no seat for traditionalists inside the magisterium to occupy), and exit is genuinely unavailable. If internalized, the constraint could be disrupted by counter-narratives and identity reframing. The measured suppression (0.68) is appropriate for structural lock; if evidence emerges that the lock is partly internalized, post-exit psychology should show lower suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_identity_lock_mechanism, empirical, 'Whether traditionalist identity-lock is structural or internalized; affects interpretation of suppression metric and exit availability.').

omega_variable(
    magisterial_authority_legitimacy_source,
    'Does the magisterium''s authority to declare Vatican II binding rest on pre-conciliar papal authority (in which case traditionalists have a claim that conciliar rupture violates the pope''s prior authority), or on the Council itself as a self-constituting event (in which case the Council is the source of its own authority and cannot violate itself)?',
    'Theological and legal analysis of conciliar authority theory. If pre-conciliar papal authority is foundational, the traditionalist argument has structural force. If the Council is self-founding, the rupture argument loses ground. The magisterium''s own position (evolved through Vatican II to Vatican III discussions) holds the Council as the binding act, which supports the latter framing.',
    'This determines whether the constraint is a legitimate reorganization of authority (Council as self-founding) or an illegitimate exercise of authority (Council as violating pre-conciliar papal supremacy). Under the latter reading, the constraint is not just extractive but may be classified as invalid, shifting toward snare with a claim of authority it does not possess.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_legitimacy_source, conceptual, 'The grounding of Vatican II magisterium''s authority: pre-conciliar papal authority or self-founding conciliar act.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(vati_tr_t6, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(vati_tr_t12, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 12, 0.43).
narrative_ontology:measurement(vati_tr_t24, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(vati_tr_t36, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 36, 0.5).
narrative_ontology:measurement(vati_tr_t48, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 48, 0.51).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vati_be_t6, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(vati_be_t12, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(vati_be_t24, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(vati_be_t36, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 36, 0.69).
narrative_ontology:measurement(vati_be_t48, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 48, 0.7).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(vati_su_t6, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(vati_su_t12, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement(vati_su_t24, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(vati_su_t36, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 36, 0.66).
narrative_ontology:measurement(vati_su_t48, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 48, 0.67).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Vatican II's doctrinal authority is a contested kernel instantiated by four distinct constraint stories: rupture_traditionalist_reading (this file), rupture_progressive_reading, continuity_reading, and composite_overdetermination_reading. Each story authors the same kernel (Vatican II as standing commitment) but under a different reading of its meaning, authority, and legitimacy. The ε values differ because each reading instantiates different beneficiary/victim structures and interprets the conciliar texts' ambiguities differently. Rupture readings (both this traditionalist and the progressive) share the empirical claim that Vatican II represents structural change; they differ in how they evaluate that change (loss vs. liberation). The family links enable the framework to model how one reading's configuration constrains or enables another reading's plausibility, and how institutional power shapes which reading becomes normative in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
