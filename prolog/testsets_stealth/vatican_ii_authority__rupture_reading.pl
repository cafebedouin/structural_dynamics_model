% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Post-Conciliar Doctrinal Authority Regime (Rupture Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This story instantiates the rupture_reading of the vatican_ii_authority
 *   kernel. The standing arrangement under contest is the post-conciliar
 *   doctrinal authority regime: the conciliar documents administered as
 *   binding teaching, the hermeneutic of continuity as the sole licensed
 *   interpretation, and the canonical machinery that disciplines refusal.
 *   Assessed by this reading's own lights, the regime retains genuine
 *   ecclesial coordination (sacramental unity, governance, ecumenical
 *   interface) while extracting assent to teachings it judges erroneous,
 *   stripping liturgical patrimony, and concentrating interpretive office in
 *   a progressive establishment. Constraint-family decomposition: the
 *   colloquial label 'Vatican II's authority' covers structurally distinct
 *   claims, so this reading is authored separately from its siblings and
 *   linked to them. The continuity reading authors low epsilon over the SAME
 *   referent (it sees organic development, not extraction); the
 *   composite_overdetermination reading declines to resolve the referent at
 *   all. Epsilon differs across the family because the readings differ, not
 *   because the referent moves.
 *
 * KEY AGENTS:
 *   - - post_conciliar_magisterium: Agenda setter (institutional/trapped) — promulgates conciliar teaching, administers the continuity hermeneutic, enforces assent
 *   - - progressive_theological_establishment: Primary beneficiary (powerful/mobile) — occupies the offices the settlement opened
 *   - - traditionalist_clergy: Primary target (organized/constrained) — bears canonical irregularity for refusing implementation
 *   - - traditional_lay_catholics: Target (powerless/identity_locked) — bears loss of liturgical and catechetical patrimony
 *   - - suppressed_traditional_orders: Target (organized/trapped) — dissolved or placed under administration
 *   - - ecumenical_dialogue_partners: Secondary beneficiary (organized/mobile) — receive unilateral accommodation
 *   - - diocesan_bishops: Dual-positioned (institutional/constrained) — gain discretion, absorb implementation conflict
 *   - - coetus_internationalis_patrum: Excluded voice (powerful/trapped) — the outvoted council minority, now deceased
 *   - - historians_of_the_council: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.78).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.8).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Post-Conciliar Doctrinal Authority Regime (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, 'dd2f394f-757e-4815-8e68-e9c7e8675f38').
narrative_ontology:cs_kernel_codification('dd2f394f-757e-4815-8e68-e9c7e8675f38', fixed_text).
narrative_ontology:cs_authority_grounding('dd2f394f-757e-4815-8e68-e9c7e8675f38', extraction).
narrative_ontology:cs_interpretation_layer_present('dd2f394f-757e-4815-8e68-e9c7e8675f38').
narrative_ontology:cs_reading_relation('dd2f394f-757e-4815-8e68-e9c7e8675f38', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('dd2f394f-757e-4815-8e68-e9c7e8675f38', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('dd2f394f-757e-4815-8e68-e9c7e8675f38', foundational, conciliar_documents_contain_irreconcilable_errors).
narrative_ontology:cs_axiom_status(conciliar_documents_contain_irreconcilable_errors, holdable).
narrative_ontology:cs_axiom_grounding('dd2f394f-757e-4815-8e68-e9c7e8675f38', conciliar_documents_contain_irreconcilable_errors, theological).
narrative_ontology:cs_axiom('dd2f394f-757e-4815-8e68-e9c7e8675f38', secondary, assent_limited_to_non_erroneous_teaching).
narrative_ontology:cs_axiom_status(assent_limited_to_non_erroneous_teaching, holdable).
narrative_ontology:cs_axiom_grounding('dd2f394f-757e-4815-8e68-e9c7e8675f38', assent_limited_to_non_erroneous_teaching, theological).
narrative_ontology:cs_reference_frame('dd2f394f-757e-4815-8e68-e9c7e8675f38', tridentine_doctrinal_synthesis).
narrative_ontology:cs_drift_state('dd2f394f-757e-4815-8e68-e9c7e8675f38', post_conciliar_implementation_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('dd2f394f-757e-4815-8e68-e9c7e8675f38', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, progressive_theological_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, ecumenical_dialogue_partners).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, diocesan_bishops).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_lay_catholics).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, suppressed_traditional_orders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, diocesan_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates the conciliar documents as authoritative teaching, administers the official interpretive framework that reads them as continuous with prior doctrine, and enforces assent through canonical discipline: mandates for the revised liturgy, review of seminary curricula, and penalties for institutes and clerics who refuse implementation. Cannot abandon its own office; its legitimacy claim is bound to the settlement it administers.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, trapped, global).

% Holds the seminary chairs, curial consultorships, doctrinal review boards, and editorial positions the conciliar settlement opened. Careers, publications, and appointments flow through fidelity to the post-conciliar direction; credentials transfer readily across universities, dicasteries, and national conferences.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, progressive_theological_establishment, beneficiary,
    powerful, biographical, mobile, global).

% Priests formed in the pre-conciliar rite, concentrated in societies operating under canonical irregularity since the 1988 consecrations. They celebrate the older liturgy, form seminarians outside approved channels, and absorb suspension decrees, suppressed faculties, and warnings of excommunication. Leaving ministry means abandoning ordination and community; staying means living without regular canonical status.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditionalist_clergy, payer,
    organized, generational, constrained, global).

% Laity attached to the pre-conciliar Mass, catechesis, and devotional life. They travel long distances to reach permitted celebrations, depend on the irregular institutes for sacraments, and carry the loss of parish life, schools, and burial customs their grandparents took for granted. Their religious identity is fused with the older forms; abandoning them means losing the faith-community itself, not merely a preference.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_lay_catholics, payer,
    powerless, biographical, identity_locked, global).

% Religious institutes dissolved, merged, or placed under commissars for resisting liturgical and doctrinal implementation. Their property passed to other bodies, their novitiates were closed, and their members faced dispensation-from-vows pressure. Reconstitution inside the approved structures requires accepting the very reforms they refused.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, suppressed_traditional_orders, payer,
    organized, generational, trapped, continental).

% Protestant bodies and Orthodox churches engaged through the conciliar opening. They gained a counterpart that softened exclusive claims, adopted shared translations and common statements, and reframed conversion language. Their own positions remain fully intact; the adjustment ran in one direction.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, ecumenical_dialogue_partners, beneficiary,
    organized, generational, mobile, global).

% Govern particular churches under the conciliar settlement. Collegiality rhetoric and liturgical translation authority widened their local discretion, but they must implement directives they did not author, absorb traditionalist backlash in their dioceses, and answer Rome for disorder either way. Permission regimes for the older liturgy land on their desks as administrative burdens.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, diocesan_bishops, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, diocesan_bishops, payer).

% The organized bloc of council fathers who contested the progressive drafts, circulated minority interventions, and warned against the schemas on liturgy, revelation, and religious liberty. Outvoted on the floor, their objections survive in council diaries and private correspondence; they are deceased and absent from every subsequent conversation about what the council meant.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, coetus_internationalis_patrum, excluded,
    powerful, generational, trapped, universal).

% Academic scholars working from the published acta, diaries of minority fathers, and drafting histories. They document the procedural pressures, vote margins, and post-conciliar reception battles without holding a confessional stake in either continuity or rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, historians_of_the_council, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__rupture_reading, progressive_theological_establishment).
narrative_ontology:fixing_cost_class(vatican_ii_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single worldwide teaching authority, a standardized liturgy, and unified sacramental discipline across the Catholic communion, and organizes the Church's public engagement with other Christian bodies and the modern state.
% TRANSFER_FUNCTION: Moves doctrinal assent, liturgical patrimony, institutional positions, and property from traditionally-formed clergy, laity, and religious institutes to the post-conciliar theological establishment and its appointees.
% ABSENT_VOICES: The council's conservative fathers' bloc (Coetus Internationalis Patrum) objected to the decisive schemas and is now deceased; their interventions survive only in diaries and acta. Suppressed institutes before 1988 had no seat in the conversations that dissolved them. Both would contest the official account of what the council settled.
% DISAPPEARANCE_RATIONALE: If the post-conciliar authority arrangement vanished overnight, the Church would reorganize around pre-conciliar norms: the older liturgy would resume as the ordinary form, the irregular institutes would regularize, seminary curricula would revert, and the ecumenical architecture built on softened claims would need renegotiation. Every named seat's situation depends on the arrangement holding.
% FOUNDING_PROBLEM: The Council was convened to solve the Church's estrangement from the modern world: aggiornamento (updating pastoral practice), liturgical participation, ecumenical reconciliation, and the relationship between religious liberty and the confessional state.
% FOUNDING_PROBLEM_CORROBORATION: No benefiting party's word is taken here. The minority fathers' diaries (published in the acta and scholarly editions) attest that the drafting battles were contested rather than consensual; secular historians of the council document procedural pressure on the Coetus bloc; and post-conciliar critics from inside the winning coalition itself (notably Jean Danielou's and Henri de Lubac's later complaints about post-conciliar destruction) corroborate that implementation outran the texts. The benefiting establishment's own attestation that the founding problem remains live is noted but carries no corroborating weight.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because, on this reading's assessment, the demanded assent runs to doctrines it holds contradictory with prior teaching, and the transferred goods (rite, catechesis, offices) are decoupled from any service rendered to those who lose them. Suppression is higher still (0.80) because persistence depends on canonical enforcement — mandated liturgical replacement, suppression of institutes, suspension of faculties — not on voluntary uptake. Theater is moderate-high (0.55): the official continuity framing is, per this reading, substantially performative, though the sacramental and governance functions beneath it are real. Accessibility collapse is moderate-low (0.45): exits exist (irregular institutes, permitted celebrations, Eastern Catholic forms) but carry irregularity costs that keep most adherents in place. Resistance is high (0.72): six decades of organized refusal, from the Coetus interventions through Econe to present liturgical disputes. The temporal series run on one shared seven-point grid. The suppression_requirement series is authored deliberately: enforcement capacity visibly built up (1969-1988), relaxed (Summorum Pontificum 2007), and rebuilt (Traditionis Custodes 2021) — a dynamic enforcement history, not a static picture. The oscillation is documented as cyclical rather than monotonic drift; the concession-restriction cycle may itself function as intermittent reinforcement (concede, watch adherence grow, revoke), which would make the oscillation an extraction mechanism rather than noise. Claim and metrics are independent authored facts: claimed_type is tangled_rope because even on this reading's own assessment the arrangement coordinates real sacramental and governance functions (valid orders, real jurisdiction, unified communion) while the metrics describe heavily extractive, actively enforced operation. The engine computes per-seat types from the structural data; this claim does not adjudicate the outcome.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as necessary doctrinal maintenance: from the magisterium's position the continuity hermeneutic protects the Church from self-destruction, and enforcement is fidelity. The payer seats experience the same structure as extraction of assent and patrimony under penalty. The dual-positioned bishop seat straddles: beneficiary of widened discretion, bearer of the conflict the settlement generates. The engine computes these divergent per-seat classifications from power, exit, and directional data; the divergence between the magisterial seat and the traditionalist seats is the measurable signal this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium sits near the beneficiary end (administers and collects legitimacy) but is trapped in its own office. The progressive establishment is the cleanest beneficiary: collects offices, mobile exit. Ecumenical partners benefit incidentally and cheaply. Traditionalist clergy, suppressed orders, and traditional laity sit near the full-target end — victims with constrained, trapped, or identity-locked exits respectively, which amplifies their effective extraction in that order. Diocesan bishops carry a directionality override: the structural derivation from their beneficiary listing would yield roughly 0.15-0.20, understating their position — they enforce reforms they did not author, absorb traditionalist backlash, and bear administrative costs of every permission regime, so the override sets d to 0.40. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (engaging modernity without dissolution) is contested: the benefiting establishment attests it is live; the rupture reading holds the proposed solution was defective and the arrangement now persists past any honest version of its mandate. The status-by-verdict combination (contested x world_rearranges) flags capture risk for downstream consumers: an arrangement everyone depends on, whose founding warrant half the parties dispute. Classification discipline cuts both ways here. Calling the arrangement a pure snare would erase what even this reading affirms — valid sacraments, real jurisdiction, a coordinating communion that traditionalists inhabit rather than flee. Calling it a rope would erase the asymmetric transfer the receipt surface records. Tangled rope holds both halves: coordination function, asymmetric extraction, active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the rupture_reading of the vatican_ii_authority kernel; how would the sibling readings'' structural data change the computed classification over the same referent?',
    'Compile the sibling stories (vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading) and compare per-seat classifications over the identical referent and stakeholder surface.',
    'If the continuity reading computes a rope-like profile and this reading computes a tangled-rope or snare-flavored profile over the same arrangement, the disagreement localizes in epsilon rather than structure; if the composite reading computes irresolvable ambiguity, the kernel''s classification is underdetermined by any single reading and the family verdict must be reported as contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Cross-reading classification divergence over a shared referent.').

omega_variable(
    obedience_internalization_ambiguity,
    'For traditional lay Catholics, is the measured suppression structural (canonical restriction of the pre-conciliar rites) or internalized (obedience formation that renders exit unthinkable even where permission exists)?',
    'Post-permission trajectory analysis: under Summorum Pontificum (2007-2021) the older liturgy was canonically available; attendance patterns and stated reasons for non-adoption during that window indicate how much suppression persisted without structural barrier.',
    'If internalized, effective suppression exceeds the structural measure and survives formal liberalization — the 2021 restriction fell on a population already partly self-suppressing; if structural, the liberalization episode genuinely lowered suppression and the 2021 rebound is the whole story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obedience_internalization_ambiguity, empirical, 'Structural vs internalized suppression among traditionalist laity.').

omega_variable(
    continuity_framing_good_faith,
    'Is the hermeneutic of continuity deliberate cover for extraction, or a sincere but mistaken interpretive commitment held by the arrangement''s custodians?',
    'Internal curial correspondence and drafting histories distinguishing strategic framing from conviction; differential behavior when continuity claims are publicly challenged versus quietly abandoned.',
    'If deliberate, the theater ratio measures intentional performance and the arrangement trends toward the snare side of the hybrid; if sincere, theater is overstated as intentionality and the arrangement is closer to a defective coordination held by mistaken custodians.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_framing_good_faith, conceptual, 'Good-faith versus strategic continuity framing.').

omega_variable(
    concession_cycle_mechanism,
    'Does the liberalize-then-restrict cycle (Ecclesia Dei 1988, Summorum Pontificum 2007, Traditionis Custodes 2021) function as intermittent reinforcement training compliance, or as genuine institutional ambivalence?',
    'Correlate concession timing with traditionalist growth metrics versus internal curial personnel turnover; test whether concessions predictably precede growth-triggered revocation.',
    'If intermittent reinforcement, the oscillation is itself an extraction mechanism and effective suppression is higher than any single time-point shows; if ambivalence, the cycle is unsettled policy rather than mechanism, and the flat-average suppression reading is fairer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concession_cycle_mechanism, empirical, 'Whether the concession-restriction cycle is mechanism or ambivalence.').

omega_variable(
    epsilon_referent_fixity,
    'Is epsilon here stably about the post-conciliar authority arrangement, or does it silently slide toward assessing the doctrinal content of the conciliar documents themselves?',
    'Audit each metric''s referent: extractiveness and suppression measure the enforcement-and-assent arrangement; the documents'' content enters only through this reading''s assessment of what assent is demanded for.',
    'If the referent slides, the story conflates two constraints (the authority arrangement; the documents'' doctrinal adequacy) and violates epsilon-invariance — decompose into separate stories linked by network edges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_fixity, conceptual, 'Referent stability for the rupture reading''s epsilon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1962, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__rupture_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(vati_tr_t1969, vatican_ii_authority__rupture_reading, theater_ratio, 1969, 0.35).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_authority__rupture_reading, theater_ratio, 1978, 0.48).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_authority__rupture_reading, theater_ratio, 1988, 0.52).
narrative_ontology:measurement(vati_tr_t2007, vatican_ii_authority__rupture_reading, theater_ratio, 2007, 0.44).
narrative_ontology:measurement(vati_tr_t2021, vatican_ii_authority__rupture_reading, theater_ratio, 2021, 0.58).
narrative_ontology:measurement(vati_tr_t2026, vatican_ii_authority__rupture_reading, theater_ratio, 2026, 0.55).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__rupture_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(vati_be_t1969, vatican_ii_authority__rupture_reading, base_extractiveness, 1969, 0.58).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_authority__rupture_reading, base_extractiveness, 1978, 0.7).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_authority__rupture_reading, base_extractiveness, 1988, 0.74).
narrative_ontology:measurement(vati_be_t2007, vatican_ii_authority__rupture_reading, base_extractiveness, 2007, 0.62).
narrative_ontology:measurement(vati_be_t2021, vatican_ii_authority__rupture_reading, base_extractiveness, 2021, 0.76).
narrative_ontology:measurement(vati_be_t2026, vatican_ii_authority__rupture_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__rupture_reading, suppression_requirement, 1962, 0.25).
narrative_ontology:measurement(vati_su_t1969, vatican_ii_authority__rupture_reading, suppression_requirement, 1969, 0.5).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_authority__rupture_reading, suppression_requirement, 1978, 0.68).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_authority__rupture_reading, suppression_requirement, 1988, 0.8).
narrative_ontology:measurement(vati_su_t2007, vatican_ii_authority__rupture_reading, suppression_requirement, 2007, 0.55).
narrative_ontology:measurement(vati_su_t2021, vatican_ii_authority__rupture_reading, suppression_requirement, 2021, 0.82).
narrative_ontology:measurement(vati_su_t2026, vatican_ii_authority__rupture_reading, suppression_requirement, 2026, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the authority of Vatican II'. The label conflates at least three structurally distinct claims: (1) continuity_reading — the documents develop the deposit without rupture, yielding negligible extraction over the shared referent; (2) rupture_reading (this file) — the documents contradict prior teaching, yielding high extraction over the same referent; (3) composite_overdetermination_reading — the council is an overdetermined composite whose ambiguity cannot be resolved into either pole. Each member carries its own epsilon, beneficiary/victim structure, and claimed type; all are linked here. The upstream member by empirical confidence is the continuity reading (official position, majority reception); this reading and the composite reading are downstream contestations that cite the same texts and events with different structural conclusions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__rupture_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
