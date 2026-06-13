% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Authority Structure as Composite Overdetermination
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II (1962–1965) is presented by the Roman Catholic magisterium as
 *   a unified council expressing the Holy Spirit's guidance for the modern
 *   Church. This reading rejects both the continuity framing (that reforms
 *   organically develop pre-conciliar theology) and the rupture framing (that
 *   the Council represents clear doctrinal break). Instead, Vatican II is
 *   understood as a composite of factionally negotiated compromises that
 *   embed genuine theological contradictions — particularly between documents
 *   emphasizing liturgical renewal and those emphasizing Eucharistic
 *   theology, between ressourcement (return to sources) and aggiornamento
 *   (updating), between collegiality and papal primacy, between scriptural
 *   authority and magisterial teaching. The structural result: the
 *   magisterium claims univocal authority while the documents provide textual
 *   warrant for incompatible interpretations. This creates extractive
 *   authority (the magisterium controls which contradictions are
 *   acknowledged) coordinated through institutional loyalty (factions accept
 *   the ambiguity to maintain communion) and suppressed through denial that
 *   ambiguity exists.
 *
 * KEY AGENTS:
 *   - Institutional Magisterium: the teaching authority of the Church; agenda-setter; controls official interpretation
 *   - Tradition-Conserving Clergy: experience discontinuity; constrained exit; pay cost of ambiguity suppression
 *   - Reform-Oriented Factions: benefit from textual warrant for progressive reinterpretation; coordinated by institutional loyalty despite ideological opposition
 *   - Theological Scholarship: benefits from documented contradictions; career advancement through complexity recognition
 *   - Laity Experiencing Discontinuity: powerless; trapped; experience practical confusion from unresolved interpretive conflicts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II Authority Structure as Composite Overdetermination").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'd5f1e482-fa60-4ca3-a78a-2ae0d16ecc24').
narrative_ontology:cs_kernel_codification('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', fixed_text).
narrative_ontology:cs_authority_grounding('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', extraction).
narrative_ontology:cs_interpretation_layer_present('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24').
narrative_ontology:cs_reading_relation('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_axiom('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', foundational, vatican_ii_contains_irresolvable_theological_contradictions).
narrative_ontology:cs_axiom_status(vatican_ii_contains_irresolvable_theological_contradictions, holdable).
narrative_ontology:cs_axiom_grounding('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', vatican_ii_contains_irresolvable_theological_contradictions, empirically_contingent).
narrative_ontology:cs_axiom('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', foundational, magisterial_authority_depends_on_denial_of_internal_contradiction).
narrative_ontology:cs_axiom_status(magisterial_authority_depends_on_denial_of_internal_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', magisterial_authority_depends_on_denial_of_internal_contradiction, deontological).
narrative_ontology:cs_axiom('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', secondary, factional_compromise_was_encoded_in_document_ambiguity).
narrative_ontology:cs_axiom_status(factional_compromise_was_encoded_in_document_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', factional_compromise_was_encoded_in_document_ambiguity, empirically_contingent).
narrative_ontology:cs_reference_frame('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', univocal_magisterial_interpretation).
narrative_ontology:cs_drift_state('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', contemporary_post_conciliar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d5f1e482-fa60-4ca3-a78a-2ae0d16ecc24', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, theological_scholarship_community).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, reform_oriented_factions).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, tradition_conserving_clergy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, vatican_administrative_apparatus).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, reform_oriented_factions).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, laity_experiencing_liturgical_discontinuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Roman Catholic Church claims interpretive monopoly over Vatican II documents. Must defend a coherent, univocal reading to maintain doctrinal authority; any admission of irresolvable internal contradiction undermines the claim that the Church's teaching office speaks with definitive authority. Controls the apparatus of official commentary, doctrinal enforcement, and liturgical implementation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Accept Vatican II as doctrinally binding while experiencing its implementation as systematic erosion of pre-conciliar practice and theology they consider essential to Catholic identity. Face pressure to adopt interpretations they regard as discontinuous with tradition; their resistance is branded as disobedience despite textual support in the documents themselves. Exit requires either leaving the priesthood or joining schismatic communities.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, tradition_conserving_clergy, payer,
    organized, generational, constrained, global).

% Gains professional legitimacy and career advancement by documenting the genuine theological contradictions embedded in Vatican II's composite structure. Their work flourishes precisely where institutional authority must deny complexity. Can publish freely in academic venues and maintain institutional distance from magisterial enforcement.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, theological_scholarship_community, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from the ambiguity that Vatican II's composite structure creates: contradictions in the documents provide textual warrant for progressive reinterpretation that the magisterium cannot completely suppress without admitting the documents are incoherent. Also pay the cost of living under competing interpretations of their own church's authority.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, reform_oriented_factions, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, reform_oriented_factions, payer).

% Experience the practical effect of unresolved doctrinal ambiguity: the Mass they learned to worship within is declared invalid and replaced; devotional practices are systematically discouraged; catechetical content contradicts what their parents taught them. They cannot exit (Catholicism is identity-constitutive for many) and cannot articulate their grievance within institutional channels without being labeled disobedient or resistant to the Holy Spirit.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, laity_experiencing_liturgical_discontinuity, payer,
    powerless, biographical, trapped, global).

% Watch from outside as the Roman Catholic magisterium claims to have resolved questions (ecumenism, scriptural authority, relationship to modernity) while documentary evidence shows irresolvable factional compromise. They can point to the structural ambiguity as evidence that Catholic claims to authoritative teaching lack coherence.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, rival_christian_traditions, observer,
    institutional, civilizational, analytical, global).

% Implements Vatican II's documents through successive doctrinal clarifications, liturgical directives, and canonical reforms while maintaining the fiction that each new directive is a legitimate interpretation of the documents rather than a selection among incompatible positions. Benefits from the ambiguity because it permits continued control without explicit doctrinal reversal.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, vatican_administrative_apparatus, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, vatican_administrative_apparatus, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vatican II attempted to resolve an irreconcilable set of institutional pressures: modernization of liturgy and theology without abandoning traditional authority; ecumenical opening without doctrinal relativism; engagement with modernity without capitulation to it. The documents represent factional compromise that coordinates enough agreement to allow the Council to conclude while embedding unresolved theological contradictions.
% TRANSFER_FUNCTION: The constraint transfers institutional authority from the pre-conciliar theological framework to a new interpretive apparatus that claims continuity while implementing rupture. This transfers cultural legitimacy, liturgical practice, and devotional life from tradition-conserving clergy to reform-oriented administrators and theological innovators. The magisterium transfers interpretive burden to lower-ranking theologians and parish clergy who must implement contradictory directives without acknowledgment of the contradiction.
% ABSENT_VOICES: Tridentine-oriented theologians and pre-conciliar philosophical schools were present at the Council but systematically sidelined in final text negotiation. Schismatic communities formed by those who recognized the rupture and could not accept it remain structurally excluded from any post-conciliar conversation. The ordinary laity who experienced liturgical discontinuity as traumatic were never asked whether they consented to the changes.
% DISAPPEARANCE_RATIONALE: If the Vatican II documents and the institutional authority structure built to enforce them disappeared, Catholicism would immediately reorganize: either a restored pre-conciliar framework (the schismatic solution) or a fully post-conciliar Catholicism without the fiction of continuity (the radical-reform solution). The constraint exists precisely to prevent this choice from being made explicitly.
% FOUNDING_PROBLEM: The Catholic Church in the mid-20th century faced pressure from multiple, incompatible institutional demands: the need to respond to biblical scholarship and historical-critical exegesis; the need to engage ecumenically with Protestant and Orthodox churches; the need to modernize liturgy and religious education; the need to claim authority over doctrine despite these modernizations; the need to preserve a legitimacy narrative grounded in unchanging tradition.
% FOUNDING_PROBLEM_CORROBORATION: Church historians and theological scholars outside the magisterium (John O'Malley, Margaret O'Gara, Massimo Faggioli) document the genuine historical contradictions in how Vatican II was negotiated and implemented. Conservative theologians attest that the problems Vatican II was built to solve remain unresolved and have metastasized into more severe credibility fractures. The magisterium itself never acknowledges the founding tensions, instead maintaining that Vatican II represents both continuity and necessary reform — a claim that cannot be coherently sustained.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over 60 years because the magisterium's interpretive monopoly becomes more entrenched as competing interpretations are branded as schismatic or disobedient rather than legitimate readings of ambiguous documents. Theater ratio rises from 0.38 to 0.62 as successive doctrinal clarifications, papal encyclicals, and conciliar documents are presented as 'further development' of Vatican II rather than selection among its contradictory elements — the performative claim of univocal interpretation increases. Suppression rises from 0.44 to 0.71 as the apparatus to enforce unified interpretation (doctrinal investigations, sanctions against theologians, liturgical police) strengthens. The plateau after year 48 reflects stabilization: the magisterium has successfully established that acknowledging Vatican II's ambiguity is tantamount to rejecting its authority.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterial seat, Vatican II is a single, luminous event whose implications unfold over time through legitimate development. From the scholar's seat, it is an incoherent composite of factional compromise that can be read only by selecting which contradictions to acknowledge. From the tradition-conserving clergy's seat, it is a mandate to implement rupture while claiming continuity — they experience the divergence acutely and suffer the cost of maintaining the fiction. The engine should compute different constraint types from these seats: the magisterial seat likely reads rope (coordination of factions under unified authority); the scholar's seat reads snare (interpretive monopoly extracting authority from acknowledged complexity); the clergy's seat reads tangled_rope (coordination by institutional loyalty + extraction through enforced interpretation). This story is written from a reading position between the scholar's and clergy's seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium's directionality approaches full beneficiary (d near 0.0): it controls interpretation, collects the authority-rent, has high institutional exit cost but complete power to redefine the terms. Tradition-conserving clergy face high d (near 0.8): they are constrained by identity-lock (priesthood is their identity), they bear the cost of implementing contradictory directives, and their resistance is criminalized within the structure. Reform-oriented factions have mid-range d (around 0.4): they benefit from the ambiguity but also depend on institutional membership and must navigate the fiction of univocal authority. Scholars have low d (near 0.2): they benefit from the complexity without structural dependence and have exit options (they can leave academia or the Church). The powerless laity have very high d (approaching 0.9): they are identity-locked, trapped within the institutional structure, and bear costs with no say in interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   Vatican II's founding problems are CONTESTED not dead: the pressure toward modernization, ecumenical engagement, and integration of historical-critical biblical scholarship remain active. The arrangement persists not because the founding problems are solved but because it transfers the tension from explicit theological controversy into suppressed but unresolved institutional contradiction. The theater ratio indicates performative governance: each papal encyclical (Humanae Vitae, Evangelium Vitae, etc.) is presented as 'unfolding Vatican II' when in fact it selects among the documents' contradictory implications. The constraint should be classified as tangled_rope because (1) genuine coordination occurred (factions maintained communion despite ideological opposition through the documents' ambiguity), (2) asymmetric extraction is clear (magisterial control over which contradictions are acknowledged), and (3) active enforcement is required (the doctrinal apparatus must suppress acknowledgment of ambiguity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentary_ambiguity_vs_intentional_factional_compromise,
    'Are the contradictions in Vatican II documents the result of genuine theological irresolution, or were they deliberately authored as factional compromise to hold the Council together?',
    'Historical-archival analysis of preparatory committee texts, working-group reports, and Council Fathers'' personal papers; testimony from participants about negotiation strategies; comparison of successive draft redactions.',
    'If deliberately engineered compromise, the reading is validly overdetermined and the suppression is extraction. If genuine theological ambiguity, the extraction reading weakens and the constraint might be retyped as rope (coordination under honest uncertainty). Documentary evidence suggests deliberate compromise, supporting this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(documentary_ambiguity_vs_intentional_factional_compromise, empirical, 'Whether Vatican II ambiguity is accidental or engineered.').

omega_variable(
    identity_lock_vs_institutional_choice_for_clergy,
    'Are tradition-conserving clergy genuinely trapped by identity-fusion with priesthood, or do they have real exit options and simply experience high switching costs?',
    'Post-exit trajectory studies: do clergy who leave the priesthood report that suppression persists after exit, or does it cease? Do schismatic communities or married Orthodox clergy report different constraint experiences?',
    'If identity-locked (suppression internalizes), the effective extraction is higher than the structural measure suggests and the constraint is more entrenched. If constrained but not identity-locked, exit options exist and the system has higher brittleness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_institutional_choice_for_clergy, empirical, 'Whether clergy exit constraint is structural or internalized.').

omega_variable(
    magisterial_awareness_of_ambiguity,
    'Does the institutional magisterium genuinely believe it has provided univocal interpretation of Vatican II, or does it privately acknowledge the ambiguity while enforcing suppression strategically?',
    'Declassification of Vatican internal documents (if access is ever granted); analysis of the hermeneutical rhetoric over time; comparison between papal writings intended for academic theologians versus those for general clergy/laity.',
    'If the magisterium is genuinely deceived about the coherence of its own interpretation, the theater_ratio is lower (some performativity is honest self-delusion). If the magisterium knowingly enforces suppression, the theater_ratio is higher and the extraction is more conscious.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magisterial_awareness_of_ambiguity, conceptual, 'Whether magisterial suppression of ambiguity is self-deceived or strategic.').

omega_variable(
    scholarship_beneficiary_complicity,
    'Does theological scholarship that documents Vatican II''s ambiguities serve a prophetic function (highlighting suppression, enabling exit from false univocity) or does it benefit from the constraint by gaining career capital from the dispute?',
    'Analysis of scholarly publication patterns; correlation between scholars'' documented criticism and their institutional advancement; study of whether scholarship accelerates doctrinal change or merely documents it.',
    'If scholarship serves prophetic function, it weakens the constraint by raising consciousness of suppression. If scholarship benefits from the constraint''s persistence, scholars become partial beneficiaries whose interest lies in maintaining productive ambiguity rather than resolving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholarship_beneficiary_complicity, preference, 'Whether scholarship amplifies or reinforces Vatican II ambiguity.').

omega_variable(
    kernel_reading_logical_structure,
    'Is the composite_overdetermination_reading logically coherent as a single reading, or does it collapse into one of the sibling readings under sustained analysis?',
    'Formal theological analysis comparing the reading''s axioms against the continuity and rupture readings'' axioms; test whether all three readings can simultaneously hold given the documentary evidence.',
    'If this reading forecloses the siblings (proves they cannot both be true in a single framework), the reading is logically stronger and the constraint structure is more stable. If this reading merely coexists with the siblings, all three readings remain live and the ambiguity is unreducible by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_logical_structure, conceptual, 'Whether composite_overdetermination reading is self-sustaining or derivative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t6, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 6, 0.44).
narrative_ontology:measurement_basis(vati_tr_t6, observed).
narrative_ontology:measurement(vati_tr_t12, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 12, 0.49).
narrative_ontology:measurement_basis(vati_tr_t12, observed).
narrative_ontology:measurement(vati_tr_t24, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 24, 0.56).
narrative_ontology:measurement_basis(vati_tr_t24, observed).
narrative_ontology:measurement(vati_tr_t36, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 36, 0.61).
narrative_ontology:measurement_basis(vati_tr_t36, observed).
narrative_ontology:measurement(vati_tr_t48, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 48, 0.62).
narrative_ontology:measurement_basis(vati_tr_t48, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 60, 0.62).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t6, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(vati_be_t6, observed).
narrative_ontology:measurement(vati_be_t12, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement_basis(vati_be_t12, observed).
narrative_ontology:measurement(vati_be_t24, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(vati_be_t24, observed).
narrative_ontology:measurement(vati_be_t36, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 36, 0.66).
narrative_ontology:measurement_basis(vati_be_t36, observed).
narrative_ontology:measurement(vati_be_t48, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement_basis(vati_be_t48, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t6, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement_basis(vati_su_t6, observed).
narrative_ontology:measurement(vati_su_t12, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement_basis(vati_su_t12, observed).
narrative_ontology:measurement(vati_su_t24, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(vati_su_t24, observed).
narrative_ontology:measurement(vati_su_t36, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement_basis(vati_su_t36, observed).
narrative_ontology:measurement(vati_su_t48, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 48, 0.71).
narrative_ontology:measurement_basis(vati_su_t48, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__composite_overdetermination_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_authority kernel decomposes into three distinct constraints corresponding to three incompatible readings: (1) continuity_reading assumes Vatican II represents organic theological development within a stable framework of magisterial authority; (2) rupture_reading assumes Vatican II breaks with tradition and contains irreconcilable doctrinal shifts; (3) composite_overdetermination_reading (this one) assumes Vatican II is neither continuous development nor clear rupture but an overdetermined composite of contradictory factional compromises. Each reading has different ε values, different beneficiary/victim structures, and different classifications. The three readings are not observations of the same constraint from different angles — they are structurally distinct constraints whose classification diverges because the ε values are genuinely different (the measurement basis, beneficiary structure, and suppression mechanisms differ across readings). This story influences both sibling readings through the network (each reading must account for the existence of the other two as alternative coherent interpretations of the same documentary set).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
