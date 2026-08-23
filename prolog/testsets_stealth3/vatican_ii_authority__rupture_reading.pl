% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Post-Conciliar Binding-Authority Enforcement Structure (Rupture Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   CONSTRAINT FAMILY NOTE: the colloquial label 'Vatican II authority'
 *   decomposes, per the epsilon-invariance principle, into three structurally
 *   distinct constraint stories - this file instantiates the RUPTURE READING
 *   only (kernel vatican_ii_authority, reading rupture_reading). The standing
 *   arrangement under contest is the post-conciliar binding-authority
 *   enforcement structure: the machinery by which the conciliar documents and
 *   their liturgical/doctrinal implementation are treated as authoritative
 *   and non-negotiable, with canonical discipline against refusal. Epsilon's
 *   referent is that standing arrangement assessed by the rupture reading's
 *   own lights - never the restored pre-conciliar order this reading
 *   endorses. The sibling files (vatican_ii_authority__continuity_reading,
 *   vatican_ii_authority__composite_overdetermination_reading) assess the
 *   SAME referent with reading-indexed values and are linked via
 *   network.affects_constraints; their epsilon diverges from this file's by
 *   design (OQ-26), and that divergence is the measurement the family exists
 *   to take. The claim/metric pairing here is internally consistent from this
 *   seat: the reading holds the settlement carries a genuine coordination
 *   function (the Church's sacramental and governance machinery persists and
 *   functions) wrapped around asymmetric extraction (assent to teachings this
 *   reading holds erroneous, enforced by canonical penalty) - hence
 *   tangled_rope, not snare; the sedevacantist variant, which denies the
 *   apparatus is the Church at all, would author this as a snare or worse,
 *   and that sharper verdict lives in the excluded seat, not in this file.
 *
 * KEY AGENTS:
 *   - post_conciliar_hierarchical_establishment: agenda-setter and principal beneficiary (institutional/identity_locked) - administers the enforcement apparatus and collects its assent yield
 *   - modernist_theological_faction: beneficiary (organized/mobile) - doctrinal program moved from censure to normative status
 *   - non_catholic_ecumenical_partners: secondary beneficiary (moderate/mobile) - collect recognition without bearing internal discipline
 *   - sspx_clergy: primary target (organized/identity_locked) - bear canonical penalties for refusing assent; cannot conform without identity death
 *   - traditional_laity: diffuse target (powerless/identity_locked) - bear displacement of liturgical and catechetical life
 *   - traditional_liturgical_institutes: dual-positioned (moderate/constrained) - collect canonical legitimacy and pay exposure to policy reversal
 *   - sedevacantist_bishops_and_clergy: excluded (moderate/trapped) - radical objection with no seat in the conversation
 *   - comparative_ecclesiology_scholars: analytical observer (moderate/analytical) - sees the full structure without canonical stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.74).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Post-Conciliar Binding-Authority Enforcement Structure (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '4a17bb3a-748e-478a-b10a-b8f39077be74').
narrative_ontology:cs_kernel_codification('4a17bb3a-748e-478a-b10a-b8f39077be74', fixed_text).
narrative_ontology:cs_authority_grounding('4a17bb3a-748e-478a-b10a-b8f39077be74', extraction).
narrative_ontology:cs_interpretation_layer_present('4a17bb3a-748e-478a-b10a-b8f39077be74').
narrative_ontology:cs_reading_relation('4a17bb3a-748e-478a-b10a-b8f39077be74', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('4a17bb3a-748e-478a-b10a-b8f39077be74', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('4a17bb3a-748e-478a-b10a-b8f39077be74', foundational, prior_magisterium_is_infallible_norm).
narrative_ontology:cs_axiom_status(prior_magisterium_is_infallible_norm, holdable).
narrative_ontology:cs_axiom_grounding('4a17bb3a-748e-478a-b10a-b8f39077be74', prior_magisterium_is_infallible_norm, theological).
narrative_ontology:cs_axiom('4a17bb3a-748e-478a-b10a-b8f39077be74', secondary, juridical_legitimacy_presupposes_doctrinal_integrity).
narrative_ontology:cs_axiom_status(juridical_legitimacy_presupposes_doctrinal_integrity, holdable).
narrative_ontology:cs_axiom_grounding('4a17bb3a-748e-478a-b10a-b8f39077be74', juridical_legitimacy_presupposes_doctrinal_integrity, conventional).
narrative_ontology:cs_reference_frame('4a17bb3a-748e-478a-b10a-b8f39077be74', pre_conciliar_doctrinal_settlement).
narrative_ontology:cs_drift_state('4a17bb3a-748e-478a-b10a-b8f39077be74', contemporary_post_traditionis_custodes, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('4a17bb3a-748e-478a-b10a-b8f39077be74', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, post_conciliar_hierarchical_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_theological_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, non_catholic_ecumenical_partners).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, sspx_clergy).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_laity).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_liturgical_institutes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, traditional_liturgical_institutes).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, hermeneutic_of_reform_in_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, dignitatis_humanae_religious_liberty).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, collegiality_ecclesiology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates and interprets the conciliar corpus through pope, curia, and doctrinal congregation; disciplines deviation through canonical penalties, withdrawn faculties, and liturgical restrictions; collects the assent, liturgical uniformity, and institutional obedience the settlement produces. Its authority claim now rests on the validity of what it enforces, so repudiating the council would dissolve its own standing - it cannot walk away from its own settlement.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, post_conciliar_hierarchical_establishment, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, post_conciliar_hierarchical_establishment, beneficiary).

% Theologians whose interdicted pre-conciliar positions - nouvelle theologie, religious liberty, ecumenical engagement, historical-critical method - moved from censure to curial consultancy, university chairs, and editorial control after the council. They collect doctrinal vindication and institutional influence without administering discipline; their mobility across academies and chanceries is unrestricted.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_theological_faction, beneficiary,
    organized, generational, mobile, global).

% Protestant federations and Orthodox churches engaged by the post-conciliar ecumenical turn. They receive recognized dialogue status, agreed statements, and reciprocal legitimacy that the pre-conciliar exclusivist posture denied them; nothing binds them to the settlement's internal discipline.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, non_catholic_ecumenical_partners, beneficiary,
    moderate, generational, mobile, global).

% Priests and bishops who refuse assent to the disputed documents. They carry the memory of declared excommunication (1988-2009), permanent irregular canonical status, exclusion from ordinary faculties, and insecure title to churches and seminaries. Conforming would require surrendering the doctrinal integrity that constitutes their vocation; the exits open to them - sedevacantism, Orthodoxy, secular life - each destroy the specific priestly identity they hold.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_clergy, payer,
    organized, generational, identity_locked, global).

% Faithful attached to the pre-conciliar liturgy and catechesis. They have lost parish continuity, migrated between licensed chapels and unlicensed ones, funded their own buildings, and raised children amid shifting permissions. Their attachment is constitutive of their religious identity; conforming to the revised forms would sever it, and leaving the Church ends it altogether.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_laity, payer,
    powerless, biographical, identity_locked, global).

% Canonically erected communities celebrating the pre-conciliar forms under pontifical right. They received legitimacy and vocations under the indult and Summorum Pontificum regimes - a real gain - while remaining bound to affirm the conciliar documents and exposed when the 2021 restrictions revoked the broad permission they depended on. Their canonical standing and patrimony rise and fall with the center's policy.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_liturgical_institutes, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, traditional_liturgical_institutes, beneficiary).

% Clergy holding that the post-conciliar See is vacant or its occupants illegitimate. They stand wholly outside the negotiation the other parties conduct; their position excludes them from every table, and their own rigor bars return. They would object to the framing of any settlement short of repudiation if admitted to the conversation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sedevacantist_bishops_and_clergy, excluded,
    moderate, generational, trapped, global).

% Academic students of religious authority across traditions. They publish on the conciliar hermeneutics dispute, interview every seat, and hold no canonical stake; their careers advance on analysis, not on either verdict prevailing.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, comparative_ecclesiology_scholars, observer,
    moderate, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__rupture_reading, post_conciliar_hierarchical_establishment).
narrative_ontology:fixing_cost_class(vatican_ii_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single worldwide doctrinal-liturgical settlement: one magisterial voice, common revised rites, unified catechesis, a disciplined clerical corps, and a stable institutional counterpart for ecumenical and civic engagement - solved centrally rather than per-diocese or per-order.
% TRANSFER_FUNCTION: Moves doctrinal assent and liturgical allegiance from the pre-conciliar corpus toward the conciliar corpus; moves canonical legitimacy (faculties, jurisdictions, buildings) away from non-assenting clergy toward compliant ones; moves institutional recognition and dialogue standing to non-Catholic partners; concentrates interpretive authority in the Roman center.
% ABSENT_VOICES: Sedevacantist clergy - the most radical objectors, who deny the apparatus's authority wholesale - sit entirely outside; the council's minority fathers survive only as archival citations, not as live interpretive voices; the pre-conciliar encyclical corpus is quoted polemically by resisters but holds no seat at any negotiating table.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen the liturgy along pre-conciliar lines, void the canonical penalties that sustain compliance, dissolve the ecumenical architecture premised on the new posture, and force every institute and diocese to renegotiate its relationship to Rome - the entire post-conciliar settlement is load-bearing.
% FOUNDING_PROBLEM: The mid-twentieth-century pastoral crisis: Mass attendance collapsing in industrialized societies, liturgy experienced as remote, confessional states dissolving into religiously neutral polities, separated Christians organizing transnationally, and a curial governance model strained by a newly global church.
% FOUNDING_PROBLEM_CORROBORATION: Secular historiography of twentieth-century Christianity - outside all benefiting parties - attests the founding problems were real mid-century phenomena documented across denominations. Whether they remain live is disputed: the magisterial establishment attests live (ongoing evangelization need); traditionalist scholarship attests the problems were misdiagnosed and the remedies produced worse ones. No seat outside the contest adjudicates the status.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.74: from this reading's lights the settlement demands assent to doctrinal error - extraction of the precise good (doctrinal integrity) the governed hold sacred - and the rate is set by enforcement capacity, not by consent. Suppression 0.78 is authored as a RAW structural property, unscaled by power or scope (only extractiveness is scaled downstream): declared excommunications, irregular status, faculty withdrawal, and the 2021 liturgical restrictions are the enforcement substrate. Theater 0.50: roughly half the settlement-adjacent activity visible from this seat is rhetorical management ('pastoral accommodations', the hermeneutic-of-reform-in-continuity framing) layered over functional discipline; the series shows theater thickening whenever enforcement tightens under conciliatory language. Accessibility_collapse 0.62, below mountain grade: alternatives persist (separation paths, approved institutes, sedevacantism) but each destroys the specific identity at stake, so understood alternatives collapse sharply without vanishing. Resistance 0.70: four decades of organized refusal, seminaries, and publications. Coordination type identity_coordination: the dominant function is boundary maintenance of a redefined Catholic membership - whose failure would most directly cause the coordination problem the settlement exists to solve. FNL alert honored: identity framing ('this is simply who the Church now is') is precisely the cover story this reading accuses the settlement of using, so the conservative 0.08 floor stands and no floor override is authored. Measurements run on ONE shared eight-point grid (t0=1962 council opening; t7=1969 missal imposition; t15=1977 implementation decade; t26=1988 Eccone consecrations and excommunications; t34=mid-1990s indult consolidation; t45=2007 Summorum Pontificum; t53=early-2010s tightening signals; t60=2022 Traditionis Custodes regime). The enforcement series is an OSCILLATOR, not a periodic cycle: ratchet (Paul VI/John Paul II) - relaxation (Benedict XVI) - re-ratchet (Francis), driven by papal turns rather than intermittent reinforcement; the oscillation is documented here rather than treated as noise, and the base_properties scalars reflect the END-state (t=60) of each series.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the establishment seat the settlement is self-governance: the institution administering its own legitimately promulgated law, with the identity-lock reading as fidelity rather than entrapment. From the payer seats the identical structure operates as enforced assent to error, with the same identity-lock reading as captivity - the lock that protects the establishment's standing is the wall around the resister's exit. The excluded seat declines the frame entirely: for sedevacantists there is no valid enforcement to classify, only usurpation. Across FILES, the continuity reading authors low epsilon over the same referent and likely computes rope; the composite reading suspends classification. The engine takes this divergence from the structural data; nothing in this file reconciles it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: the establishment (declared beneficiary, collects assent and legitimacy) and the modernist faction (declared beneficiary, mobile, collected vindication without paying discipline). Ecumenical partners collect recognition while bound to nothing - near-zero d. Payers map to the high-d end: SSPX clergy and traditional laity are identity-locked targets (identity-locked targets sit nearer full-target than mobile ones), and the liturgical institutes sit intermediate (payer with genuine secondary benefit, constrained exit). OVERRIDE AUTHORED: {power_atom: institutional, d_value: 0.05}. The automatic derivation reads identity_locked as pushing toward the target end - correct for victims, wrong here: the establishment's lock is ENFORCER-SIDE (it cannot repudiate the settlement its legitimacy rests on), which pins it at the beneficiary end rather than moving it toward target. The derivation chain cannot distinguish victim-side from enforcer-side identity fusion, so the override corrects it. The sedevacantist seat derives no structural d (excluded, absent from both arrays) and falls to fallback - fitting, since their objection denies the frame in which d is defined; noted rather than forced.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim does real work against both neighboring errors. Labeling the settlement rope (the continuity reading's practical effect) would book doctrinal enforcement as development cost and erase the asymmetry this reading exists to register. Labeling it snare would erase the genuine coordination that persists - sacramental unity, worldwide governance, a functioning clergy - which even this reading concedes operates daily. Piton is excluded structurally: the agenda-setter profits visibly and maintains the apparatus actively, so neither the no-concentrated-beneficiary condition nor the cost-asymmetry condition holds; fixing_cost is prohibitive precisely because the only seat that could fix it is the seat whose legitimacy the fixing would destroy, and gain_flow names that seat - the captured-cell combination, consistent with the claim. On the R5 mismatch consumer: founding_problem_status=contested crossed with disappearance_verdict=world_rearranges raises no dead-mandate/zombie flag - the founding problem is disputed, not dead, and mandatrophy is deliberately NOT declared. The temporal series supports this: extraction and enforcement ended the interval HIGHER than any earlier point, the signature of a live mandate being worked harder, not an atrophied one being performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the rupture reading of the vatican_ii_authority kernel; the continuity and composite-overdetermination readings are separate constraints. How would instantiating those readings change the structural classification of the same standing arrangement?',
    'Compile and compare the sibling stories; the engine''s foreclosure computation from cs_axiom_contradiction across readings determines which premises can be jointly held and which readings logically displace which.',
    'Under the continuity reading the same arrangement authors low epsilon and plausibly computes rope (organic-development coordination); under the composite reading the classification stays suspended between verdicts. Only this file''s figures carry the rupture-seat valuation; cross-file comparison is the intended consumption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Kernel membership: one of three readings of vatican_ii_authority; sibling readings are separate files, not internal alternatives.').

omega_variable(
    textual_irreconcilability_status,
    'Are the alleged contradictions between the conciliar documents and prior magisterial teaching textually demonstrable, or do the continuity reading''s distinctions (pastoral versus doctrinal register, hierarchical gradation of assent) reconcile each disputed locus?',
    'Locus-by-locus doctrinal analysis of religious liberty, ecumenism, collegiality, and liturgical law - the 2009-2011 Roman-SSPX doctrinal discussions generated partial transcripts, and independent scholastic analysis could adjudicate each locus on the texts alone.',
    'If the loci reconcile, this reading''s epsilon collapses toward the continuity valuation and the constraint recomputes nearer rope; if they stand as contradictions, the extraction attribution hardens and the snare gradient rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_irreconcilability_status, empirical, 'Whether the rupture premise survives textual confrontation with the continuity distinctions.').

omega_variable(
    modernist_faction_coherence,
    'Is the ''modernist faction'' a coherent coordinated beneficiary, or a retrospective construction imposed on heterogeneous theologians who disagreed deeply with one another?',
    'Network analysis of pre-conciliar censure records, private correspondence, and post-conciliar appointment patterns: did the benefited actors act as a coordinated bloc, or converge opportunistically on a settlement they did not design?',
    'If constructed, the beneficiary declaration over-attributes agency; gain_flow consolidates on the institutional establishment alone and the extraction story becomes structural rather than factional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_faction_coherence, empirical, 'Coherence of the attributed beneficiary bloc behind the settlement.').

omega_variable(
    enforcement_dependence,
    'Does the settlement persist by conviction (voluntary coordination around the conciliar paradigm by most clergy and laity) or by enforcement (canonical penalty and liturgical restriction applied to a resisting minority)?',
    'Natural experiment across the Summorum Pontificum window (2007-2021), when enforcement slackened: track compliance, identity retention, and institutional behavior in jurisdictions where permission widened versus where it never did.',
    'If conviction-dominant, suppression is over-measured and the arrangement is more rope-like than authored; if enforcement-dominant, lifting enforcement triggers rapid rearrangement and the snare gradient rises above the tangled_rope band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependence, empirical, 'Conviction versus coercion as the operative persistence mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__rupture_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t7, vatican_ii_authority__rupture_reading, theater_ratio, 7, 0.18).
narrative_ontology:measurement_basis(vati_tr_t7, observed).
narrative_ontology:measurement(vati_tr_t15, vatican_ii_authority__rupture_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(vati_tr_t15, observed).
narrative_ontology:measurement(vati_tr_t26, vatican_ii_authority__rupture_reading, theater_ratio, 26, 0.4).
narrative_ontology:measurement_basis(vati_tr_t26, observed).
narrative_ontology:measurement(vati_tr_t34, vatican_ii_authority__rupture_reading, theater_ratio, 34, 0.44).
narrative_ontology:measurement_basis(vati_tr_t34, observed).
narrative_ontology:measurement(vati_tr_t45, vatican_ii_authority__rupture_reading, theater_ratio, 45, 0.36).
narrative_ontology:measurement_basis(vati_tr_t45, observed).
narrative_ontology:measurement(vati_tr_t53, vatican_ii_authority__rupture_reading, theater_ratio, 53, 0.43).
narrative_ontology:measurement_basis(vati_tr_t53, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__rupture_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__rupture_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t7, vatican_ii_authority__rupture_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement_basis(vati_be_t7, observed).
narrative_ontology:measurement(vati_be_t15, vatican_ii_authority__rupture_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(vati_be_t15, observed).
narrative_ontology:measurement(vati_be_t26, vatican_ii_authority__rupture_reading, base_extractiveness, 26, 0.66).
narrative_ontology:measurement_basis(vati_be_t26, observed).
narrative_ontology:measurement(vati_be_t34, vatican_ii_authority__rupture_reading, base_extractiveness, 34, 0.64).
narrative_ontology:measurement_basis(vati_be_t34, observed).
narrative_ontology:measurement(vati_be_t45, vatican_ii_authority__rupture_reading, base_extractiveness, 45, 0.56).
narrative_ontology:measurement_basis(vati_be_t45, observed).
narrative_ontology:measurement(vati_be_t53, vatican_ii_authority__rupture_reading, base_extractiveness, 53, 0.63).
narrative_ontology:measurement_basis(vati_be_t53, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__rupture_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__rupture_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t7, vatican_ii_authority__rupture_reading, suppression_requirement, 7, 0.35).
narrative_ontology:measurement_basis(vati_su_t7, observed).
narrative_ontology:measurement(vati_su_t15, vatican_ii_authority__rupture_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(vati_su_t15, observed).
narrative_ontology:measurement(vati_su_t26, vatican_ii_authority__rupture_reading, suppression_requirement, 26, 0.76).
narrative_ontology:measurement_basis(vati_su_t26, observed).
narrative_ontology:measurement(vati_su_t34, vatican_ii_authority__rupture_reading, suppression_requirement, 34, 0.68).
narrative_ontology:measurement_basis(vati_su_t34, observed).
narrative_ontology:measurement(vati_su_t45, vatican_ii_authority__rupture_reading, suppression_requirement, 45, 0.46).
narrative_ontology:measurement_basis(vati_su_t45, observed).
narrative_ontology:measurement(vati_su_t53, vatican_ii_authority__rupture_reading, suppression_requirement, 53, 0.54).
narrative_ontology:measurement_basis(vati_su_t53, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__rupture_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel vatican_ii_authority. The single colloquial label 'Vatican II authority' covers three structurally distinct claims: rupture (this file - substantive break, doctrinal error, high epsilon), continuity (organic development, low epsilon), and composite overdetermination (multiple incompatible doctrinal shifts, classification suspended). All three assess the SAME standing referent - the post-conciliar binding-authority enforcement structure - with reading-indexed epsilon values (OQ-26); they differ in epsilon because the readings differ, not because the referent does. Upstream/downstream: the continuity reading supplies the establishment's official self-description and therefore shapes the enforcement environment this file models (its hermeneutic is what the theater_ratio partly measures); the composite reading pressures both verdict-readings by denying decidability. Each file links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__rupture_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
