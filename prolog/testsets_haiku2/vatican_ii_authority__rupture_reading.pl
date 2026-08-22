% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Vatican II Authority Structure: Rupture Reading
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The rupture reading asserts that Vatican II (1962-1965) represents a
 *   substantive break in Catholic doctrine and ecclesial practice from the
 *   pre-conciliar tradition. In this reading, the Council's documents contain
 *   doctrinal errors, irreconcilable contradictions with prior magisterial
 *   teaching, and internal incoherence. The reading locates the beneficiary
 *   as the progressive theological faction that used the Council to
 *   accomplish modernizing reforms previously blocked by ecclesiastical
 *   authority, and the victim as traditional Catholic communities whose
 *   religious identity, liturgical practice, and doctrinal formation were
 *   displaced. The rupture reading is held as a live theological position by
 *   traditionalist communities (Society of Saint Pius X, independent
 *   traditionalist bishops), some conservative papal advisors, and academic
 *   historians documenting genuine theological discontinuity. It coexists
 *   with the continuity reading (Vatican II as organic development) and the
 *   composite reading (Vatican II as overdetermined and ambiguous). This is
 *   ONE of three constraint stories instantiating the vatican_ii_authority
 *   kernel; it is not a complete description of Vatican II's historical role
 *   or theological status, but rather the structural instantiation of one
 *   specific reading's truth-claim. The authoring seat adopts the rupture
 *   reading's epistemic position: the constraint described is how authority
 *   operates when the rupture reading is true.
 *
 * KEY AGENTS:
 *   - Vatican II Magisterium: agenda-setter position; holds institutional authority to declare doctrine; in the rupture frame, shifted doctrine away from prior teaching
 *   - Progressive theological faction: beneficiary; controlled Council proceedings and post-conciliar implementation; benefits from legitimated reform agenda
 *   - Traditional Catholic communities: payer; bear the cost of doctrinal and liturgical discontinuity; identity-locked exit (cannot leave Church unilaterally; cannot reject papal authority)
 *   - SSPX and traditionalist structures: payer and excluded; suppressed via doctrinal condemnation and sacramental illicitness; their reading is structurally locked out
 *   - Vatican Curia: agenda-setter and beneficiary; maintains post-conciliar apparatus; has institutional incentive to defend Council's validity
 *   - Academic theological establishment: beneficiary; gatekeeps new consensus; suppresses pre-conciliar methodology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.72).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.68).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II Authority Structure: Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '06116514-a7c2-45b7-8d11-728cc61bb640').
narrative_ontology:cs_kernel_codification('06116514-a7c2-45b7-8d11-728cc61bb640', formalized).
narrative_ontology:cs_authority_grounding('06116514-a7c2-45b7-8d11-728cc61bb640', extraction).
narrative_ontology:cs_interpretation_layer_present('06116514-a7c2-45b7-8d11-728cc61bb640').
narrative_ontology:cs_reading_relation('06116514-a7c2-45b7-8d11-728cc61bb640', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('06116514-a7c2-45b7-8d11-728cc61bb640', vatican_ii_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('06116514-a7c2-45b7-8d11-728cc61bb640', foundational, vatican_ii_doctrinal_rupture).
narrative_ontology:cs_axiom_status(vatican_ii_doctrinal_rupture, holdable).
narrative_ontology:cs_axiom_grounding('06116514-a7c2-45b7-8d11-728cc61bb640', vatican_ii_doctrinal_rupture, empirically_contingent).
narrative_ontology:cs_axiom('06116514-a7c2-45b7-8d11-728cc61bb640', secondary, pre_conciliar_authority_superseded).
narrative_ontology:cs_axiom_status(pre_conciliar_authority_superseded, holdable).
narrative_ontology:cs_axiom_grounding('06116514-a7c2-45b7-8d11-728cc61bb640', pre_conciliar_authority_superseded, deontological).
narrative_ontology:cs_reference_frame('06116514-a7c2-45b7-8d11-728cc61bb640', pre_conciliar_doctrinal_stability).
narrative_ontology:cs_drift_state('06116514-a7c2-45b7-8d11-728cc61bb640', contemporary_post_vatican_ii_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('06116514-a7c2-45b7-8d11-728cc61bb640', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, progressive_theological_faction).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_communities).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, pre_conciliar_doctrinal_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, ecumenical_partners).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, vatican_curia).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, academic_theological_establishment).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, sspx_traditionalist_resistance).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, doctrinal_discontinuity_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, hermeneutics_of_rupture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Council itself and the post-conciliar papal magisterium that interprets and implements its documents. In the rupture reading, this institutional seat shifted doctrine away from established teaching and now enforces the new interpretation as authoritative, suppressing dissent from traditional positions. The magisterium carries the institutional authority to declare what the Church teaches, making its reading of Vatican II the operative authority structure regardless of internal contradiction.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, vatican_ii_magisterium, agenda_setter,
    institutional, civilizational, trapped, universal).

% Theologians, bishops, and institutional reformers who advocated pre-Council change and who read Vatican II as vindication of their modernizing project. They benefit from the Council's apparent authorization of liturgical, catechetical, and disciplinary reform. From the rupture reading's standpoint, they used the Council to accomplish what pre-conciliar authority had resisted, and now maintain post-conciliar reforms by controlling interpretation and institutional machinery.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, progressive_theological_faction, beneficiary,
    organized, biographical, mobile, continental).

% Catholics whose religious identity, liturgical practice, and theological formation are rooted in pre-conciliar tradition. In the rupture reading, they bear the cost of doctrinal discontinuity: the Mass they knew is suppressed, catechesis they received is declared deficient or erroneous, and devotional and disciplinary practices they understood as expressions of faith are dismantled. Their exit is constrained by identity fusion—leaving the Church is not a live option; neither is unilateral rejection of papal authority. They are the involuntary subjects of the shift.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_communities, payer,
    moderate, generational, identity_locked, global).

% The corpus of pre-Vatican II magisterial teaching, scholastic theology, and ecumenical councils. Not a real-world actor, but a structural authority whose standing is undermined by the rupture reading's assertion that Vatican II contradicts or supersedes it. In the rupture frame, the authority of prior teaching is effectively negated—not by formal repudiation (which would be transparent) but by reinterpretation and institutional enforcement of incompatible positions as 'development.'
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, pre_conciliar_doctrinal_authority, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, pre_conciliar_doctrinal_authority, observer).
narrative_ontology:stakeholder_non_agent(vatican_ii_authority__rupture_reading, pre_conciliar_doctrinal_authority).

% The Society of Saint Pius X and allied traditionalist structures that reject Vatican II's authority on the grounds that it ruptures doctrine and contains errors. They are excluded from official magisterial conversation—their objections are treated as schismatic dissent rather than as valid theological critique. Their suppression is structural: they are formally condemned and their sacraments declared illicit, which is the enforcement mechanism that prevents their reading from gaining institutional traction.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_traditionalist_resistance, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, sspx_traditionalist_resistance, excluded).

% Non-Catholic Christian denominations and other world religions who benefit from Vatican II's opening toward dialogue and reformed teachings on religious freedom and non-Christian faiths. They benefit from the post-conciliar Church's self-presentation as reformed and dialogical. From the rupture reading, they are second-order beneficiaries of the doctrinal shift: they gain legitimacy and dialogue partners because the Church has broken from positions that opposed them.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, ecumenical_partners, beneficiary,
    organized, biographical, mobile, continental).

% The bureaucratic apparatus that must implement and enforce post-conciliar teaching. They have institutional incentive to present the Council as valid and authoritative (their own power derives from it) and to suppress voices that challenge its legitimacy. In the rupture reading, they are complicit in the maintenance of doctrinal contradiction by managing interpretive apparatus and controlling access to dissenting voices.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, vatican_curia, agenda_setter,
    institutional, generational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, vatican_curia, beneficiary).

% University theology departments and seminaries that shifted from scholastic to modern theological frameworks post-Council. They benefit from legitimated reinterpretation of tradition and new research agendas. In the rupture reading, they are gatekeepers of the new theological consensus, controlling what counts as orthodox interpretation and suppressing pre-conciliar theological method.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, academic_theological_establishment, beneficiary,
    organized, biographical, mobile, continental).

% The formal Council event (1962-1965) and its authoritative documents. Not a real actor, but the textual and institutional nucleus of the controversy. The rupture reading treats the Council's documents as internally contradictory or containing doctrinal error, making the Council itself a carrier of structural defect rather than a legitimate source of authority.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, vatican_ii_council_itself, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_authority__rupture_reading, vatican_ii_council_itself).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vatican II convened the universal Church to deliberate and pronounce on doctrine and discipline. The stated coordination problem: how can the Church remain authoritative and credible in a modern, pluralistic, secularizing world? The coordination function: provide legitimated authority to modernize practice while maintaining that the change is continuous with tradition.
% TRANSFER_FUNCTION: From traditional Catholics, pre-conciliar theology, and liturgical continuity TO the progressive theological faction and the post-conciliar magisterium: the transfer is the loss of liturgical and doctrinal authority in exchange for institutional legitimacy gained by the progressives.
% ABSENT_VOICES: Pre-conciliar theologians and bishops (largely absent by the Council's opening); traditional laity and lower clergy whose faith was formed in the pre-conciliar Church; academic theologians working in scholastic or pre-modern frameworks (marginalized during and after the Council); the doctrinal authority of ecumenical councils and papal teaching prior to Vatican II (treated as superseded rather than consulted). The structures that would argue against the Council's authority—traditionalist episcopal governance, pre-conciliar seminary networks, the lived experience of stable doctrine—were either abolished or marginalized during the Council's implementation.
% DISAPPEARANCE_RATIONALE: If Vatican II's institutional authority and post-conciliar enforcement apparatus vanished, the Church would face immediate structural reorganization. Traditionalist communities would regain legitimacy for pre-conciliar liturgy and doctrine; the Mass they knew would return to licit status; clerical celibacy would be reconsidered; ecumenical openness would reset; seminary formation would revert to scholastic theology. The theological consensus established post-Council would lose its mandatory character. Thousands of Catholics who left or joined traditionalist structures in response to the Council's changes might return to an institution that restored what they understood as stable tradition.
% FOUNDING_PROBLEM: The Church faced a crisis of cultural authority and credibility in a post-Christian, pluralistic West. The founding problem was: how can an institution claiming universal unchanging truth maintain legitimacy in a culture that rejects institutional authority on metaphysical questions and demands dialogue with modernity?
% FOUNDING_PROBLEM_CORROBORATION: In the rupture reading, the founding problem was real but the Council's solution created a worse crisis. The progressives attest the founding problem was live and the Council's reforms addressed it. Traditionalist witnesses (Lefebvre, SSPX structures, conservative Catholic historians and theologians) attest the founding problem motivated the Council but the solution backfired: instead of making the Church credible to the modern world, it fractured the Church from its tradition, creating internal incoherence and loss of Catholic identity. Academic historians (Klaus Schatz, John O'Malley, Roberto de Mattei) document genuine theological discontinuity and Council ambiguity. John Paul II and Benedict XVI's repeated efforts to reframe Vatican II as continuous with tradition (not addressed by the Council itself, but added post-hoc by papal authority) constitute witness from the institutional beneficiary seat that the continuity reading required external authority to succeed—the Council's own words did not suffice.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.38 at the Council (when the shift was still open, alternative readings alive) to 0.72 by year 65 (when post-conciliar institutional apparatus had consolidated the new teaching as mandatory). Suppression follows a similar arc: initial resistance from traditionalist bishops was met with disciplinary action; by year 65, SSPX is formally schismatic and traditionalist communities are pushed to the ecclesiastical margins. Theater rises from 0.22 to 0.41: the Council's documents invoke tradition and continuity rhetoric, but the substantive content contradicts prior teaching, requiring continuous hermeneutical performance ('development,' 'legitimate evolution,' 'reading in light of tradition') to maintain the appearance of coherence. The accessibility collapse measures how completely alternatives disappear: at the individual level (traditional communities), alternatives collapse most completely (0.72) because they are identity-locked—even after 65 years, they cannot access counter-teaching except through marginalized, schismatic channels. At the organizational level of resistance (SSPX, traditionalist episcopal structures), alternatives persist but are structurally locked out (capacity for resistance remains at 0.52 by year 65, but the cost of pursuing it is excommunication). The coercion grid models this asymmetry: structural-level collapse is lower (0.62) because academic and institutional alternatives can be articulated in print and academic circles; individual-level collapse is highest (0.72) because the faithful in pews cannot act on theoretical objections without breaking institutional identity. Suppression at the organizational level (0.72 by year 65) exceeds structural level (0.58) because the enforcement mechanism specifically targets organized resistance—SSPX is the enforcement target; academic debate is tolerated. Resistance follows inverse pattern: organizational resistance persists (0.52) because SSPX, while marginalized, remains active; individual resistance collapses (0.55) because isolated traditionalists face maximal suppression with no organizational vehicle. One shared time grid ensures every metric is authored at every examined point (three metrics x seven time points = 21 total measurements, each metric on the same grid).
 *
 * PERSPECTIVAL GAP:
 *   The Vatican II magisterium and progressive faction compute this constraint very differently from the traditional Catholic seat. From the institutional beneficiary seat, the Council represents genuine coordination—a legitimate pastoral response to modernity, carrying real authority to reform outdated practices. From the traditional victim seat, the same institutional apparatus enforces doctrinal contradiction while suppressing dissent. The engine computes per-seat classification from the structural data: the beneficiary seat (progressive faction, institutional apparatus) experiences low directionality (benefits, controls exits, has arbitrage mobility—d near 0.0), while the victim seat (traditional communities, identity-locked) experiences high directionality (bears costs, trapped exit—d near 1.0). The beneficiary-seat computation should yield lower effective extraction; the victim-seat computation should yield higher. The rupture reading's structural asymmetry is encoded in the divergent directionality: the constraint operates as genuine coordination for those positioned to benefit, and as extractive enforcement for those positioned to pay.
 *
 * DIRECTIONALITY LOGIC:
 *   The rupture reading fixes directionality through the beneficiary/victim declaration: progressive theological faction benefits (low d), traditional Catholic communities bear costs (high d), SSPX and traditionalist structures are payers and excluded (high d with suppressed voice). The progressive faction has mobile exit options and organized power—they could dissent and still function within Catholicism or outside it; their benefit from the Council is optional leverage, not survival. Traditional communities have identity-locked exit: their exit means either rejecting papal authority (apostasy from their self-concept) or joining schismatic structures (which requires abandoning their understanding of ecclesial legitimacy). The identity lock amplifies the directionality toward the target end. The Vatican Curia and theological establishment occupy the institutional seat: they have trapped exit (they cannot unilaterally reject the Council without destroying their own authority) but are beneficiaries (the Council legitimates their reforms). No override needed here—the structural derivation is correct. The institutional seats are partially trapped but substantially benefit, placing them near symmetric (d ~0.4–0.5), which matches their actual position as both enforcers and beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading models a constraint where the founding problem (how to make an ancient institution credible in a modern world) was real but the Council's solution created a new problem: internal doctrinal contradiction that requires continuous hermeneutical maintenance and suppression of alternative readings. The founding_problem_status is 'contested' because the rupture reading and the continuity reading disagree about whether the founding problem still motivates the constraint or whether it has been superseded by a new problem (the crisis created by the Council itself). The disappearance_verdict is 'world_rearranges' because the Council's authority structure is not a natural fact—if it vanished, the Church would reorganize around a different theological consensus. The Tangled Rope classification prevents mislabeling this as pure extraction: there IS a genuine coordination function at the Council's foundation (convening the Church, responding to modernity), and there ARE real beneficiaries of the reforms (ecumenical partners, modernizing theology). But the asymmetric extraction (traditional communities bear the cost of discontinuity without choosing it) and active enforcement (suppression of SSPX, exclusion of traditionalist voices) make it more than pure coordination. The 'development' hermeneutic is the critical performance: it claims continuity while enacting rupture, which is the classic Tangled Rope signature—the coordination cover story permits the extraction to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_continuity_vs_substantive_shift,
    'Is Vatican II''s relationship to prior doctrine one of continuity (legitimate organic development from unchanging deposit) or substantive rupture (contradiction or supersession of prior teaching)?',
    'Rigorous systematic comparison of pre-conciliar magisterial teaching on specific doctrines (religious freedom, nature of priesthood, salvation of non-Christians) with Vatican II and post-conciliar documents, conducted by historians and theologians outside the immediate benefiting parties. The test is whether the Council''s teaching can be logically derived from prior teaching or whether it contradicts or supersedes it.',
    'If rupture is established, the Council loses the status of legitimate universal magisterium and becomes a defective teaching event requiring correction. This would validate the rupture reading and undermine the institutional authority that enforces post-conciliar orthodoxy. If continuity is established, the rupture reading fails and the constraint collapses toward Rope or even benign coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_continuity_vs_substantive_shift, empirical, 'The core factual dispute: does Vatican II continue or break from tradition?').

omega_variable(
    institutional_suppression_necessity,
    'Is the suppression of traditionalist voices (SSPX condemnation, exclusion from official magisterial conversation) structurally necessary to maintain the Council''s authority, or could the Council withstand open doctrinal debate?',
    'Counterfactual: if traditionalist voices were given institutional platform and hermeneutical equality (allowed into academic councils, permitted to publish in Vatican-sponsored theology journals, represented in curial offices), would post-conciliar doctrine maintain coherence or would internal contradictions become undeniable?',
    'If suppression is necessary (the constraint collapses if dissent is permitted), the high suppression_requirement and theater_ratio are structurally justified—the extraction is extracted by enforcing a defective teaching. If suppression is contingent (doctrine could withstand open debate), then the suppression is purely extractive power play, shifting the constraint toward pure Snare and raising the moral cost of the institution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_suppression_necessity, conceptual, 'Whether suppression maintains an otherwise-incoherent structure or represents extractive power').

omega_variable(
    hermeneutical_vs_textual_doctrine,
    'Are the Council''s documents internally contradictory (textual rupture), or is the contradiction an artifact of interpretation (the hermeneutic split between Lefebvre and Vatican)?',
    'Close reading of primary conciliar texts with explicit attention to logical contradiction: does the Council document assert P and not-P on the same point, or do the apparent contradictions dissolve when one reads carefully? Determine whether academic consensus can be reached on textual meaning independent of hermeneutical allegiance.',
    'If textual contradiction is real, the Council is structurally defective as a teaching event. If contradiction is hermeneutical (the Council permits multiple readings, but one reading is enforced as mandatory), then the extraction is via interpretive authority rather than doctrinal error. This distinction affects whether the rupture is a fact about doctrine or a fact about power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutical_vs_textual_doctrine, empirical, 'Is the rupture in the Council''s texts or in the interpretation thereof?').

omega_variable(
    identity_lock_mechanism_suppression_internalized,
    'For traditional Catholic communities, is the suppression of their reading structurally imposed (they cannot access counter-teaching without breaking identity) or internalized (they have absorbed the framework that makes the Council''s authority seem mandatory)?',
    'Observational: do traditionalist communities who exit the post-conciliar Church (joining SSPX or other traditionalist structures) show evidence that suppression was structural (they move toward the traditionalist reading once the institutional barrier is removed) or internalized (they experience guilt, internal conflict, or attraction back toward Rome even after choosing alternatives)? Post-exit trajectory and reported subjective experience distinguish structural from internalized suppression.',
    'If suppression is structural, removing institutional barriers (permitting traditionalist liturgy, validating traditionalist theology, restoring access to pre-conciliar seminary) would likely bring many back to institutional Church. If suppression is internalized, structural barriers'' removal would not suffice—the identity work of coming back into the institution would be difficult. This affects the cost of fixing the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression_internalized, empirical, 'Structural vs. internalized suppression mechanism in identity-locked communities').

omega_variable(
    kernel_reading_committer_status,
    'Is the rupture reading a coherent theological possibility within Catholic intellectual tradition, or is it logically foreclosed by the nature of ecumenical councils (a foreclosed reading should not be held as a live option)?',
    'Genealogical: trace the rupture reading''s intellectual ancestry (where does it come from?), its internal coherence as a theological system (can one consistently hold it?), and whether any major theological or magisterial figure endorsed it in substantive form before its modern traditionalist instantiation. Determine whether the reading is a recovery of a suppressed voice or a novel construction.',
    'If the reading is genealogically shallow or theologically unstable, its suppression by the institution becomes more defensible as management of doctrinal confusion. If the reading has deep roots and internal coherence, its suppression appears more clearly as power and less as legitimate authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_status, conceptual, 'Genealogy and coherence of the rupture reading itself as a theological possibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__rupture_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(vati_tr_t5, vatican_ii_authority__rupture_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__rupture_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__rupture_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(vati_tr_t35, vatican_ii_authority__rupture_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__rupture_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(vati_tr_t65, vatican_ii_authority__rupture_reading, theater_ratio, 65, 0.41).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__rupture_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vati_be_t5, vatican_ii_authority__rupture_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__rupture_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__rupture_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(vati_be_t35, vatican_ii_authority__rupture_reading, base_extractiveness, 35, 0.7).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__rupture_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(vati_be_t65, vatican_ii_authority__rupture_reading, base_extractiveness, 65, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__rupture_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vati_su_t5, vatican_ii_authority__rupture_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__rupture_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__rupture_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(vati_su_t35, vatican_ii_authority__rupture_reading, suppression_requirement, 35, 0.65).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__rupture_reading, suppression_requirement, 50, 0.67).
narrative_ontology:measurement(vati_su_t65, vatican_ii_authority__rupture_reading, suppression_requirement, 65, 0.68).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=65
narrative_ontology:measurement(vati_grid_01, vatican_ii_authority__rupture_reading, accessibility_collapse(class), 0, 0.52).
narrative_ontology:measurement(vati_grid_02, vatican_ii_authority__rupture_reading, accessibility_collapse(class), 65, 0.71).
narrative_ontology:measurement(vati_grid_03, vatican_ii_authority__rupture_reading, accessibility_collapse(individual), 0, 0.58).
narrative_ontology:measurement(vati_grid_04, vatican_ii_authority__rupture_reading, accessibility_collapse(individual), 65, 0.72).
narrative_ontology:measurement(vati_grid_05, vatican_ii_authority__rupture_reading, accessibility_collapse(organizational), 0, 0.38).
narrative_ontology:measurement(vati_grid_06, vatican_ii_authority__rupture_reading, accessibility_collapse(organizational), 65, 0.58).
narrative_ontology:measurement(vati_grid_07, vatican_ii_authority__rupture_reading, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(vati_grid_08, vatican_ii_authority__rupture_reading, accessibility_collapse(structural), 65, 0.62).
narrative_ontology:measurement(vati_grid_09, vatican_ii_authority__rupture_reading, resistance(class), 0, 0.75).
narrative_ontology:measurement(vati_grid_10, vatican_ii_authority__rupture_reading, resistance(class), 65, 0.61).
narrative_ontology:measurement(vati_grid_11, vatican_ii_authority__rupture_reading, resistance(individual), 0, 0.71).
narrative_ontology:measurement(vati_grid_12, vatican_ii_authority__rupture_reading, resistance(individual), 65, 0.55).
narrative_ontology:measurement(vati_grid_13, vatican_ii_authority__rupture_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(vati_grid_14, vatican_ii_authority__rupture_reading, resistance(organizational), 65, 0.52).
narrative_ontology:measurement(vati_grid_15, vatican_ii_authority__rupture_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(vati_grid_16, vatican_ii_authority__rupture_reading, resistance(structural), 65, 0.48).
narrative_ontology:measurement(vati_grid_17, vatican_ii_authority__rupture_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(vati_grid_18, vatican_ii_authority__rupture_reading, stakes_inflation(class), 65, 0.75).
narrative_ontology:measurement(vati_grid_19, vatican_ii_authority__rupture_reading, stakes_inflation(individual), 0, 0.62).
narrative_ontology:measurement(vati_grid_20, vatican_ii_authority__rupture_reading, stakes_inflation(individual), 65, 0.81).
narrative_ontology:measurement(vati_grid_21, vatican_ii_authority__rupture_reading, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement(vati_grid_22, vatican_ii_authority__rupture_reading, stakes_inflation(organizational), 65, 0.61).
narrative_ontology:measurement(vati_grid_23, vatican_ii_authority__rupture_reading, stakes_inflation(structural), 0, 0.42).
narrative_ontology:measurement(vati_grid_24, vatican_ii_authority__rupture_reading, stakes_inflation(structural), 65, 0.68).
narrative_ontology:measurement(vati_grid_25, vatican_ii_authority__rupture_reading, suppression(class), 0, 0.41).
narrative_ontology:measurement(vati_grid_26, vatican_ii_authority__rupture_reading, suppression(class), 65, 0.71).
narrative_ontology:measurement(vati_grid_27, vatican_ii_authority__rupture_reading, suppression(individual), 0, 0.45).
narrative_ontology:measurement(vati_grid_28, vatican_ii_authority__rupture_reading, suppression(individual), 65, 0.68).
narrative_ontology:measurement(vati_grid_29, vatican_ii_authority__rupture_reading, suppression(organizational), 0, 0.28).
narrative_ontology:measurement(vati_grid_30, vatican_ii_authority__rupture_reading, suppression(organizational), 65, 0.72).
narrative_ontology:measurement(vati_grid_31, vatican_ii_authority__rupture_reading, suppression(structural), 0, 0.32).
narrative_ontology:measurement(vati_grid_32, vatican_ii_authority__rupture_reading, suppression(structural), 65, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__rupture_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_authority kernel is instantiated as three structurally distinct constraint stories, each representing a reading that claims truth about what Vatican II is and means. The rupture_reading asserts Vatican II contains doctrinal errors and breaks from prior teaching. The continuity_reading asserts Vatican II is organic doctrinal development. The composite_overdetermination_reading asserts Vatican II is ambiguous and indeterminate. Each reading produces a different epsilon (extractiveness), different beneficiary/victim structure, and different classification. The readings are linked via network.affects_constraints to show they are competing interpretations of a single contested kernel, not independent constraints. Decomposition follows OQ-26 (ε-invariance principle): if measuring Vatican II under the rupture frame yields high ε (council enforces doctrinal rupture, suppresses traditionalist voices) but measuring it under the continuity frame yields low ε (council coordinates doctrinal development, benefits the faithful), then one is looking at two different constraints—one per reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
