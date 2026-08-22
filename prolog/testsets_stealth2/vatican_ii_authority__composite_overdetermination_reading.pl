% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Composite Overdetermination Reading of Vatican II Interpretive Authority
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   The conciliar corpus was drafted under factional negotiation:
 *   deliberately ambiguous formulas (the subsistence wording on the Church,
 *   the collegiality-primacy balance, the religious-liberty reversal managed
 *   through development language) secured near-unanimous votes from bishops
 *   who read the same sentences incompatibly. This story instantiates the
 *   composite_overdetermination_reading: the corpus is an overdetermined
 *   composite of doctrinal shifts with incompatible rationales, and the
 *   standing arrangement under contest is the magisterial interpretive regime
 *   that claims univocal authoritative reading of it. Epsilon's referent is
 *   that standing arrangement — the settlement plus its interpretive
 *   administration — assessed by this reading's own lights, never the
 *   pluralist reception this reading would endorse. The structural finding
 *   the delta predicts is honored here: the arrangement's principal taxed
 *   asset is the institution's own univocal claim, while the scholarly guild
 *   collects the complexity-recognition capital the irresolvability
 *   generates. Claim and metrics are independent authored facts: claimed_type
 *   records this reading's structural assessment (genuine coordination
 *   holding the communion together, asymmetric extraction riding the same
 *   structure); the metrics record the arrangement's observed operation. KEY
 *   AGENTS (by structural relationship): magisterial_interpretive_authority:
 *   agenda-setting administrator whose interpretive claim bears the
 *   arrangement's credibility tax (institutional/constrained);
 *   conciliar_scholars: primary beneficiary collecting complexity capital,
 *   episodically censured as individuals (moderate/identity_locked);
 *   parish_clergy_and_laity: diffuse payers of conflict costs with no
 *   adjudication seat (powerless/trapped); traditionalist_communities and
 *   progressive_reform_movements: flank insurgents penalized by enforcement
 *   their own readings provoke (moderate and organized/constrained);
 *   ecumenical_partners: excluded voices who shaped the texts but not their
 *   adjudication (organized/mobile); ecclesiological_historians: analytical
 *   observers documenting the factional structure (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.6).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Composite Overdetermination Reading of Vatican II Interpretive Authority").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, '6a08fa36-f554-4f92-8705-b9409e149eb2').
narrative_ontology:cs_kernel_codification('6a08fa36-f554-4f92-8705-b9409e149eb2', fixed_text).
narrative_ontology:cs_authority_grounding('6a08fa36-f554-4f92-8705-b9409e149eb2', lineage).
narrative_ontology:cs_interpretation_layer_present('6a08fa36-f554-4f92-8705-b9409e149eb2').
narrative_ontology:cs_reading_relation('6a08fa36-f554-4f92-8705-b9409e149eb2', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('6a08fa36-f554-4f92-8705-b9409e149eb2', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_axiom('6a08fa36-f554-4f92-8705-b9409e149eb2', foundational, doctrinal_ambiguity_is_irreducible).
narrative_ontology:cs_axiom_status(doctrinal_ambiguity_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('6a08fa36-f554-4f92-8705-b9409e149eb2', doctrinal_ambiguity_is_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('6a08fa36-f554-4f92-8705-b9409e149eb2', secondary, univocal_interpretive_claim_exceeds_texts).
narrative_ontology:cs_axiom_status(univocal_interpretive_claim_exceeds_texts, holdable).
narrative_ontology:cs_axiom_grounding('6a08fa36-f554-4f92-8705-b9409e149eb2', univocal_interpretive_claim_exceeds_texts, empirically_contingent).
narrative_ontology:cs_reference_frame('6a08fa36-f554-4f92-8705-b9409e149eb2', overdetermined_composite_settlement).
narrative_ontology:cs_drift_state('6a08fa36-f554-4f92-8705-b9409e149eb2', contemporary_hermeneutic_contest, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6a08fa36-f554-4f92-8705-b9409e149eb2', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholars).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, magisterial_interpretive_authority).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, parish_clergy_and_laity).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, progressive_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholars).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, consensus_through_constructive_ambiguity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues official interpretations of the conciliar corpus (the hermeneutic of reform in continuity), disciplines deviating readings through its doctrinal offices, and convenes synods to stabilize reception. Every univocal assertion it publishes draws counter-readings produced by the very scholarship it relies on, and each enforcement cycle costs credibility it cannot replenish. It cannot abandon the interpretive claim without dissolving its own office, and it cannot fully win the claim because the corpus it governs does not support univocity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, magisterial_interpretive_authority, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, magisterial_interpretive_authority, payer).

% Build careers on mapping the composite: draft histories, factional reconstructions, reception studies. The irresolvability of the texts sustains the entire research program, and official interpretations remain dependent on their labor. Individual scholars face investigation or censure when findings serve rupture-leaning conclusions, but the guild collectively is indispensable to any official reading and collects the complexity-recognition capital the ambiguity generates.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholars, beneficiary,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholars, payer).

% Inherit whichever strand of the composite their locality and formation emphasize, then bear the liturgical reversals, catechetical whiplash, and credibility costs of official reinterpretations. They hold no seat in interpretive adjudication and cannot exit the tradition without forfeiting community, sacramental life, and family embeddedness.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, parish_clergy_and_laity, payer,
    powerless, biographical, trapped, global).

% Read the composite as rupture and organize around that verdict, drawing canonical irregularity and exclusion from full communion structures. The same ambiguity that penalizes them continuously vindicates their case, feeding an insurgency they did not create and cannot resolve by winning, since winning would dissolve the object of their protest.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    moderate, generational, constrained, global).

% Press the conciliar impetus beyond its enacted form and draw doctrinal warnings, investigations, and marginalization in return. Their insurgency is fed by the same unresolved strands they invoke; like the traditionalist flank, they pay enforcement costs for a conflict the corpus itself guarantees.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, progressive_reform_movements, payer,
    organized, generational, constrained, global).

% Protestant and Orthodox interlocutors whose dialogue shaped the conciliar texts on religious liberty and Christian unity, but who hold no seat in Catholic interpretive adjudication. They would object that later univocal Roman claims retroactively narrow texts that were negotiated as openings, and they are structurally outside the room where those claims are made.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, ecumenical_partners, excluded,
    organized, generational, mobile, global).

% Reconstruct the drafting history, the voting blocs, and the factional compromises behind each contested formulation from outside confessional commitment. Their documentation is the evidentiary base every party borrows and none controls.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, ecclesiological_historians, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholars).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Held a global episcopate carrying incompatible theologies to near-unanimous adoption of the same sixteen texts, and continues to supply a single doctrinal reference point that keeps the communion from fragmenting along the fault lines the texts encode.
% TRANSFER_FUNCTION: Moves interpretive authority upward: official readings appropriate the scholarly labor that reconstructs what the texts mean, while the credibility costs of asserting univocal meaning over an overdetermined corpus flow back onto the magisterial claim itself, and the practical costs of doctrinal conflict flow down to clergy and laity; complexity-recognition capital flows to the scholarly guild.
% ABSENT_VOICES: Ecumenical partners who shaped the conciliar texts hold no seat in Catholic interpretive adjudication; rank-and-file faithful have no formal voice in doctrinal reception; during the council itself the minority bloc was outvoted and its reservations entered the texts only as the ambiguities now contested.
% DISAPPEARANCE_RATIONALE: Every post-conciliar faction defines itself through or against the settlement: remove the composite corpus and its interpretive regime overnight and traditionalists lose the object of their objection, progressives lose their charter, the curial offices lose their adjudication function, and the liturgical, ecumenical, and collegial arrangements built on the texts require wholesale reconstruction.
% FOUNDING_PROBLEM: How a global hierarchical church confronting modernity — religious pluralism, secular constitutional states, ecumenism, liturgical archaism — could renew its self-presentation without splitting: aggiornamento without schism.
% FOUNDING_PROBLEM_CORROBORATION: Secular and non-Catholic historians of the council working on the international critical-history project, ecumenical partners' reception assessments, and sociologists of religion documenting persistent post-conciliar conflict all attest from outside the beneficiary set that the integration problem remains structurally unresolved. The magisterial offices themselves assert the problem is governed by the hermeneutic of continuity; no party outside the institution attests that the problem is closed.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is substantial (0.60 at interval end) because the arrangement converts an irresolvable corpus into a continuing transfer: scholarly labor is appropriated by official readings, credibility is taxed from the magisterial claim with each enforcement cycle, and conflict costs are pushed onto clergy and laity. Suppression (0.55) is real but bounded — doctrinal offices investigate and censure, yet academic theology retains partial autonomy and rival readings persist, hence accessibility_collapse stays low (0.40): the continuity, rupture, and composite readings all remain live, which is precisely what this reading asserts. Resistance is high (0.65) because every flank resists every other's resolution. Theater rises slowly (0.08 to 0.30) as commemorative rhetoric, anniversary hermeneutics, and formulaic continuity affirmations substitute for actual resolution of the encoded contradictions. The suppression_requirement series is deliberately non-monotonic: enforcement machinery built through the 1970s, peaked in the mid-1980s to mid-1990s (investigations, silencings, mandatory-profession episodes), then decayed under a decentralized style before partially re-ratcheting as synodal processes re-exposed the composite structure. The cycle is driven by pontificate transitions and personnel, not by intermittent reinforcement as an extraction strategy. All three series share one eight-point time grid; the terminal values match the base_properties scalars, which describe the current state.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as administration and taxation simultaneously: it issues the interpretations, enforces them, and absorbs the credibility cost of each enforcement cycle against a corpus that will not support univocity. The scholar seat experiences subsidy with episodic censure — the guild prospers on the irresolvability while individual members who lean rupture-ward pay disciplinary costs. Clergy and laity experience pure cost without a seat. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Conciliar scholars sit near the beneficiary pole: the constraint subsidizes their research program and their indispensability, and identity_locked exit amplifies their attachment to the arrangement that funds them. The magisterial seat is pulled toward the target pole by its victims[] declaration and constrained exit despite holding the agenda-setter role — the derivation reads the structural relationship, not the administrative title, and no override is needed because the victim declaration already places it target-side. Parish clergy and laity sit near the full-target end: trapped exit, diffuse costs, no seat. Traditionalist and progressive insurgents are targets whose enforcement costs are generated by the corpus itself. Ecumenical partners are excluded rather than coordinated — outside the derivation but structurally causative of the texts' openness. The residual ambiguity — whether the magisterial seat's long-run net position is extraction or subsidy — is carried by the extraction_vs_subsidy_boundary omega rather than forced by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (aggiornamento without schism) is contested, not dead: the integration problem persists in transformed form, so this is not a dead-mandate zombie and the mismatch consumer reading (contested status x world_rearranges verdict) correctly fires no capture/zombie flag. The candidate mandatrophy residue is narrower: the specific mandate to interpret the corpus univocally may be obsolescent, since its success conditions are structurally unattainable on this reading — the arrangement persists while that particular promise decays. The tangled_rope classification prevents both mislabels: a pure-rope reading would erase the credibility tax, the censures, and the flank insurgencies; a pure-snare reading would erase the real consensus achievement of 1962-65 and the communion-maintenance function that still anchors a global body. Both functions and both costs are the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the vatican_ii_authority kernel — the composite_overdetermination_reading. What would change structurally if a sibling reading were adopted instead?',
    'Adoption of a sibling reading as the operative frame: the continuity_reading would legitimate the magisterial claim (institution becomes beneficiary, dissenting scholars become payers, epsilon falls toward coordination cost); the rupture_reading would convert the institution into an erring party (victims become the faithful taught error, enforcement becomes compounding injury). The disagreement is located entirely at the resolvability of the conciliar ambiguities.',
    'The entire beneficiary/victim structure inverts under sibling adoption; this story''s epsilon, seats, and classification are valid only within the composite frame and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    resolvability_of_conciliar_ambiguities,
    'Are the conciliar ambiguities genuinely irresolvable, or is a unified rationale recoverable from the drafting history (relations, modi, schema evolution) as the continuity reading requires?',
    'Completion and analysis of the critical editions of the conciliar documents: if the draft trajectory shows a single governing rationale surviving the compromises, the composite thesis weakens; if the final texts encode irreducible factional settlements with incompatible rationales, the thesis is confirmed.',
    'If resolvable, this constraint collapses into the continuity_reading (or rupture_reading), epsilon drops toward coordination cost, and the classification shifts toward rope; if irresolvable, the composite frame holds and the magisterial univocal claim remains structurally overextended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resolvability_of_conciliar_ambiguities, empirical, 'The load-bearing uncertainty of this reading: resolvability of the encoded contradictions.').

omega_variable(
    extraction_vs_subsidy_boundary,
    'Is the magisterial seat''s long-run net position under this arrangement extraction (credibility tax exceeding authority rents) or subsidy (unity rents exceeding the tax)?',
    'Longitudinal comparison of authority-rent indicators (docility of reception, disciplinary effectiveness, ecumenical weight) against credibility-depletion indicators (flank insurgency growth, enforcement blowback, reception failures) across pontificates.',
    'If net extraction, the magisterial seat computes target-side and the arrangement reads as extraction from its own administrator; if net subsidy, the seat computes beneficiary-side and the victims[] weighting overstates its burden — the seat divergence the engine computes flips.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_subsidy_boundary, conceptual, 'Whether the agenda-setting institution is net payer or net collector under the arrangement it runs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (disciplinary machinery, career and canonical risk) or internalized (formation-shaped deference that persists where enforcement is absent)?',
    'Post-relaxation trajectory: compare scholarly and clerical assertiveness in periods and regions where enforcement capacity decayed (post-2015 decentralization) against periods of active enforcement; persistence of self-censorship without enforcement indicates internalized component.',
    'If substantially internalized, effective suppression exceeds the structural measure — the arrangement''s coercive force travels inside its members and survives enforcement decay; if structural, the observed enforcement decay genuinely lowers suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the interpretive regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.08).
narrative_ontology:measurement(vati_tr_t1968, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1968, 0.14).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.19).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.23).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.28).
narrative_ontology:measurement(vati_be_t1968, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.63).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.22).
narrative_ontology:measurement(vati_su_t1968, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1968, 0.38).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.66).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Vatican II' decomposes into three epsilon-distinct constraints per the epsilon-invariance principle: the continuity_reading (upstream, institutionally authoritative, low extraction from its own seat), the rupture_reading (downstream polemical challenge, high claimed error content), and this composite_overdetermination_reading (downstream analytical challenge, asserting the upstream claim's success conditions are unattainable). Each story links the other two through affects_constraints; measuring the corpus through one reading's observables yields a different epsilon than through another's, which is the signal that these are different constraints sharing one kernel, not one constraint viewed from angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
